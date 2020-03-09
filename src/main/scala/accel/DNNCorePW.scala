package accel

import FPU.FType
import chisel3.util._
import chisel3.{when, _}
import config._
import dnn_layers.{PDP_Block, PW_Block}
import node.{FPvecN, matNxN, vecN}
import shell._

/** DNNCore.
  *
  * The DNNcore defines the current DNN accelerator by connecting the M-Bricks, L-Bricks and I-Bricks together.
  * By changing the parameters and batch size, the bricks will be configured automatically.
  */
class DNNCorePW(implicit val p: Parameters) extends Module {
  val Hx = 5  //  Number of Rows
  val Wx = 7
  val Fx = 2
  val Px = 10
  val K = 3
  val Cx = 6
  val Cb = 2
  // C = Cx * Cb
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  // nRd = Hx + 3 = 6
  // nWr = Px * (Hx - K + 1) = 10 * 1 = 10
  val cycle_count = new Counter(2000)

//  val NumChannel = 3

  val S = new FType(8, 24)

//  val memShape = new vecN(16, 0, false)
  val memShape = new FPvecN(16, S, 0)

  val macDWShape = new matNxN(K, false)
  val macPW2Shape = new vecN(Fx, 0, false)

  val wgtDWShape = new vecN(K * K, 0, false)
  val wgtPW2Shape = new vecN(Fx, 0, false)

//  val CxShape = new vecN(Cx, 0, false)
  val CxShape = new FPvecN(Cx, S, 0)

//  val DW_B1 = Module(new DW_Block(3, "wgt", "inp")(memShape)(wgtDWShape)(macDWShape))

//  val conv = Module(new DW_PW_Block(NumChannel, MACperCH, NumPWFilter, "wgt", "wgtPW", "inp")
//                   (memShape)(wgtDWShape)(wgtPWShape)(macDWShape)(macPWShape))

  val conv = Module(new PW_Block(Hx, Fx, Cb, "intWgtPW1", "inp")(memShape)(CxShape))
//  val conv = Module(new PDP_Block(Hx, K, Fx, 19, Px,
//                    "intWgtPW1", "intWgtDW", "intWgtPW2", "inp")
//                    (memShape)(CxShape)
//                    (wgtDWShape)(macDWShape)
//                    (wgtPW2Shape)(macPW2Shape))

  /* ================================================================== *
     *                      Basic Block signals                         *
     * ================================================================== */
  conv.io.wgtIndex := 0.U

  conv.io.rowWidth := Wx.U //3.U

  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */

  io.vcr.ecnt(0).bits := cycle_count.value

  io.vcr.ecnt(1).bits := conv.io.inDMA_act_time
  io.vcr.ecnt(2).bits := conv.io.inDMA_wgt_time
  io.vcr.ecnt(3).bits := conv.io.mac_time

  /* ================================================================== *
    *                    VME Reads and writes                           *
    * ================================================================== */

  for (i <- 0 until Hx) {
    io.vme.rd(i) <> conv.io.vme_rd(i)
  }
  io.vme.rd(Hx) <> conv.io.vme_wgt_rd

  for (i <- 0 until Fx * Hx) {
    io.vme.wr(i) <> conv.io.vme_wr(i)
  }


  conv.io.start := false.B

  conv.io.inBaseAddr := io.vcr.ptrs(0)

  conv.io.wgt_baddr := io.vcr.ptrs(1)

  conv.io.outBaseAddr := io.vcr.ptrs(2)

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)

  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        conv.io.start := true.B
        state := sExec
      }
    }
    is(sExec) {
      when(conv.io.done) {
        state := sIdle
      }
    }
  }

  val last = state === sExec && conv.io.done
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last
  io.vcr.ecnt(1).valid := last
  io.vcr.ecnt(2).valid := last
  io.vcr.ecnt(3).valid := last

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
