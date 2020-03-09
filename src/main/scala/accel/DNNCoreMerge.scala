package accel

import FPU.FType
import chisel3.util._
import chisel3.{when, _}
import config._
import dnn_layers.PW_Block
import node.{FPvecN, matNxN, vecN}
import shell._
import tensorKernels.MVM_Block

/** DNNCore.
  *
  * The DNNcore defines the current DNN accelerator by connecting the M-Bricks, L-Bricks and I-Bricks together.
  * By changing the parameters and batch size, the bricks will be configured automatically.
  */
class DNNCoreMerge(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  val cycle_count = new Counter(2000)



  val S = new FType(8, 24)
  val shape = new FPvecN(1, S, 0)

  val merge = Module(new MVM_Block(NumRows = 2, "intWgtPW1", "inp")(shape))

  /* ================================================================== *
     *                      Basic Block signals                         *
     * ================================================================== */
  merge.io.len := 32.U //3.U

  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */

  io.vcr.ecnt(0).bits := cycle_count.value

  io.vcr.ecnt(1).bits := merge.io.inDMA_time
  io.vcr.ecnt(2).bits := merge.io.merge_time
  io.vcr.ecnt(3).bits := merge.io.merge_time

  /* ================================================================== *
    *                    VME Reads and writes                           *
    * ================================================================== */

  for (i <- 0 until 2) {
    io.vme.rd(i) <> merge.io.vme_rd(i)
  }

//  for (i <- 0 until 2) {
//    io.vme.wr(i) <> merge.io.vme_wr(i)
//  }


  io.vme.wr(0) <> merge.io.vme_wr

  merge.io.start := false.B

  merge.io.in1_BaseAddr := io.vcr.ptrs(0)
  merge.io.in2_BaseAddr := io.vcr.ptrs(1)
  merge.io.outBaseAddr := io.vcr.ptrs(2)

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)

  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        merge.io.start := true.B
        state := sExec
      }
    }
    is(sExec) {
      when(merge.io.done) {
        state := sIdle
      }
    }
  }

  val last = state === sExec && merge.io.done
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last
  io.vcr.ecnt(1).valid := last
  io.vcr.ecnt(2).valid := last
  io.vcr.ecnt(3).valid := last

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
