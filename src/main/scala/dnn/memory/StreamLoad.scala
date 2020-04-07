
package dnn.memory

import chisel3._
import chisel3.util._
import config._
import dnnnode.MIMOQueue
import interfaces.CooDataBundle
import shell._
//import vta.util.config._
import dnn.memory.ISA._


/** TensorLoad.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class StreamLoad(tensorType: String = "none", debug: Boolean = false)(
  implicit p: Parameters)
  extends Module {
  val tp = new TensorParams(tensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val inst = Input(UInt(INST_BITS.W))
    val baddr = Input(UInt(mp.addrBits.W))
    val vme_rd = new VMEReadMaster
//    val tensor = new TensorClient(tensorType)
    val out = Decoupled(UInt(p(XLEN).W))
  })
  val sizeFactor = tp.tensorLength * tp.numMemBlock
  val strideFactor = tp.tensorLength * tp.tensorWidth

  val dec = io.inst.asTypeOf(new MemDecode)
  val dataCtrl = Module(
    new TensorDataCtrl(tensorType, sizeFactor, strideFactor))
  val dataCtrlDone = RegInit(false.B)

  val tag = Reg(UInt(log2Ceil(tp.numMemBlock).W))
  val set = Reg(UInt(log2Ceil(tp.tensorLength).W))

  val queue = Module(new MIMOQueue(UInt(p(XLEN).W), entries = 100, tp.tensorWidth, NumOuts = 1))

  val sIdle :: sReadCmd :: sReadData :: Nil =
    Enum(7)
  val state = RegInit(sIdle)

  // control
  switch(state) {
    is(sIdle) {
      when(io.start) {
        state := sReadCmd
      }
    }
    is(sReadCmd) {
      when(io.vme_rd.cmd.ready) {
        state := sReadData
      }
    }
    is(sReadData) {
      when(io.vme_rd.data.valid) {
        when(dataCtrl.io.done) {
          state := sIdle
        }.elsewhen(dataCtrl.io.stride || dataCtrl.io.split) {
          state := sReadCmd
        }
      }
    }
  }

  // data controller
  dataCtrl.io.start := state === sIdle & io.start
  dataCtrl.io.inst := io.inst
  dataCtrl.io.baddr := io.baddr
  dataCtrl.io.xinit := io.vme_rd.cmd.fire()
  dataCtrl.io.xupdate := io.vme_rd.data.fire()
  dataCtrl.io.yupdate := io.vme_rd.data.fire()

  when(state === sIdle) {
    dataCtrlDone := false.B
  }.elsewhen(io.vme_rd.data.fire() && dataCtrl.io.done) {
    dataCtrlDone := true.B
  }


  // read-from-dram
  io.vme_rd.cmd.valid := state === sReadCmd
  io.vme_rd.cmd.bits.addr := dataCtrl.io.addr
  io.vme_rd.cmd.bits.len := dataCtrl.io.len

//  io.vme_rd.data.ready := state === sReadData
  io.vme_rd.data.ready := queue.io.enq.ready && state === sReadData

  // write-to-sram
  when(state === sIdle || state === sReadCmd || tag === (tp.numMemBlock - 1).U) {
    tag := 0.U
  }.elsewhen(io.vme_rd.data.fire()) {
    tag := tag + 1.U
  }

  when(
    state === sIdle || dataCtrlDone || (set === (tp.tensorLength - 1).U && tag === (tp.numMemBlock - 1).U)) {
    set := 0.U
  }.elsewhen(
    io.vme_rd.data.fire() && tag === (tp.numMemBlock - 1).U) {
    set := set + 1.U
  }

  val waddr_cur = Reg(UInt(tp.memAddrBits.W))
  val waddr_nxt = Reg(UInt(tp.memAddrBits.W))
  when(state === sIdle) {
    waddr_cur := dec.sram_offset
    waddr_nxt := dec.sram_offset
  }.elsewhen((io.vme_rd.data.fire()) && set === (tp.tensorLength - 1).U && tag === (tp.numMemBlock - 1).U) {
    waddr_cur := waddr_cur + 1.U
  }
    .elsewhen(dataCtrl.io.stride) {
      waddr_cur := waddr_nxt + dec.xsize
      waddr_nxt := waddr_nxt + dec.xsize
    }

//  val tensorFile = Seq.fill(tp.tensorLength) {
//    SyncReadMem(tp.memDepth, Vec(tp.numMemBlock, UInt(tp.memBlockBits.W)))
//  }

  queue.io.enq.bits.asUInt() := io.vme_rd.data.bits
  queue.io.enq.valid := io.vme_rd.data.valid

  io.out.bits := queue.io.deq.bits.asUInt()
  io.out.valid := queue.io.deq.valid
  queue.io.deq.ready := io.out.ready

//  val wmask = Seq.fill(tp.tensorLength) { Wire(Vec(tp.numMemBlock, Bool())) }
//  val wdata = Seq.fill(tp.tensorLength) {
//    Wire(Vec(tp.numMemBlock, UInt(tp.memBlockBits.W)))
//  }
//  val no_mask = Wire(Vec(tp.numMemBlock, Bool()))
//  no_mask.foreach { m =>
//    m := true.B
//  }

//  for (i <- 0 until tp.tensorLength) {
//    for (j <- 0 until tp.numMemBlock) {
//      wmask(i)(j) := tag === j.U
//      wdata(i)(j) := io.vme_rd.data.bits
//    }
//    val tdata = io.tensor.wr.bits.data(i).asUInt.asTypeOf(wdata(i))
//    val muxWen =
//      Mux(state === sIdle,
//        io.tensor.wr.valid,
//        (io.vme_rd.data.fire()) & set === i.U)
//    val muxWaddr = Mux(state === sIdle, io.tensor.wr.bits.idx, waddr_cur)
//    val muxWdata = Mux(state === sIdle, tdata, wdata(i))
//    val muxWmask = Mux(state === sIdle, no_mask, wmask(i))
////    when(muxWen) {
////      tensorFile(i).write(muxWaddr, muxWdata, muxWmask)
////    }
//  }

  // read-from-sram
//  val rvalid = RegNext(io.tensor.rd.idx.valid)
//  io.tensor.rd.data.valid := rvalid

//  val rdata =
//    tensorFile.map(_.read(io.tensor.rd.idx.bits, io.tensor.rd.idx.valid))
//  rdata.zipWithIndex.foreach {
//    case (r, i) =>
//      io.tensor.rd.data.bits(i) := r.asUInt.asTypeOf(io.tensor.rd.data.bits(i))
//  }

  // done
  val done_no_pad = io.vme_rd.data.fire() & dataCtrl.io.done & dec.xpad_1 === 0.U & dec.ypad_1 === 0.U
  io.done := done_no_pad

  // debug
  if (debug) {
    if (tensorType == "inp") {
      when(io.vme_rd.cmd.fire()) {
        printf("[TensorLoad] [inp] cmd addr:%x len:%x\n",
          dataCtrl.io.addr,
          dataCtrl.io.len)
      }
    } else if (tensorType == "wgt") {
      when(io.vme_rd.cmd.fire()) {
        printf("[TensorLoad] [wgt] cmd addr:%x len:%x\n",
          dataCtrl.io.addr,
          dataCtrl.io.len)
      }
    } else if (tensorType == "acc") {
      when(io.vme_rd.cmd.fire()) {
        printf("[TensorLoad] [acc] cmd addr:%x len:%x\n",
          dataCtrl.io.addr,
          dataCtrl.io.len)
      }
    }
  }
}
