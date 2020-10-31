
package dnn.memory

import chisel3._
import chisel3.util._
import config._
import dnn.memory.ISA._
import dnnnode.StoreQueue
import interfaces.CooDataBundle
import shell._


/** outDMA_act
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class outStreamDMA_cooNewIO(memTensorType: String = "none")(implicit val p: Parameters)
  extends Module {
  val tp = new TensorParams(memTensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val done = Output(Bool())
    val baddr_row = Input(UInt(mp.addrBits.W))
    val baddr_col = Input(UInt(mp.addrBits.W))
    val baddr_val = Input(UInt(mp.addrBits.W))
    val vme_wr_row = new VMEWriteMaster
    val vme_wr_col = new VMEWriteMaster
    val vme_wr_val = new VMEWriteMaster
    val in = Flipped(Decoupled(new CooDataBundle(UInt(p(XLEN).W))))
    val last = Input(Bool())
    val outLen = Output(UInt(mp.addrBits.W))
  })
}

class outStreamDMA_cooNew(bufSize: Int, memTensorType: String = "none")(implicit p: Parameters)
  extends outStreamDMA_cooNewIO(memTensorType)(p) {
  require(bufSize > tp.tensorWidth, "buffer size should be greater than the tensorFile width")

  val streamStore_row = Module(new StreamStore(bufSize =  5000, memTensorType = "out"))
  val streamStore_col = Module(new StreamStore(bufSize =  5000, memTensorType = "out"))
  val streamStore_val = Module(new StreamStore(bufSize =  5000, memTensorType = "out"))

  streamStore_row.io.in.bits := io.in.bits.row
  streamStore_row.io.in.valid := io.in.valid
  streamStore_row.io.last := io.last

  streamStore_col.io.in.bits := io.in.bits.col
  streamStore_col.io.in.valid := io.in.valid
  streamStore_col.io.last := io.last

  streamStore_val.io.in.bits := io.in.bits.data
  streamStore_val.io.in.valid := io.in.valid
  streamStore_val.io.last := io.last

  io.in.ready := streamStore_row.io.in.ready && streamStore_col.io.in.ready && streamStore_val.io.in.ready

  io.outLen := streamStore_row.io.outLen

//  val pushCnt = Counter(math.pow(2, p(XLEN)).toInt)

  io.vme_wr_row <> streamStore_row.io.vme_wr
  io.vme_wr_col <> streamStore_col.io.vme_wr
  io.vme_wr_val <> streamStore_val.io.vme_wr

  streamStore_row.io.baddr := io.baddr_row
  streamStore_col.io.baddr := io.baddr_col
  streamStore_val.io.baddr := io.baddr_val

  streamStore_row.io.start := false.B
  streamStore_col.io.start := false.B
  streamStore_val.io.start := false.B

  val doneR = for (i <- 0 until 3) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }

  when (streamStore_row.io.done) {doneR(0) := true.B}
  when (streamStore_col.io.done) {doneR(1) := true.B}
  when (streamStore_val.io.done) {doneR(2) := true.B}

  io.done := false.B

  val sIdle :: sBusy :: Nil = Enum(2)
  val state = RegInit(sIdle)

  switch(state){
    is(sIdle){
      when(io.in.valid){
        streamStore_row.io.start := true.B
        streamStore_col.io.start := true.B
        streamStore_val.io.start := true.B
        state := sBusy
      }
    }
    is(sBusy){
      when(doneR.reduceLeft(_ && _)){
        io.done := true.B
        doneR.foreach(a => a := false.B)
        state := sIdle
      }
    }
  }
}
