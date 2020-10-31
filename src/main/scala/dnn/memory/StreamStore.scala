
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
class StreamStore_IO(memTensorType: String = "none")(implicit val p: Parameters)
  extends Module {
  val tp = new TensorParams(memTensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val last = Input(Bool())
    val done = Output(Bool())
    val baddr = Input(UInt(mp.addrBits.W))
    val vme_wr = new VMEWriteMaster
    val in = Flipped(Decoupled(UInt(p(XLEN).W)))
    val outLen = Output(UInt(mp.addrBits.W))
  })
}

class StreamStore(bufSize: Int, memTensorType: String = "none")(implicit p: Parameters)
  extends StreamStore_IO(memTensorType)(p) {
  require(bufSize > math.pow(2, mp.lenBits).toInt * tp.tensorWidth, "buffer size should be greater than the tensorFile width")

  val storeQueue = Module(new StoreQueue(UInt(p(XLEN).W), bufSize, tp.tensorWidth))

//  val popCnt = Counter(math.pow(2, p(XLEN)).toInt)
  val pushCnt = Counter(math.pow(2, p(XLEN)).toInt)

//  val sendCnt = Counter(math.pow(2, p(XLEN)).toInt)

  val last = RegInit(false.B)

  val addr = Reg(chiselTypeOf(io.vme_wr.cmd.bits.addr))
  val len = Reg(chiselTypeOf(io.vme_wr.cmd.bits.len))

  when(io.last){
    last := true.B
  }

  val sIdle :: sLoading :: sSendReq :: sBusy :: sLastReq :: sLastSending :: Nil = Enum(6)
  val state = RegInit(sIdle)


  when(storeQueue.io.enq.fire()){
    pushCnt.inc()
  }

  storeQueue.io.last := io.last

  io.vme_wr.cmd.bits.addr := addr
  io.vme_wr.cmd.bits.len := len
  io.vme_wr.cmd.valid := false.B

  io.done := false.B
  io.outLen := pushCnt.value

  switch(state){
    is(sIdle){
      when(io.start){
        addr := io.baddr
        state := sLoading
      }
    }
    is(sLoading){
      when(storeQueue.io.count >= (math.pow(2, mp.lenBits).toInt * tp.tensorWidth).asUInt()){
        len := math.pow(2, mp.lenBits).toInt.asUInt() - 1.U
        state := sSendReq
      }.elsewhen(last){
        len := Mux (storeQueue.io.count % tp.tensorWidth.U === 0.U, storeQueue.io.count/tp.tensorWidth.U - 1.U, storeQueue.io.count/tp.tensorWidth.U)
        state := sLastReq
      }
    }
    is(sSendReq){
      when(io.vme_wr.cmd.fire()){
        state := sBusy
      }
    }
    is(sBusy){
      when(io.vme_wr.ack){
        state := sLoading
      }
    }
    is(sLastReq){
      when(io.vme_wr.cmd.fire()){
        state := sLastSending
      }
    }
    is(sLastSending){
      when(io.vme_wr.ack){
        io.done := true.B
        last := false.B
        state := sIdle
      }
    }
  }

  when(state === sSendReq || state === sLastReq){
    io.vme_wr.cmd.valid := true.B
  }

  storeQueue.io.last := io.last
  storeQueue.io.enq <> io.in
  io.in.ready := storeQueue.io.enq.ready && !last

  io.vme_wr.data.bits := storeQueue.io.deq.bits.asUInt()
  io.vme_wr.data.valid := storeQueue.io.deq.valid
  storeQueue.io.deq.ready := io.vme_wr.data.ready

}
