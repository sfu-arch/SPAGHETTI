
package dnnnode

import chisel3._
import chisel3.util._
import config._
import dnn.memory.{TensorMaster, TensorParams}
import interfaces.CustomDataBundle
import node.Shapes
import shell._
//import vta.util.config._


/** TensorLoad.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class DiffShapeTransformerIO(NumRows: Int, memTensorType: String = "none")(implicit val p: Parameters)
  extends Module {
  val tp = new TensorParams(memTensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val len = Input(UInt(mp.addrBits.W))
    val tensor = Vec(NumRows, new TensorMaster(memTensorType))
    val deq = Vec(NumRows, Decoupled(UInt(p(XLEN).W)))
  })
}

class DiffShapeTransformer(NumRows: Int, bufSize: Int, memTensorType: String = "none")(implicit p: Parameters)
  extends DiffShapeTransformerIO(NumRows, memTensorType)(p) {

  val elemNum = io.len
  val memTensorRows = Mux(io.len % tp.tensorWidth.U === 0.U,
    io.len / tp.tensorWidth.U,
    (io.len / tp.tensorWidth.U) + 1.U)

  val readTensorCnt = Counter(tp.memDepth)
  val popCnt = Counter(tp.memDepth)

  val sIdle :: sRead :: sClear :: Nil = Enum(3)
  val state = RegInit(sIdle)


  val queue = for (i <- 0 until NumRows) yield {
    val buffer = Module(new DiffQueue(UInt(p(XLEN).W), bufSize, tp.tensorWidth))
    buffer
  }

  val validReg = RegInit(false.B)

  for (i <- 0 until NumRows) {
    queue(i).io.clear := false.B
    queue(i).io.enq.bits := io.tensor(i).rd.data.bits.asTypeOf(queue(i).io.enq.bits)
    queue(i).io.enq.valid := queue(i).io.enq.ready && validReg === sRead//io.tensor(i).rd.data.valid
    io.tensor(i).rd.idx.valid := queue(i).io.enq.ready && state === sRead
    io.tensor(i).rd.idx.bits := readTensorCnt.value
    io.tensor(i).wr <> DontCare

    io.deq(i).bits := queue(i).io.deq.bits.asUInt()
    io.deq(i).valid := queue(i).io.deq.valid
    queue(i).io.deq.ready := io.deq(i).ready
  }

  io.done := false.B

  when(queue.map(_.io.enq.ready).reduceLeft(_&&_) && state  === sRead) {readTensorCnt.inc()}

  when(queue.map(_.io.deq.fire()).reduceLeft(_&&_)) {popCnt.inc()}


  switch(state){
    is(sIdle){
      when(io.start){
        state := sRead
      }
    }
    is(sRead){
      validReg := true.B
      when(readTensorCnt.value === memTensorRows && queue.map(_.io.enq.ready).reduceLeft(_&&_)){
        validReg := false.B
        readTensorCnt.value := 0.U
        state := sClear
      }
    }
    is(sClear){
      when(popCnt.value === elemNum - 1.U){
        popCnt.value := 0.U
        queue.foreach(_.io.clear := true.B)
        io.done := true.B
        state := sIdle
      }
    }
  }


}
