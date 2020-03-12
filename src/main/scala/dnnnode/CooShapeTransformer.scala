
package dnnnode

import chisel3._
import chisel3.util._
import config._
import dnn.memory.{TensorMaster, TensorParams}
import interfaces.{CooDataBundle}
import node.Shapes
import shell._


/** Coordinate Shape Transformer.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class CooShapeTransformerIO[gen <: Shapes](memTensorType: String = "none")(outShape: => gen)(implicit val p: Parameters)
  extends Module {
  val tp = new TensorParams(memTensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val len = Input(UInt(mp.addrBits.W))
    val indTensor = new TensorMaster(memTensorType)
    val valTensor = new TensorMaster(memTensorType)
    val out = Vec(outShape.getLength(), Decoupled(new CooDataBundle(UInt(p(XLEN).W))))
  })
}

class CooShapeTransformer[L <: Shapes](bufSize: Int, memTensorType: String = "none")
                                      (outShape: => L)(implicit p: Parameters)
  extends CooShapeTransformerIO(memTensorType)(outShape)(p) {

  val elemNum = io.len / outShape.getLength().U
  val memTensorRows = Mux(io.len % tp.tensorWidth.U === 0.U,
    io.len / tp.tensorWidth.U,
    (io.len / tp.tensorWidth.U) + 1.U)

  val pushCnt = Counter(tp.memDepth)
  val popCnt = Counter(tp.memDepth)

  val sIdle :: sRead :: sClear :: Nil = Enum(3)
  val state = RegInit(sIdle)


  val queue = Module(new MIMOQueue(new CooDataBundle(UInt(p(XLEN).W)), bufSize, tp.tensorWidth, outShape.getLength()))

  val validReg = RegInit(false.B)

  val dataIn = Wire(Vec(tp.tensorWidth, new CooDataBundle(UInt(p(XLEN).W))))
  for (i <- 0 until tp.tensorWidth) {
    dataIn(i).row := io.indTensor.rd.data.bits(0)(i)
    dataIn(i).data := io.valTensor.rd.data.bits(0)(i)
    dataIn(i).col := 0.U
    dataIn(i).valid := true.B
  }

  io.done := false.B
  queue.io.clear := false.B
  queue.io.enq.bits := dataIn
  queue.io.enq.valid := queue.io.enq.ready && validReg === sRead//io.tensor(i).rd.data.valid
  io.indTensor.rd.idx.valid := queue.io.enq.ready && state === sRead
  io.indTensor.rd.idx.bits := pushCnt.value
  io.indTensor.wr <> DontCare

  io.valTensor.rd.idx.valid := queue.io.enq.ready && state === sRead
  io.valTensor.rd.idx.bits := pushCnt.value
  io.valTensor.wr <> DontCare

  for (i <- 0 until outShape.getLength()) {
    io.out(i).bits <> queue.io.deq.bits(i)
    io.out(i).valid := queue.io.deq.valid
    queue.io.deq.ready := io.out.map(_.ready).reduceLeft(_&&_)
  }

  when(queue.io.enq.ready && state  === sRead) {pushCnt.inc()}
  when(queue.io.deq.fire()) {popCnt.inc()}

  switch(state){
    is(sIdle){
      when(io.start){
        state := sRead
      }
    }
    is(sRead){
      validReg := true.B
      when(pushCnt.value === memTensorRows && queue.io.enq.ready){
        validReg := false.B
        pushCnt.value := 0.U
        state := sClear
      }
    }
    is(sClear){
      when(popCnt.value === elemNum - 1.U){
        popCnt.value := 0.U
        queue.io.clear := true.B
        io.done := true.B
        state := sIdle
      }
    }
  }

}
