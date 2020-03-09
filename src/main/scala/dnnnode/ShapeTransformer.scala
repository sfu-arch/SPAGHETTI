package dnnnode

import Chisel.Enum
import chisel3.util._
import chisel3.{Flipped, Module, UInt, _}
import config.{Parameters, XLEN}
import dnn.types.{OperatorDot, OperatorReduction}
import muxes._
import interfaces.CustomDataBundle
import node.{HandShakingIONPS, HandShakingNPS, Shapes, matNxN, vecN}

class ShapeTransformerIO[gen <: vecN, gen2 <: Shapes](NumIns: Int, NumOuts: Int)(shapeIn: => gen)(shapeOut: => gen2)(implicit p: Parameters)
  extends HandShakingIONPS(NumOuts)(new CustomDataBundle(UInt(shapeOut.getWidth.W))) {
  val in = Vec(NumIns, Flipped(Decoupled(new CustomDataBundle(UInt(shapeIn.getWidth.W)))))

  override def cloneType = new ShapeTransformerIO(NumIns, NumOuts)(shapeIn)(shapeOut).asInstanceOf[this.type]
}

class ShapeTransformer[L <: vecN, K <: Shapes](NumIns: Int, NumOuts: Int, ID: Int)(shapeIn: => L)(shapeOut: => K)(implicit p: Parameters)
  extends HandShakingNPS(NumOuts, ID)(new CustomDataBundle(UInt(shapeOut.getWidth.W)))(p) {
  override lazy val io = IO(new ShapeTransformerIO(NumIns, NumOuts)(shapeIn)(shapeOut))


  /*===========================================*
   *            Registers                      *
   *===========================================*/
  val dataIn_R = RegInit(VecInit(Seq.fill(NumIns)(CustomDataBundle.default(0.U(shapeIn.getWidth.W)))))
  val dataIn_valid_R = RegInit(VecInit(Seq.fill(NumIns)(false.B)))
  val dataIn_Wire = Wire(Vec(shapeIn.N, Vec (NumIns, UInt(xlen.W))))

  val input_data = dataIn_R.map(_.data.asUInt())


  for (i <- 0 until shapeIn.N) {
    for (j <- 0 until NumIns) {
      val index = ((i + 1) * xlen) - 1
      dataIn_Wire(i)(j) := input_data(j)(index, i * xlen)
    }
  }


  val buffer = Module(new CustomQueue(new CustomDataBundle(UInt((NumIns * xlen).W)), 40, NumIns))

  val mux = Module(new Mux(new CustomDataBundle(UInt((NumIns * xlen).W)), shapeIn.N))
  val countOn = RegInit(init = false.B)
  val cnt = Counter(shapeIn.N)

  mux.io.sel := cnt.value
  mux.io.en := true.B
  for (i <- 0 until shapeIn.N) {
    mux.io.inputs(i).data := dataIn_Wire(i).asTypeOf(CustomDataBundle(UInt((NumIns * xlen).W))).data
    mux.io.inputs(i).valid := dataIn_R.map(_.valid).reduceLeft(_ && _)
    mux.io.inputs(i).predicate := dataIn_R.map(_.valid).reduceLeft(_ && _)
    mux.io.inputs(i).taskID := dataIn_R.map(_.taskID).reduceLeft(_ | _)
  }

  buffer.io.enq.bits <> mux.io.output
  buffer.io.enq.valid := dataIn_valid_R.reduceLeft(_ && _)
  buffer.io.enq.bits.predicate := true.B

  val s_idle :: s_BufferWrite :: s_Transfer :: s_Finish :: Nil = Enum(4)
  val state = RegInit(s_idle)

  /*===============================================*
   *            Latch inputs. Wire up left       *
   *===============================================*/

  for (i <- 0 until NumIns) {
    io.in(i).ready := ~dataIn_valid_R(i)
    when(io.in(i).fire()) {
      dataIn_R(i).data := io.in(i).bits.data
      dataIn_valid_R(i) := true.B
    }
  }

  for (i <- 0 until NumOuts) {
    io.Out(i).valid := buffer.io.Out.valid //&& (buffer.io.count > 1.U)
    io.Out(i).bits.data := VecInit(buffer.io.Out.bits.map(_.data.asUInt())).asUInt()
    io.Out(i).bits.valid := buffer.io.Out.valid
    io.Out(i).bits.taskID := 0.U
    io.Out(i).bits.predicate := true.B
  }

  buffer.io.deq.ready := io.Out.map(_.ready).reduceLeft(_ && _) & buffer.io.Out.valid

  when (dataIn_valid_R.reduceLeft(_ && _) && buffer.io.enq.ready) {
    cnt.inc()
  }

  when (cnt.value === (shapeIn.N - 1).U) {
    dataIn_R.foreach(_ := CustomDataBundle.default(0.U(shapeIn.getWidth.W)))
    dataIn_valid_R.foreach(_ := false.B)
  }


}


