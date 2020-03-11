
package tensorKernels

import chisel3._
import chisel3.util._
import config._
import dnn.CooSCALNode
import dnn.memory._
import dnn.types.{OperatorCooSCAL, OperatorDot, OperatorReduction}
import dnnnode.{DGEMVNode, Mac1D, ShapeTransformer}
import interfaces.ControlBundle
import node.{Shapes, vecN}
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
class MVM_BlockIO(NumRows: Int, memTensorType: String = "none")(implicit val p: Parameters)
  extends Module {
  val tpMem = new TensorParams(memTensorType)

  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val in1_BaseAddr = Input(UInt(mp.addrBits.W))
    val in2_BaseAddr = Input(UInt(mp.addrBits.W))

    val outBaseAddr = Input(UInt(mp.addrBits.W))

    val len = Input(UInt(mp.addrBits.W))

    val vme_rd = Vec(NumRows, new VMEReadMaster)
//    val vme_wr = Vec(NumRows, new VMEWriteMaster)
    val vme_wr = new VMEWriteMaster


    val inDMA_time = Output(UInt(mp.addrBits.W))
    val outDMA_time = Output(UInt(mp.addrBits.W))
    val merge_time = Output(UInt(mp.addrBits.W))
  })
}

class MVM_Block[L <: Shapes : OperatorDot : OperatorReduction : OperatorCooSCAL]
(NumRows: Int, memTensorType: String = "none")
(vecShape: => L)(implicit p: Parameters)
  extends MVM_BlockIO(NumRows, memTensorType)(p) {

  val inDMA1 =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType)(vecShape))
  val inDMA2 =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType)(vecShape))

  val ShapeTransformer = Module(new ShapeTransformer(NumRows = 2, 1, 20, memTensorType)(vecShape))

  val shape = new vecN(2, 0, false)
  val mul = Module(new CooSCALNode(N = 2, ID = 0, opCode = "Mul")(shape))


  val merger = Module(new MergeSort(maxStreamLen = 16, ID = 1, rowBased = true))

  val outDMA = Module(new outDMA_act(NumRows = 1, 20, memTensorType))

  val readTensorCnt = Counter(tpMem.memDepth)

  val sIdle :: sInRead :: sExec :: Nil = Enum(3)
  val state = RegInit(sIdle)

  io.done := false.B
  merger.io.start := false.B

  val inDMA_time = Counter(2000)
  val outDMA_time = Counter(2000)
  val merge_time = Counter(2000)

  io.inDMA_time := inDMA_time.value
  io.outDMA_time := outDMA_time.value
  io.merge_time := merge_time.value

  /* ================================================================== *
    *                      inDMA_acts & loadNodes                       *
    * ================================================================== */

  inDMA1.io.start := io.start
  inDMA1.io.rowWidth := io.len
  inDMA1.io.depth := 1.U
  inDMA1.io.baddr := io.in1_BaseAddr

  inDMA2.io.start := io.start
  inDMA2.io.rowWidth := io.len
  inDMA2.io.depth := 1.U
  inDMA2.io.baddr := io.in2_BaseAddr

  ShapeTransformer.io.rowWidth := io.len
  ShapeTransformer.io.depth := 1.U

  inDMA1.io.tensor(0) <> ShapeTransformer.io.tensor(0)
  inDMA2.io.tensor(0) <> ShapeTransformer.io.tensor(1)
  io.vme_rd(0) <> inDMA1.io.vme_rd(0)
  io.vme_rd(1) <> inDMA2.io.vme_rd(0)


  io.vme_wr <> outDMA.io.vme_wr(0)
//  for (i <- 0 until NumRows) {
//    io.vme_wr(i) <> outDMA.io.vme_wr(i)
//    outDMA.io.last(i) := Merger.io.last
//  }

  outDMA.io.last(0) := merger.io.last
  merger.io.len := 5.U
//  outDMA.io.last(1) := false.B

  val inDMA_doneR = for (i <- 0 until 2) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }

  ShapeTransformer.io.start := inDMA_doneR.reduceLeft(_ && _)

  when (inDMA_doneR.reduceLeft(_ && _)) {
    inDMA_doneR.foreach(a => a := false.B)
  }

  when(inDMA1.io.done) {inDMA_doneR(0) := true.B}
  when(inDMA2.io.done) {inDMA_doneR(1) := true.B}

  outDMA.io.start := merger.io.done

  /* ================================================================== *
    *                        loadNodes & mac1Ds                         *
    * ================================================================== */

//  Merger.io.in1 <> ShapeTransformer.io.Out(0)(0)
  //  Merger.io.in2 <> ShapeTransformer.io.Out(1)(0)
  mul.io.scal.bits.data := ShapeTransformer.io.Out(0)(0).bits.data
  mul.io.scal.bits.row := 0.U
  mul.io.scal.bits.col := 0.U
  mul.io.scal.bits.valid := true.B
  mul.io.scal.valid := ShapeTransformer.io.Out(0)(0).valid

  for (i <- 0 until 2) {
    mul.io.vec(i).bits.data := ShapeTransformer.io.Out(0)(0).bits.data
    mul.io.vec(i).bits.row := 0.U
    mul.io.vec(i).bits.col := 0.U
    mul.io.vec(i).bits.valid := true.B
    mul.io.vec(i).valid := ShapeTransformer.io.Out(0)(0).valid

    mul.io.out(i).ready := merger.io.in.ready

  }

  ShapeTransformer.io.Out(0)(0).ready := mul.io.scal.ready

  merger.io.in.valid := mul.io.out(0).valid
  merger.io.in.bits.data := mul.io.out(0).bits.data
  merger.io.in.bits.row := 0.U
  merger.io.in.bits.col := 0.U
  merger.io.in.bits.valid := true.B


  ShapeTransformer.io.Out(1)(0).ready := mul.io.scal.ready //Merger.io.in2.ready

//  merger.io.in.valid := ShapeTransformer.io.Out(0)(0).valid
//  ShapeTransformer.io.Out(0)(0).ready := merger.io.in.ready
//  merger.io.in.bits.data := ShapeTransformer.io.Out(0)(0).bits.data
//  merger.io.in.bits.row := 0.U
//  merger.io.in.bits.col := 0.U
//  merger.io.in.bits.valid := true.B


  outDMA.io.baddr := io.outBaseAddr
  outDMA.io.rowWidth := io.len

//  outDMA.io.in(0) <> Merger.io.out1
//  outDMA.io.in(1) <> Merger.io.out2
  merger.io.out.ready := outDMA.io.in(0).ready
  outDMA.io.in(0).valid := merger.io.out.valid
  outDMA.io.in(0).bits.data := merger.io.out.bits.data
  outDMA.io.in(0).bits.predicate := true.B
  outDMA.io.in(0).bits.valid := true.B
  outDMA.io.in(0).bits.taskID := 0.U

//  Merger.io.out2.ready := outDMA.io.in(1).ready
//  outDMA.io.in(1).valid := Merger.io.out2.valid
//  outDMA.io.in(1).bits.data := Merger.io.out2.bits.data
//  outDMA.io.in(1).bits.predicate := true.B
//  outDMA.io.in(1).bits.valid := true.B
//  outDMA.io.in(1).bits.taskID := 0.U



  /* ================================================================== *
      *                        Done Signal                              *
      * ================================================================== */

  when(state === sIdle){
    inDMA_time.value := 0.U
    outDMA_time.value := 0.U
    merge_time.value := 0.U
  }

  when(state === sInRead) {inDMA_time.inc()}
  when(state === sExec) {merge_time.inc()}

  switch(state) {
    is(sIdle) {
      when(io.start) {
        inDMA1.io.start := true.B
        inDMA2.io.start := true.B
        state := sInRead
      }
    }
    is(sInRead) {
      when(inDMA_doneR.reduceLeft(_ && _)){
        state := sExec
        merger.io.start := true.B
      }
    }
    is(sExec){
      when(outDMA.io.done) {
        io.done := true.B
        state := sIdle
      }

    }
  }
}
