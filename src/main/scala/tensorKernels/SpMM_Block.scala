
package tensorKernels

import chisel3._
import chisel3.util._
import config._
import dnn.CooSCALNode
import dnn.memory._
import dnn.types.{OperatorCooSCAL, OperatorDot, OperatorReduction}
import dnnnode.{CooShapeTransformer, DGEMVNode, Mac1D, ShapeTransformer}
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
class SpMM_BlockIO(NumRows: Int, memTensorType: String = "none")(implicit val p: Parameters)
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

class SpMM_Block[L <: Shapes : OperatorDot : OperatorReduction : OperatorCooSCAL]
(NumRows: Int, memTensorType: String = "none")
(segShape: => L)(implicit p: Parameters)
  extends SpMM_BlockIO(NumRows, memTensorType)(p) {

  val indDMA =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val valDMA =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))

  val shapeTransformer = Module(new CooShapeTransformer(20, memTensorType)(segShape))

  val mul = Module(new CooSCALNode(N = 2, ID = 0, opCode = "Mul")(segShape))


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

  indDMA.io.start := io.start
  indDMA.io.rowWidth := io.len
  indDMA.io.depth := 1.U
  indDMA.io.baddr := io.in1_BaseAddr

  valDMA.io.start := io.start
  valDMA.io.rowWidth := io.len
  valDMA.io.depth := 1.U
  valDMA.io.baddr := io.in2_BaseAddr

  shapeTransformer.io.len := io.len

  indDMA.io.tensor(0) <> shapeTransformer.io.indTensor
  valDMA.io.tensor(0) <> shapeTransformer.io.valTensor
  io.vme_rd(0) <> indDMA.io.vme_rd(0)
  io.vme_rd(1) <> valDMA.io.vme_rd(0)


  io.vme_wr <> outDMA.io.vme_wr(0)

  outDMA.io.last(0) := merger.io.last
  merger.io.len := 16.U

  val inDMA_doneR = for (i <- 0 until 2) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }

  shapeTransformer.io.start := inDMA_doneR.reduceLeft(_ && _)

  when (inDMA_doneR.reduceLeft(_ && _)) {
    inDMA_doneR.foreach(a => a := false.B)
  }

  when(indDMA.io.done) {inDMA_doneR(0) := true.B}
  when(valDMA.io.done) {inDMA_doneR(1) := true.B}

  outDMA.io.start := merger.io.done

  /* ================================================================== *
    *                        loadNodes & mac1Ds                         *
    * ================================================================== */

  mul.io.scal.bits.data := shapeTransformer.io.out(0).bits.data
  mul.io.scal.bits.row := 0.U
  mul.io.scal.bits.col := 0.U
  mul.io.scal.bits.valid := true.B
  mul.io.scal.valid := shapeTransformer.io.out(0).valid

  for (i <- 0 until 2) {
    mul.io.out(i).ready := merger.io.in.ready
  }

  for (i <- 0 until segShape.getLength()) {
    mul.io.vec(i) <> shapeTransformer.io.out(i)

  }

  merger.io.in.valid := mul.io.out(0).valid
  merger.io.in.bits.data := mul.io.out(0).bits.data
  merger.io.in.bits.row := 0.U
  merger.io.in.bits.col := 0.U
  merger.io.in.bits.valid := true.B


  outDMA.io.baddr := io.outBaseAddr
  outDMA.io.rowWidth := io.len / 2.U


  merger.io.out.ready := outDMA.io.in(0).ready
  outDMA.io.in(0).valid := merger.io.out.valid
  outDMA.io.in(0).bits.data := merger.io.out.bits.data
  outDMA.io.in(0).bits.predicate := true.B
  outDMA.io.in(0).bits.valid := true.B
  outDMA.io.in(0).bits.taskID := 0.U


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
        indDMA.io.start := true.B
        valDMA.io.start := true.B
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
