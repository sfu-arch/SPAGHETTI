
package tensorKernels

import chisel3._
import chisel3.util._
import config._
import dnn.CooSCALNode
import dnn.memory._
import dnn.types.{OperatorCooSCAL, OperatorDot, OperatorReduction}
import dnnnode.{CooShapeTransformer, DGEMVNode, MIMOQueue, Mac1D, ShapeTransformer}
import interfaces.{ControlBundle, CustomDataBundle}
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

    val ind_A_BaseAddr = Input(UInt(mp.addrBits.W))
    val val_A_BaseAddr = Input(UInt(mp.addrBits.W))
    val ptr_A_BaseAddr = Input(UInt(mp.addrBits.W))

    val ind_B_BaseAddr = Input(UInt(mp.addrBits.W))
    val val_B_BaseAddr = Input(UInt(mp.addrBits.W))
    val ptr_B_BaseAddr = Input(UInt(mp.addrBits.W))

    val outBaseAddr_row = Input(UInt(mp.addrBits.W))
    val outBaseAddr_col = Input(UInt(mp.addrBits.W))
    val outBaseAddr_val = Input(UInt(mp.addrBits.W))

    val len = Input(UInt(mp.addrBits.W))
    val segCols = Input(UInt(mp.addrBits.W))

    val vme_rd_ptr = Vec(2, new VMEReadMaster)
    val vme_rd_ind = Vec(2, new VMEReadMaster)
    val vme_rd_val = Vec(2, new VMEReadMaster)
//    val vme_wr = Vec(NumRows, new VMEWriteMaster)
    val vme_wr_row = new VMEWriteMaster
    val vme_wr_col = new VMEWriteMaster
    val vme_wr_val = new VMEWriteMaster

    val inDMA_time = Output(UInt(mp.addrBits.W))
    val outDMA_time = Output(UInt(mp.addrBits.W))
    val merge_time = Output(UInt(mp.addrBits.W))
  })
}

class SpMM_Block[L <: Shapes : OperatorDot : OperatorReduction : OperatorCooSCAL]
(NumRows: Int, memTensorType: String = "none")
(segShape: => L)(implicit p: Parameters)
  extends SpMM_BlockIO(NumRows, memTensorType)(p) {


  val shape = new vecN(1, 0, false)

  val indDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val valDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val ptrDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))

  val indDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val valDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val ptrDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))

  val shapeTransformer_A = Module(new CooShapeTransformer(rowBased = true, 20, memTensorType)(segShape))
  val shapeTransformer_B = Module(new CooShapeTransformer(rowBased = false, 20, memTensorType)(segShape))

  val ptrST_A = Module(new ShapeTransformer(NumRows = 1, NumOuts = 1, 20, memTensorType)(shape))
  val ptrST_B = Module(new ShapeTransformer(NumRows = 1, NumOuts = 1, 20, memTensorType)(shape))
  val ptrDiff_A = Module(new Queue(UInt(p(XLEN).W), 20))
  val ptrDiff_B = Module(new Queue(UInt(p(XLEN).W), 20))


  val mul = Module(new CooSCALNode(N = 1, ID = 0, opCode = "Mul")(segShape))


  val merger = Module(new MergeSort(maxStreamLen = 16, ID = 1, rowBased = true))

  val outDMA = Module(new outDMA_coo(bufSize = 24, memTensorType))

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

  ptrDMA_A.io.start := io.start
  ptrDMA_A.io.rowWidth := io.segCols
  ptrDMA_A.io.depth := 1.U
  ptrDMA_A.io.baddr := io.ptr_A_BaseAddr

  indDMA_A.io.start := io.start
  indDMA_A.io.rowWidth := io.len
  indDMA_A.io.depth := 1.U
  indDMA_A.io.baddr := io.ind_A_BaseAddr

  valDMA_A.io.start := io.start
  valDMA_A.io.rowWidth := io.len
  valDMA_A.io.depth := 1.U
  valDMA_A.io.baddr := io.val_A_BaseAddr

  ptrDMA_B.io.start := io.start
  ptrDMA_B.io.rowWidth := io.segCols
  ptrDMA_B.io.depth := 1.U
  ptrDMA_B.io.baddr := io.ptr_B_BaseAddr

  indDMA_B.io.start := io.start
  indDMA_B.io.rowWidth := io.len
  indDMA_B.io.depth := 1.U
  indDMA_B.io.baddr := io.ind_B_BaseAddr

  valDMA_B.io.start := io.start
  valDMA_B.io.rowWidth := io.len
  valDMA_B.io.depth := 1.U
  valDMA_B.io.baddr := io.val_B_BaseAddr

  io.vme_rd_ind(0) <> indDMA_A.io.vme_rd(0)
  io.vme_rd_ind(1) <> indDMA_B.io.vme_rd(0)

  io.vme_rd_val(0) <> valDMA_A.io.vme_rd(0)
  io.vme_rd_val(1) <> valDMA_B.io.vme_rd(0)

  io.vme_rd_ptr(0) <> ptrDMA_A.io.vme_rd(0)
  io.vme_rd_ptr(1) <> ptrDMA_B.io.vme_rd(0)

  indDMA_A.io.tensor(0) <> shapeTransformer_A.io.indTensor
  valDMA_A.io.tensor(0) <> shapeTransformer_A.io.valTensor
  ptrDMA_A.io.tensor(0) <> ptrST_A.io.tensor(0)

  indDMA_B.io.tensor(0) <> shapeTransformer_B.io.indTensor
  valDMA_B.io.tensor(0) <> shapeTransformer_B.io.valTensor
  ptrDMA_B.io.tensor(0) <> ptrST_B.io.tensor(0)

  /* ================================================================== *
   *                      inDMA_acts & loadNodes                       *
   * ================================================================== */
  shapeTransformer_A.io.len := io.len //10
  shapeTransformer_B.io.len := io.len

  ptrST_A.io.len := io.segCols
  ptrST_B.io.len := io.segCols
  ptrST_A.io.depth := 1.U
  ptrST_B.io.depth := 1.U

  io.vme_wr_row <> outDMA.io.vme_wr_row
  io.vme_wr_col <> outDMA.io.vme_wr_col
  io.vme_wr_val <> outDMA.io.vme_wr_val



  merger.io.len := io.len
  outDMA.io.baddr_row := io.outBaseAddr_row
  outDMA.io.baddr_col := io.outBaseAddr_col
  outDMA.io.baddr_val := io.outBaseAddr_val
  outDMA.io.len := io.len

  outDMA.io.start := merger.io.done
  outDMA.io.last := merger.io.last

  ptrDiff_A.io.deq <> DontCare
  ptrDiff_B.io.deq <> DontCare

//  outDMA_ptr_B.io.in(0).valid := ptrDiff_B.io.deq.valid
//  ptrDiff_B.io.deq.ready := outDMA_ptr_B.io.in(0).ready
//  outDMA_ptr_B.io.in(0).bits.data := ptrDiff_B.io.deq.bits
//  outDMA_ptr_B.io.in(0).bits.taskID := 0.U
//  outDMA_ptr_B.io.in(0).bits.predicate := true.B
//  outDMA_ptr_B.io.in(0).bits.valid := true.B

  /* ================================================================== *
    *                        DMA done registers                         *
    * ================================================================== */

  val DMA_doneR_A = for (i <- 0 until 3) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }
  val DMA_doneR_B = for (i <- 0 until 3) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }
  shapeTransformer_A.io.start := DMA_doneR_A.reduceLeft(_ && _)
  ptrST_A.io.start := DMA_doneR_A.reduceLeft(_ && _)

  shapeTransformer_B.io.start := DMA_doneR_B.reduceLeft(_ && _)
  ptrST_B.io.start := DMA_doneR_B.reduceLeft(_ && _)

  when (DMA_doneR_A.reduceLeft(_ && _)) {
    DMA_doneR_A.foreach(a => a := false.B)
  }
  when (DMA_doneR_B.reduceLeft(_ && _)) {
    DMA_doneR_B.foreach(a => a := false.B)
  }

  when(ptrDMA_A.io.done) {DMA_doneR_A(0) := true.B}
  when(indDMA_A.io.done) {DMA_doneR_A(1) := true.B}
  when(valDMA_A.io.done) {DMA_doneR_A(2) := true.B}
  when(ptrDMA_B.io.done) {DMA_doneR_B(0) := true.B}
  when(indDMA_B.io.done) {DMA_doneR_B(1) := true.B}
  when(valDMA_B.io.done) {DMA_doneR_B(2) := true.B}



  /* ================================================================== *
    *                        pointer difference                         *
    * ================================================================== */

  val ptrData_A = RegInit(0.U(p(XLEN).W))
  val ptrData_B = RegInit(0.U(p(XLEN).W))

  ptrData_A := ptrST_A.io.out(0)(0).bits.data.asUInt()
  ptrData_B := ptrST_B.io.out(0)(0).bits.data.asUInt()

  ptrDiff_A.io.enq.bits := ptrST_A.io.out(0)(0).bits.data.asUInt() - ptrData_A
  ptrDiff_B.io.enq.bits := ptrST_B.io.out(0)(0).bits.data.asUInt() - ptrData_B

  ptrDiff_A.io.enq.valid := ptrST_A.io.out(0)(0).valid
  ptrST_A.io.out(0)(0).ready := ptrDiff_A.io.enq.ready

  ptrDiff_B.io.enq.valid := ptrST_B.io.out(0)(0).valid
  ptrST_B.io.out(0)(0).ready := ptrDiff_B.io.enq.ready

  /* ================================================================== *
     *                       loadNodes & mac1Ds                         *
     * ================================================================== */

  mul.io.scal <> shapeTransformer_B.io.out(0)

  for (i <- 0 until segShape.getLength()) {
    mul.io.vec(i) <> shapeTransformer_A.io.out(i)
  }

  merger.io.in <> mul.io.out(0)

  outDMA.io.in <> merger.io.out


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
        indDMA_A.io.start := true.B
        valDMA_A.io.start := true.B
        state := sInRead
      }
    }
    is(sInRead) {
      when(DMA_doneR_A.reduceLeft(_ && _)){
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
