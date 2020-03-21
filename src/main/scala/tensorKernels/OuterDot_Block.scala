
package tensorKernels

import chisel3._
import chisel3.util._
import config._
import dnn.CooSCALNode
import dnn.memory._
import dnn.types.{OperatorCooSCAL, OperatorDot, OperatorReduction}
import dnnnode.{CooShapeTransformer, CooShifter, ShapeTransformer}
import interfaces.CooDataBundle
import node.{Shapes, vecN}
import shell._
//import vta.util.config._


/** OuterProduct Block
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class OuterDot_BlockIO(memTensorType: String = "none")(implicit val p: Parameters)
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

    val nnz_A = Input(UInt(mp.addrBits.W))
    val nnz_B = Input(UInt(mp.addrBits.W))
    val segSize = Input(UInt(mp.addrBits.W))

    val vme_rd_ptr = Vec(2, new VMEReadMaster)
    val vme_rd_ind = Vec(2, new VMEReadMaster)
    val vme_rd_val = Vec(2, new VMEReadMaster)

    val out = Decoupled(new CooDataBundle(UInt(p(XLEN).W)))
    val eopOut = Output(Bool( ))
    val lastOut = Output(Bool( ))

    val inDMA_time = Output(UInt(mp.addrBits.W))
    val merge_time = Output(UInt(mp.addrBits.W))
  })
}

class OuterDot_Block[L <: Shapes : OperatorDot : OperatorReduction : OperatorCooSCAL]
(memTensorType: String = "none", maxRowLen: Int, maxColLen: Int)
(segShape: => L)(implicit p: Parameters)
  extends OuterDot_BlockIO(memTensorType)(p) {

  val shape = new vecN(1, 0, false)

  val indDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val valDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val ptrDMA_A =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))

  val indDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val valDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))
  val ptrDMA_B =  Module(new inDMA_act_HWC(NumRows = 1, 1, memTensorType))

  val shapeTransformer_A = Module(new CooShapeTransformer(rowBased = true, 20, memTensorType)(segShape))
  val shapeTransformer_B = Module(new CooShifter(rowBased = false, 20, memTensorType)(segShape))

  val ptrST_A = Module(new ShapeTransformer(NumRows = 1, NumOuts = 1, 20, memTensorType)(shape))
  val ptrST_B = Module(new ShapeTransformer(NumRows = 1, NumOuts = 1, 20, memTensorType)(shape))
  val ptrDiff_A = Module(new Queue(UInt(p(XLEN).W), 20))
  val ptrDiff_B = Module(new Queue(UInt(p(XLEN).W), 20))


  val mul = Module(new CooSCALNode(N = 1, ID = 0, opCode = "Mul")(segShape))


//  val row_merger = Module(new MergeSort(maxStreamLen = maxRowLen, ID = 1, rowBased = true))

  val sIdle :: sInRead :: sExec :: Nil = Enum(3)
  val state = RegInit(sIdle)

  io.done := false.B

  val inDMA_time = Counter(2000)
  val merge_time = Counter(2000)

  io.inDMA_time := inDMA_time.value
  io.merge_time := merge_time.value

  /* ================================================================== *
    *                      inDMA_acts & loadNodes                       *
    * ================================================================== */

  ptrDMA_A.io.start := io.start
  ptrDMA_A.io.rowWidth := io.segSize
  ptrDMA_A.io.depth := 1.U
  ptrDMA_A.io.baddr := io.ptr_A_BaseAddr

  indDMA_A.io.start := io.start
  indDMA_A.io.rowWidth := io.nnz_A
  indDMA_A.io.depth := 1.U
  indDMA_A.io.baddr := io.ind_A_BaseAddr

  valDMA_A.io.start := io.start
  valDMA_A.io.rowWidth := io.nnz_A
  valDMA_A.io.depth := 1.U
  valDMA_A.io.baddr := io.val_A_BaseAddr

  ptrDMA_B.io.start := io.start
  ptrDMA_B.io.rowWidth := io.segSize
  ptrDMA_B.io.depth := 1.U
  ptrDMA_B.io.baddr := io.ptr_B_BaseAddr

  indDMA_B.io.start := io.start
  indDMA_B.io.rowWidth := io.nnz_B
  indDMA_B.io.depth := 1.U
  indDMA_B.io.baddr := io.ind_B_BaseAddr

  valDMA_B.io.start := io.start
  valDMA_B.io.rowWidth := io.nnz_B
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
  shapeTransformer_A.io.len := io.nnz_A
  shapeTransformer_B.io.len := io.nnz_B

  ptrST_A.io.len := io.segSize
  ptrST_B.io.len := io.segSize
  ptrST_A.io.depth := 1.U
  ptrST_B.io.depth := 1.U

  ptrDiff_A.io.deq.ready := false.B
  ptrDiff_B.io.deq.ready := false.B

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
     *                       multiplier and st                          *
     * ================================================================== */

  mul.io.scal.bits := shapeTransformer_B.io.out.bits
  mul.io.scal.valid := shapeTransformer_B.io.out.valid && ptrDiff_B.io.deq.valid
  shapeTransformer_B.io.out.ready := false.B

  mul.io.vec(0).bits := shapeTransformer_A.io.out(0).bits
  mul.io.vec(0).valid := shapeTransformer_A.io.out(0).valid && ptrDiff_A.io.deq.valid
  shapeTransformer_A.io.out(0).ready := false.B

  io.out <> mul.io.out(0)

  /* ================================================================== *
    *                         row merger output                         *
    * ================================================================== */

//  io.out <> row_merger.io.out
//  io.eopOut := row_merger.io.eopOut

  /* ================================================================== *
    *                          State Machine                            *
    * ================================================================== */

  when(state === sIdle){
    inDMA_time.value := 0.U
    merge_time.value := 0.U
  }

  when(state === sInRead) {inDMA_time.inc()}
  when(state === sExec) {merge_time.inc()}

  val bCnt = Counter(maxRowLen)
  val aCnt = Counter(maxRowLen)

  shapeTransformer_B.io.idx := bCnt.value
  shapeTransformer_B.io.numDeq := ptrDiff_B.io.deq.bits

  val outCnt_a = Counter(maxRowLen)
  val outCnt_b = Counter(maxRowLen)
  when(ptrDiff_A.io.deq.fire()){outCnt_a.inc()}
  when(ptrDiff_B.io.deq.fire()){outCnt_b.inc()}

//  row_merger.io.eopIn := false.B
//  row_merger.io.lastIn := false.B
  io.eopOut := false.B
  io.lastOut := false.B


  val colAisZero = Wire(Bool())
  colAisZero := false.B
  when(ptrDiff_A.io.deq.bits === 0.U) {colAisZero := true.B}

  val rowBisZero = Wire(Bool())
  rowBisZero := false.B
  when(ptrDiff_B.io.deq.bits === 0.U) {rowBisZero := true.B}

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
      }
    }
    is(sExec) {

      when(ptrDiff_A.io.deq.valid && ptrDiff_B.io.deq.valid){
        when(mul.io.scal.ready && mul.io.vec(0).ready && !colAisZero && !rowBisZero) {
          bCnt.inc()
          when(bCnt.value === ptrDiff_B.io.deq.bits - 1.U) {
            shapeTransformer_A.io.out(0).ready := true.B
            bCnt.value := 0.U
            aCnt.inc()
            when((aCnt.value === ptrDiff_A.io.deq.bits - 1.U)) {
              aCnt.value := 0.U
              shapeTransformer_B.io.out.ready := true.B
              ptrDiff_A.io.deq.ready := true.B
              ptrDiff_B.io.deq.ready := true.B
            }
          }
        }.elsewhen(colAisZero && !rowBisZero) {
          shapeTransformer_B.io.out.ready := true.B
          bCnt.inc()
        }.elsewhen(rowBisZero && !colAisZero) {
          shapeTransformer_A.io.out(0).ready := true.B
          aCnt.inc()
          when((aCnt.value === ptrDiff_A.io.deq.bits - 1.U)) {
            aCnt.value := 0.U
            shapeTransformer_B.io.out.ready := true.B
            ptrDiff_A.io.deq.ready := true.B
            ptrDiff_B.io.deq.ready := true.B
          }
        }.elsewhen(colAisZero && rowBisZero) {
          ptrDiff_A.io.deq.ready := true.B
          ptrDiff_B.io.deq.ready := true.B
        }
      }

      when(outCnt_a.value === io.segSize && outCnt_b.value === io.segSize) {
        io.eopOut := true.B
        io.lastOut := true.B
        state := sIdle
      }
    }

  }
}
