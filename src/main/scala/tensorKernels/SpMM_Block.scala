package tensorKernels

import chisel3._
import chisel3.util._
import config._
import dnn.memory._
import dnn.types.{OperatorCooSCAL, OperatorDot, OperatorNRSCAL, OperatorReduction}
import node.{Shapes, vecN}
import shell._


/** TensorLoad.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class SpMM_BlockIO(numSegments: Int, memTensorType: String = "none")(implicit val p: Parameters)
  extends Module {
  val tpMem = new TensorParams(memTensorType)

  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val ind_A_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val val_A_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val ptr_A_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))

    val ind_B_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val val_B_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val ptr_B_BaseAddr = Vec(numSegments, Input(UInt(mp.addrBits.W)))

    val outBaseAddr_row = Input(UInt(mp.addrBits.W))
    val outBaseAddr_col = Input(UInt(mp.addrBits.W))
    val outBaseAddr_val = Input(UInt(mp.addrBits.W))

    val nnz_A = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val nnz_B = Vec(numSegments, Input(UInt(mp.addrBits.W)))
    val segSize = Vec(numSegments, Input(UInt(mp.addrBits.W)))

    val vme_rd_ptr = Vec(2 * numSegments, new VMEReadMaster)
    val vme_rd_ind = Vec(2 * numSegments, new VMEReadMaster)
    val vme_rd_val = Vec(2 * numSegments, new VMEReadMaster)

    val vme_wr_row = new VMEWriteMaster
    val vme_wr_col = new VMEWriteMaster
    val vme_wr_val = new VMEWriteMaster

    val inDMA_time = Output(UInt(mp.addrBits.W))
    val outDMA_len = Output(UInt(mp.addrBits.W))
    val merge_time = Output(UInt(mp.addrBits.W))
  })
}

class SpMM_Block[L <: Shapes : OperatorDot : OperatorReduction : OperatorNRSCAL : OperatorCooSCAL]
(numSegments: Int, memTensorType: String = "none", maxRowLen: Int, maxColLen: Int)
(segShape: => L)(implicit p: Parameters)
  extends SpMM_BlockIO(numSegments = numSegments, memTensorType)(p) {


  val seg = for (i <- 0 until numSegments) yield {
    val outDot = Module(new OuterDot(memTensorType = "inp", maxRowLen = maxRowLen, maxColLen = maxColLen)(segShape))
    outDot
  }

  val row_merger = for (i <- 0 until numSegments) yield {
    val rowMerger = Module(new MergeSort(maxStreamLen = maxRowLen, ID = 1, rowBased = true))
    rowMerger
  }

  val arbiter = Module(new WeightedArbiter(n = numSegments))

  val col_merger = Module(new MergeAdd(maxStreamLen = maxColLen, ID = 1, rowBased = false)(segShape))

  val outDMA = Module(new outDMA_coo(bufSize = 20, memTensorType))

  val inDMA_time = Counter(2000)
  val outDMA_time = Counter(2000)
  val merge_time = Counter(2000)

  io.inDMA_time := inDMA_time.value
  io.outDMA_len := outDMA.io.outLen
  io.merge_time := merge_time.value

  /* ================================================================== *
    *                      inDMA_acts & loadNodes                       *
    * ================================================================== */
  for (i <- 0 until numSegments) {
    seg(i).io.start := io.start

    seg(i).io.ind_A_BaseAddr := io.ind_A_BaseAddr(i)
    seg(i).io.ptr_A_BaseAddr := io.ptr_A_BaseAddr(i)
    seg(i).io.val_A_BaseAddr := io.val_A_BaseAddr(i)

    seg(i).io.ind_B_BaseAddr := io.ind_B_BaseAddr(i)
    seg(i).io.ptr_B_BaseAddr := io.ptr_B_BaseAddr(i)
    seg(i).io.val_B_BaseAddr := io.val_B_BaseAddr(i)

    seg(i).io.nnz_A := io.nnz_A(i)
    seg(i).io.nnz_B := io.nnz_B(i)
    seg(i).io.segSize := io.segSize(i)

    io.vme_rd_ind(2 * i + 0) <> seg(i).io.vme_rd_ind(0)
    io.vme_rd_ind(2 * i + 1) <> seg(i).io.vme_rd_ind(1)

    io.vme_rd_ptr(2 * i + 0) <> seg(i).io.vme_rd_ptr(0)
    io.vme_rd_ptr(2 * i + 1) <> seg(i).io.vme_rd_ptr(1)

    io.vme_rd_val(2 * i + 0) <> seg(i).io.vme_rd_val(0)
    io.vme_rd_val(2 * i + 1) <> seg(i).io.vme_rd_val(1)

    row_merger(i).io.in <> seg(i).io.out
    row_merger(i).io.eopIn := seg(i).io.eopOut
    row_merger(i).io.lastIn := seg(i).io.lastOut

    arbiter.io.in(i) <> row_merger(i).io.out
    arbiter.io.eopIn(i) := row_merger(i).io.eopOut
    arbiter.io.lastIn(i) := row_merger(i).io.lastOut
  }

  io.vme_wr_row <> outDMA.io.vme_wr_row
  io.vme_wr_col <> outDMA.io.vme_wr_col
  io.vme_wr_val <> outDMA.io.vme_wr_val

  outDMA.io.baddr_row := io.outBaseAddr_row
  outDMA.io.baddr_col := io.outBaseAddr_col
  outDMA.io.baddr_val := io.outBaseAddr_val

  /* ================================================================== *
    *         outDot -> row_merger -> col_merger -> outDMA              *
    * ================================================================== */


  col_merger.io.eopIn := arbiter.io.eopOut
  col_merger.io.in <> arbiter.io.out

  outDMA.io.in <> col_merger.io.out
  outDMA.io.eop := col_merger.io.lastOut
  io.done := outDMA.io.done
}
