package accel

import FPU.FType
import chisel3.util._
import chisel3.{when, _}
import config._
import node.{FPvecN, matNxN, vecN}
import shell._
import tensorKernels.SpMM_Block

/** SparseTensorCore.
  *
  * SparseTensorCore is able to perform the linear algebraic computations on sparse tensors.
  */
class SpTensorCore(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  val cycle_count = new Counter(100000000)

  val numSegments = 3
  val numColMerger = 3


  val S = new FType(8, 24)
  val shape = new FPvecN(1, S, 0)
//  val shape = new vecN(1, 0, false)

  val block = Module(new SpMM_Block(numSegments = numSegments, numColMerger = numColMerger, memTensorType = "inp", maxRowLen = 15000, maxColLen = 2000)(shape))

  /* ================================================================== *
     *                      Basic Block signals                         *
     * ================================================================== */
  for (i <- 0 until numSegments) {
    block.io.nnz_A(i) := io.vcr.vals(i * 3 + 0)
    block.io.nnz_B(i) := io.vcr.vals(i * 3 + 1)
    block.io.segSize(i) := io.vcr.vals(i * 3 + 2)

  }

  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */

  io.vcr.ecnt(0).bits := cycle_count.value

  for (i <- 0 until numColMerger) {
    io.vcr.ecnt(i+1).bits := block.io.outDMA_len(i)
  }

  /* ================================================================== *
    *                    VME Reads and writes                           *
    * ================================================================== */

  block.io.start := false.B

  for (i <- 0 until numSegments) {
    io.vme.rd(6 * i + 0) <> block.io.vme_rd_ptr(2 * i + 0)
    io.vme.rd(6 * i + 1) <> block.io.vme_rd_ind(2 * i + 0)
    io.vme.rd(6 * i + 2) <> block.io.vme_rd_val(2 * i + 0)

    io.vme.rd(6 * i + 3) <> block.io.vme_rd_ptr(2 * i + 1)
    io.vme.rd(6 * i + 4) <> block.io.vme_rd_ind(2 * i + 1)
    io.vme.rd(6 * i + 5) <> block.io.vme_rd_val(2 * i + 1)

    block.io.ptr_A_BaseAddr(i) := io.vcr.ptrs(i * 6 + 0)
    block.io.ind_A_BaseAddr(i) := io.vcr.ptrs(i * 6 + 1)
    block.io.val_A_BaseAddr(i) := io.vcr.ptrs(i * 6 + 2)

    block.io.ptr_B_BaseAddr(i) := io.vcr.ptrs(i * 6 + 3)
    block.io.ind_B_BaseAddr(i) := io.vcr.ptrs(i * 6 + 4)
    block.io.val_B_BaseAddr(i) := io.vcr.ptrs(i * 6 + 5)
  }

  for (i <- 0 until numColMerger) {
    io.vme.wr(3 * i + 0) <> block.io.vme_wr_row(i)
    io.vme.wr(3 * i + 1) <> block.io.vme_wr_col(i)
    io.vme.wr(3 * i + 2) <> block.io.vme_wr_val(i)

    block.io.outBaseAddr_row(i) := io.vcr.ptrs((numSegments * 6) + (3 * i) + 0)
    block.io.outBaseAddr_col(i) := io.vcr.ptrs((numSegments * 6) + (3 * i) + 1)
    block.io.outBaseAddr_val(i) := io.vcr.ptrs((numSegments * 6) + (3 * i) + 2)
  }

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)

  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        block.io.start := true.B
        state := sExec
      }
    }
    is(sExec) {
      when(block.io.done) {
        state := sIdle
      }
    }
  }

  val last = state === sExec && block.io.done
  io.vcr.finish := last
  io.vcr.ecnt.map(_.valid).foreach(a => a := last)

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
