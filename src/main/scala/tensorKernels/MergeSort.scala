
package tensorKernels

import chisel3.util.{Counter, Decoupled, Queue, isPow2, log2Ceil}
import chisel3.{Flipped, Module, UInt, _}
import config.{Parameters, XLEN}
import interfaces.{BoolBundle, CooDataBundle}
import muxes.{Demux, Mux}

class MergeSortIO(maxStreamLen: Int)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val eopIn = Input(Bool( ))
    val in = Flipped(Decoupled(new CooDataBundle(UInt(p(XLEN).W))))
    val out = Decoupled(new CooDataBundle(UInt(p(XLEN).W)))
    val eopOut = Output(Bool( ))
  })
}

class MergeSort(maxStreamLen: Int, ID: Int, rowBased: Boolean)(implicit p: Parameters)
  extends MergeSortIO(maxStreamLen)(p) {
  require(maxStreamLen > 0, "Level must be greater than zero")

  val num_Merger = log2Ceil(maxStreamLen)

  val merger = for (i <-0 until num_Merger) yield {
    val Merger = Module(new MergeNode(level = math.pow(2,i).toInt, ID = 1, rowBased = rowBased))
    Merger
  }

  /*===============================================*
   *                Connections                    *
   *===============================================*/
  merger(0).io.eopIn := io.eopIn

  val sel = RegInit(false.B)
  when(io.in.fire()) {sel := !sel}

  val demux = Module(new Demux(new CooDataBundle(UInt(p(XLEN).W)), Nops = 2))

  demux.io.en := io.in.valid
  demux.io.input <> io.in.bits
  demux.io.sel := sel

  merger(0).io.in1.bits <> demux.io.outputs(0)
  merger(0).io.in2.bits <> demux.io.outputs(1)

  merger(0).io.in1.valid := demux.io.outputs(0).valid
  merger(0).io.in2.valid := demux.io.outputs(1).valid

  io.in.ready := Mux(sel, merger(0).io.in2.ready, merger(0).io.in1.ready)

  for (i <-1 until num_Merger) {
    merger(i).io.in1 <> merger(i-1).io.out1
    merger(i).io.in2 <> merger(i-1).io.out2
    merger(i).io.eopIn := merger(i-1).io.eopOut
  }

  io.out <> merger(num_Merger - 1).io.out1
  merger(num_Merger - 1).io.out2.ready := false.B

  io.eopOut := merger(num_Merger - 1).io.eopOut
}