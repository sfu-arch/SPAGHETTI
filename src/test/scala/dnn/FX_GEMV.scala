package dnn

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, OrderedDecoupledHWIOTester, PeekPokeTester}
import org.scalatest.{FlatSpec, Matchers}
import dataflow._
import muxes._
import config._
import util._
import interfaces._
import node._


// Tester.
class FXGEMVCompTests(df: GEMV_1Cycle[FXmatNxN, FXvecN, FXmatNxN])
                     (implicit p: config.Parameters) extends PeekPokeTester(df) {
  poke(df.io.enable.valid, true)
  poke(df.io.enable.bits.control, true)

  poke(df.io.LeftIO.bits.data, 0x0013001300130013L)
  poke(df.io.LeftIO.valid, true)
  poke(df.io.LeftIO.bits.predicate, true)


  poke(df.io.RightIO.bits.data, 0x0013001300130013L)
  poke(df.io.RightIO.valid, true)
  poke(df.io.RightIO.bits.predicate, true)

  poke(df.io.Out(0).ready, true.B)
  step(10)
}


class FXGEMVCompTester extends FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new Mat_VecConfig).toInstance)
  it should "Typ Compute Tester" in {
    chisel3.iotesters.Driver.execute(Array("--backend-name", "verilator", "--target-dir", "test_run_dir"),
      () => new GEMV_1Cycle(NumOuts = 1, ID = 0, opCode = "Add")(sign = false)(new FXmatNxN(2, 4), new FXvecN(2, 4))(new FXmatNxN(2, 4))) {
      c => new FXGEMVCompTests(c)
    } should be(true)
  }
}
