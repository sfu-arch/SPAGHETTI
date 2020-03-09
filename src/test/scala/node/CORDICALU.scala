// See LICENSE for license details.

package node

import chisel3._
import chisel3.util._
import FPU._
import FType._
import chisel3.core.FixedPoint
import chisel3.iotesters.{ChiselFlatSpec, Driver, OrderedDecoupledHWIOTester, PeekPokeTester}
import org.scalatest.{FlatSpec, Matchers}
import node._
import dataflow._
import muxes._
import config._
import dsptools.DspTester
import util._
import interfaces._


// Tester.
class CORDICALUTester(df: CORDICALU)
                     (implicit p: config.Parameters) extends DspTester(df) {


  poke(df.io.in1, 0x62.U)
  poke(df.io.in2, 0x62.U)
  print(s"${peek(df.io.out)}")

}

class CORDICALUTests extends FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  it should "ALU tester" in {
    chisel3.iotesters.Driver.execute(Array("--backend-name", "verilator", "--target-dir", "test_run_dir"),
      () => new CORDICALU(16, 4, opCode = "sin")) {
      c => new CORDICALUTester(c)
    } should be(true)
  }
}