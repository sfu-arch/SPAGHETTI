// See LICENSE for license details.

package node

import chisel3._
import chisel3.util._
import FPU._
import FType._

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, OrderedDecoupledHWIOTester}
import org.scalatest.{Matchers, FlatSpec}

import node._
import dataflow._
import muxes._
import config._
import util._
import interfaces._


// Tester.
class UALUTester(df: UALU)
                (implicit p: config.Parameters) extends PeekPokeTester(df) {


  poke(df.io.in1, 0xFFFE.U)
  poke(df.io.in2, 0xFFFE.U)
  //  if (df.opCode == "Mac") poke(df.io.in3.get, 0x0002.U)
  step(1)
  print(s"Output: ${(peek(df.io.out))}\n")
}

class ALUTests extends FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new HALFPrecisionFPConfig).toInstance)
  it should "ALU tester" in {
    chisel3.iotesters.Driver.execute(Array("--backend-name", "firrtl", "--target-dir", "test_run_dir"),
      () => new UALU(xlen = p(XLEN), opCode = "Mac", issign = true)) {
      c => new UALUTester(c)
    } should be(true)
  }
}