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
import util._
import interfaces._


// Tester.
class DSPALUTester(df: DSPALU[FixedPoint])
                  (implicit p: config.Parameters) extends PeekPokeTester(df) {


  poke(df.io.in1, 0x14.U)
  poke(df.io.in2, 0x14.U)
  if (df.opCode == "Mac") poke(df.io.in3.get, 0x14.U)
  step(1)
  print(s"Output: ${(peek(df.io.out))}\n")
}

class DSPALUTests extends FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  it should "ALU tester" in {
    chisel3.iotesters.Driver.execute(Array("--backend-name", "firrtl", "--target-dir", "test_run_dir"),
      () => new DSPALU(FixedPoint(32.W, 4.BP), opCode = "Mac")) {
      c => new DSPALUTester(c)
    } should be(true)
  }
}