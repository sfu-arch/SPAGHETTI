// // See LICENSE for license details.

package verilogmain


import dataflow._
import chisel3._
import chisel3.util._

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, OrderedDecoupledHWIOTester}
import org.scalatest.{Matchers, FlatSpec}

import config._
import util._
import interfaces._



/*

class MixedDataflowPeekPoker32(df: MixedDataFlow32)(implicit p: config.Parameters) extends PeekPokeTester(df)  {
	for(t <- 0 until 50){
		step(1)
	}
}



//object MixedDataflowVerilog32 extends App {
  //implicit val p = config.Parameters.root((new MixedDataflowConfig).toInstance)
  //chisel3.iotesters.Driver.execute(args, () => new MixedDataFlow32()(p))
  //{ c => new MixedDataflowPeekPoker(c)  }
//}

class MixedDataflowVerilog32 extends  FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  it should "Not fuse tester" in {
    chisel3.iotesters.Driver.execute(Array("--backend-name", "verilator", "--target-dir", "test_run_dir"),
      () => new MixedDataFlow32()(p)) {
      c => new MixedDataflowPeekPoker32(c)
    } should be(true)
  }

}


*/
