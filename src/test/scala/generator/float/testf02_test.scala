package dataflow

import chisel3._
import chisel3.util._
import chisel3.Module
import chisel3.testers._
import chisel3.iotesters._
import org.scalatest.{FlatSpec, Matchers}
import muxes._
import config._
import control._
import util._
import interfaces._
import regfile._
import memory._
import stack._
import arbiters._
import loop._
import accel._
import node._
import junctions._


class testf02MainIO(implicit val p: Parameters) extends Module with CoreParams with CacheParams {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Call(List(32, 32, 32))))
    val req = Flipped(Decoupled(new MemReq))
    val resp = Output(Valid(new MemResp))
    val out = Decoupled(new Call(List(32)))
  })

  def cloneType = new testf02MainIO()(p).asInstanceOf[this.type]
}

class testf02Main(implicit p: Parameters) extends testf02MainIO {

  val cache = Module(new Cache) // Simple Nasti Cache


  val memModel = Module(new NastiMemSlave) // Model of DRAM to connect to Cache
  val memCopy = Mem(1024, UInt(32.W)) // Local memory just to keep track of writes to cache for validation

  // Connect the wrapper I/O to the memory model initialization interface so the
  // test bench can write contents at start.
  memModel.io.nasti <> cache.io.nasti
  memModel.io.init.bits.addr := 0.U
  memModel.io.init.bits.data := 0.U
  memModel.io.init.valid := false.B
  cache.io.cpu.abort := false.B

  // Wire up the cache and modules under test.
  val test01_fp = Module(new testf02DF())

  //Put an arbiter infront of cache
  val CacheArbiter = Module(new MemArbiter(2))

  // Connect input signals to cache
  CacheArbiter.io.cpu.MemReq(0) <> test01_fp.io.MemReq
  test01_fp.io.MemResp <> CacheArbiter.io.cpu.MemResp(0)

  //Connect main module to cache arbiter
  CacheArbiter.io.cpu.MemReq(1) <> io.req
  io.resp <> CacheArbiter.io.cpu.MemResp(1)

  //Connect cache to the arbiter
  cache.io.cpu.req <> CacheArbiter.io.cache.MemReq
  CacheArbiter.io.cache.MemResp <> cache.io.cpu.resp

  //Connect in/out ports
  test01_fp.io.in <> io.in
  io.out <> test01_fp.io.out

  // Check if trace option is on or off
  if (p(TRACE) == false) {
    println(Console.RED + "****** Trace option is off. *********" + Console.RESET)
  }
  else
    println(Console.BLUE + "****** Trace option is on. *********" + Console.RESET)

}


class testf02Test01[T <: testf02MainIO](c: T) extends PeekPokeTester(c) {


  /**
    * test02DF interface:
    *
    * in = Flipped(Decoupled(new Call(List(...))))
    * out = Decoupled(new Call(List(32)))
    */


  // Initializing the signals

  poke(c.io.in.valid, false.B)

  poke(c.io.in.bits.data("field0").data, 0.U)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0.U)
  poke(c.io.in.bits.data("field1").predicate, false.B)
  poke(c.io.out.ready, false.B)
  step(1)

  poke(c.io.in.valid, true.B)
  poke(c.io.in.bits.enable.control, true.B)
  poke(c.io.in.bits.data("field0").data, 0x41100000.U)
  poke(c.io.in.bits.data("field0").predicate, true.B)
  poke(c.io.in.bits.data("field1").data, 0x4059999a.U)
  poke(c.io.in.bits.data("field1").predicate, true.B)
  poke(c.io.out.ready, true.B)
  step(1)

  poke(c.io.in.valid, false.B)
  poke(c.io.in.bits.enable.control, false.B)
  poke(c.io.in.bits.data("field0").data, 0.U)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0.U)
  poke(c.io.in.bits.data("field1").predicate, false.B)
  poke(c.io.out.ready, true.B)
  step(1)

  var time = 1
  var result = false

  while (time < 200) {
    time += 1
    step(1)
    //println(s"Cycle: $time")
    if (peek(c.io.out.valid) == 1 &&
      peek(c.io.out.bits.data("field0").predicate) == 1) {
      result = true
      val data = peek(c.io.out.bits.data("field0").data)
      if (data != 0x41466667) {
        println(Console.RED + s"*** Incorrect result received. Got $data. Hoping for 11")
        fail
      } else {
        println(Console.BLUE + s"*** Correct result received @ cycle: $time.")
      }
    }
  }
  if (!result) {
    println("*** Timeout.")
    fail
  }


}

class testf02Tester extends FlatSpec with Matchers {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  it should "Check that testf02 works correctly." in {
    // iotester flags:
    // -ll  = log level <Error|Warn|Info|Debug|Trace>
    // -tbn = backend <firrtl|verilator|vcs>
    // -td  = target directory
    // -tts = seed for RNG
    chisel3.iotesters.Driver.execute(
      Array(
        // "-ll", "Info",
        "-tbn", "verilator",
        "-td", "test_run_dir",
        "-tts", "0001"),
      () => new testf02Main()) {
      c => new testf02Test01(c)
    } should be(true)
  }
}

