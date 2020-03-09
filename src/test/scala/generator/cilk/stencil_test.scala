package dataflow


import chisel3._
import chisel3.Module
import org.scalatest.{FlatSpec, Matchers}
import config._
import memory._
import accel._
import scala.util.Random
import helpers._

class stencilDirect()(implicit p: Parameters) extends AccelIO(List(32, 32), List()) {

  val cache = Module(new Cache) // Simple Nasti Cache
  val memModel = Module(new NastiMemSlave) // Model of DRAM to connect to Cache

  // Connect the wrapper I/O to the memory model initialization interface so the
  // test bench can write contents at start.
  memModel.io.nasti <> cache.io.nasti
  memModel.io.init.bits.addr := 0.U
  memModel.io.init.bits.data := 0.U
  memModel.io.init.valid := false.B
  cache.io.cpu.abort := false.B

  // Wire up the cache, TM, and modules under test.
  val stencil = Module(new stencilDF())
  val stencil_detach1 = Module(new stencil_detach1DF())
  val stencil_inner = Module(new stencil_innerDF())

  stencil.io.MemReq <> DontCare
  stencil.io.MemResp <> DontCare

  val MemArbiter = Module(new MemArbiter(3))

  MemArbiter.io.cpu.MemReq(0) <> stencil_inner.io.MemReq
  stencil_inner.io.MemResp <> MemArbiter.io.cpu.MemResp(0)

  MemArbiter.io.cpu.MemReq(1) <> stencil_detach1.io.MemReq
  stencil_detach1.io.MemResp <> MemArbiter.io.cpu.MemResp(1)

  MemArbiter.io.cpu.MemReq(2) <> io.req
  io.resp <> MemArbiter.io.cpu.MemResp(2)

  cache.io.cpu.req <> MemArbiter.io.cache.MemReq
  MemArbiter.io.cache.MemResp <> cache.io.cpu.resp

  // tester to cilk_for_test02
  stencil.io.in <> io.in

  // cilk_for_test02 to task controller
  stencil_detach1.io.in <> stencil.io.call_8_out
  stencil.io.call_8_in <> stencil_detach1.io.out

  stencil_inner.io.in <> stencil_detach1.io.call_4_out
  stencil_detach1.io.call_4_in <> stencil_inner.io.out

  // cilk_for_test02 to tester
  io.out <> stencil.io.out

}


class stencilMainTM(tiles: Int)(implicit p: Parameters) extends AccelIO(List(32, 32), List()) {

  val cache = Module(new Cache) // Simple Nasti Cache
  val memModel = Module(new NastiMemSlave(latency = 80)) // Model of DRAM to connect to Cache

  // Connect the wrapper I/O to the memory model initialization interface so the
  // test bench can write contents at start.
  memModel.io.nasti <> cache.io.nasti
  memModel.io.init.bits.addr := 0.U
  memModel.io.init.bits.data := 0.U
  memModel.io.init.valid := false.B
  cache.io.cpu.abort := false.B

  // Wire up the cache, TM, and modules under test.
  val children = tiles
  val TaskControllerModule = Module(new TaskController(List(32, 32, 32), List(), 1, children))
  val stencil = Module(new stencilDF())

  stencil.io.MemReq <> DontCare
  stencil.io.MemResp <> DontCare

  val stencil_detach1 = for (i <- 0 until children) yield {
    val detach1 = Module(new stencil_detach1DF())
    detach1
  }
  val stencil_inner = for (i <- 0 until children) yield {
    val inner = Module(new stencil_innerDF())
    inner
  }

  val MemArbiter = Module(new MemArbiter((2 * children) + 1))
  for (i <- 0 until children) {
    MemArbiter.io.cpu.MemReq(i) <> stencil_inner(i).io.MemReq
    stencil_inner(i).io.MemResp <> MemArbiter.io.cpu.MemResp(i)

    MemArbiter.io.cpu.MemReq(children + i) <> stencil_detach1(i).io.MemReq
    stencil_detach1(i).io.MemResp <> MemArbiter.io.cpu.MemResp(children + i)
  }

  MemArbiter.io.cpu.MemReq(2 * children) <> io.req
  io.resp <> MemArbiter.io.cpu.MemResp(2 * children)

  cache.io.cpu.req <> MemArbiter.io.cache.MemReq
  MemArbiter.io.cache.MemResp <> cache.io.cpu.resp

  // tester to cilk_for_test02
  stencil.io.in <> io.in

  // cilk_for_test02 to task controller
  TaskControllerModule.io.parentIn(0) <> stencil.io.call_8_out

  // task controller to sub-task stencil_detach1
  for (i <- 0 until children) {
    stencil_detach1(i).io.in <> TaskControllerModule.io.childOut(i)
    stencil_inner(i).io.in <> stencil_detach1(i).io.call_4_out
    stencil_detach1(i).io.call_4_in <> stencil_inner(i).io.out
    TaskControllerModule.io.childIn(i) <> stencil_detach1(i).io.out
  }

  // Task controller to cilk_for_test02
  stencil.io.call_8_in <> TaskControllerModule.io.parentOut(0)

  // cilk_for_test02 to tester
  io.out <> stencil.io.out

}

class stencilTest01[T <: AccelIO](c: T, tiles: Int)
                                 (inAddrVec: List[Int], inDataVec: List[Int],
                                  outAddrVec: List[Int], outDataVec: List[Int])
  extends AccelTesterLocal(c)(inAddrVec, inDataVec, outAddrVec, outDataVec) {


  val dataSize = 16
  initMemory()

  // Initializing the signals
  poke(c.io.in.bits.enable.control, false.B)
  poke(c.io.in.bits.enable.taskID, 0)
  poke(c.io.in.valid, false.B)
  poke(c.io.in.bits.data("field0").data, 0)
  poke(c.io.in.bits.data("field0").taskID, 0)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0)
  poke(c.io.in.bits.data("field1").taskID, 0)
  poke(c.io.in.bits.data("field1").predicate, false.B)
  poke(c.io.out.ready, false.B)
  step(1)
  poke(c.io.in.bits.enable.control, true.B)
  poke(c.io.in.valid, true.B)
  poke(c.io.in.bits.data("field0").data, 0) // Array a[] base address
  poke(c.io.in.bits.data("field0").predicate, true.B)
  poke(c.io.in.bits.data("field1").data, 256) // Array b[] base address
  poke(c.io.in.bits.data("field1").predicate, true.B)
  poke(c.io.out.ready, true.B)
  step(1)
  poke(c.io.in.bits.enable.control, false.B)
  poke(c.io.in.valid, false.B)
  poke(c.io.in.bits.data("field0").data, 0)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0.U)
  poke(c.io.in.bits.data("field1").predicate, false.B)

  step(1)

  var time = 0
  var result = false
  while (time < 20000) {
    time += 1
    step(1)
    if (peek(c.io.out.valid) == 1) {
      result = true
      println(Console.BLUE + s"*** Result returned for t=$tiles. Run time: $time cycles." + Console.RESET)
    }
  }


  checkMemory()

  if (!result) {
    println(Console.RED + "*** Timeout." + Console.RESET)
    fail
  }
}


class stencilTest02[T <: AccelIO](c: T, tiles: Int)
                                 (inAddrVec: List[Int], inDataVec: List[Int],
                                  outAddrVec: List[Int], outDataVec: List[Int])
  extends AccelTesterLocal(c)(inAddrVec, inDataVec, outAddrVec, outDataVec) {


  val dataSize = 16
  initMemory()

  // Initializing the signals
  poke(c.io.in.bits.enable.control, false.B)
  poke(c.io.in.bits.enable.taskID, 0)
  poke(c.io.in.valid, false.B)
  poke(c.io.in.bits.data("field0").data, 0)
  poke(c.io.in.bits.data("field0").taskID, 0)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0)
  poke(c.io.in.bits.data("field1").taskID, 0)
  poke(c.io.in.bits.data("field1").predicate, false.B)
  poke(c.io.out.ready, false.B)
  step(1)
  poke(c.io.in.bits.enable.control, true.B)
  poke(c.io.in.valid, true.B)
  poke(c.io.in.bits.data("field0").data, 0) // Array a[] base address
  poke(c.io.in.bits.data("field0").predicate, true.B)
  poke(c.io.in.bits.data("field1").data, dataSize * dataSize) // Array b[] base address
  poke(c.io.in.bits.data("field1").predicate, true.B)
  poke(c.io.out.ready, true.B)
  step(1)
  poke(c.io.in.bits.enable.control, false.B)
  poke(c.io.in.valid, false.B)
  poke(c.io.in.bits.data("field0").data, 0)
  poke(c.io.in.bits.data("field0").predicate, false.B)
  poke(c.io.in.bits.data("field1").data, 0.U)
  poke(c.io.in.bits.data("field1").predicate, false.B)

  step(1)

  var time = 0
  var result = false
  while (time < 50000) {
    time += 1
    step(1)
    if (peek(c.io.out.valid) == 1) {
      result = true
      println(Console.BLUE + s"*** Result returned for t=$tiles. Run time: $time cycles." + Console.RESET)
    }
  }

  checkMemory()

  if (!result) {
    println(Console.RED + "*** Timeout." + Console.RESET)
    fail
  }
}


class stencilTester1 extends FlatSpec with Matchers {

  val inDataVec = List(
    7, 9, 3, 8,
    0, 2, 4, 8,
    3, 9, 0, 5,
    2, 2, 7, 3)
  val inAddrVec = List.range(0, 4 * 16, 4)

  val outAddrVec = List.range(256, 256 + (4 * 16), 4)
  val outDataVec = List(
    3, 3, 4, 3,
    4, 5, 6, 4,
    3, 4, 5, 4,
    2, 3, 3, 2
  )


  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  val testParams = p.alterPartial({
    case TLEN => 8
    case TRACE => false
  })
  // iotester flags:
  // -ll  = log level <Error|Warn|Info|Debug|Trace>
  // -tbn = backend <firrtl|verilator|vcs>
  // -td  = target directory
  // -tts = seed for RNG
  it should s"Test: Direct connection" in {
    chisel3.iotesters.Driver.execute(
      Array(
        "-ll", "Error",
        "-tn", "stencilDirect",
        "-tbn", "verilator",
        "-td", s"test_run_dir/stencil_direct",
        "-tts", "0001"),
      () => new stencilDirect()(testParams)) {
      c => new stencilTest01(c, 1)(inAddrVec, inDataVec, outAddrVec, outDataVec)
    } should be(true)
  }
}


class stencilTester2 extends FlatSpec with Matchers {

  val inDataVec = List(
    7, 9, 3, 8,
    0, 2, 4, 8,
    3, 9, 0, 5,
    2, 2, 7, 3)
  val inAddrVec = List.range(0, 4 * 16, 4)

  val outAddrVec = List.range(256, 256 + (4 * 16), 4)
  val outDataVec = List(
    3, 3, 4, 3,
    4, 5, 6, 4,
    3, 4, 5, 4,
    2, 3, 3, 2
  )

  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  val testParams = p.alterPartial({
    case TLEN => 8
    case TRACE => true
  })
  // iotester flags:
  // -ll  = log level <Error|Warn|Info|Debug|Trace>
  // -tbn = backend <firrtl|verilator|vcs>
  // -td  = target directory
  // -tts = seed for RNG
  //  val tile_list = List(1,2,4,8)
  val tile_list = List(1)
  for (tile <- tile_list) {
    it should s"Test: $tile tiles" in {
      chisel3.iotesters.Driver.execute(
        Array(
          "-ll", "Warn",
          "-tn", "cilkStencilTM",
          "-tbn", "verilator",
          "-td", s"test_run_dir/stencil_${tile}",
          "-tts", "0001"),
        () => new stencilMainTM(tile)(testParams)) {
        c => new stencilTest02(c, tile)(inAddrVec, inDataVec.toList, outAddrVec, outDataVec)
      } should be(true)
    }
  }
}
