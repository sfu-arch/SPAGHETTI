package accel


import arbiters.TypeStackFile
import chisel3.util._
import chisel3.{Module, RegInit, when, _}
import config._
import control.BasicBlockNoMaskNode
import dnn.wrappers.SystolicSquareWrapper
import dnn.{DotNode, ReduceNode}
import interfaces.{ControlBundle, DataBundle}
import junctions.SplitCallNew
import memory.{ReadTypMemoryController, WriteTypMemoryController}
import node.{FXmatNxN, TypLoad, TypStore}
import shell._


 /** Register File.
  *
  * Six 32-bit register file.
  *
  * -------------------------------
  *  Register description    | addr
  * -------------------------|-----
  *  Control status register | 0x00
  *  Cycle counter           | 0x04
  *  Constant value          | 0x08
  *  Vector length           | 0x0c
  *  Input pointer lsb       | 0x10
  *  Input pointer msb       | 0x14
  *  Output pointer lsb      | 0x18
  *  Output pointer msb      | 0x1c
  * -------------------------------

  * ------------------------------
  *  Control status register | bit
  * ------------------------------
  *  Launch                  | 0
  *  Finish                  | 1
  * ------------------------------
  */


/*
+------------------+                          +-----------------+
|                  | f(bits)+--------+        |                 |
|   VMEReadMaster  +------->+Buffers +-------->VMEWriteMaster   |
|                  |        +--------+        |                 |
+------------------+                          +-----------------+

 */

class DNNCoreTest(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  val buffer = Module(new Queue(io.vme.rd(0).data.bits.cloneType,40))

  val sIdle :: sReq :: sBusy :: Nil = Enum(3)
  val Rstate = RegInit(sIdle)
  val Wstate = RegInit(sIdle)

  val cycle_count = new Counter(200)

  when (Rstate =/= sIdle) {
    cycle_count.inc( )
  }


  io.vcr.ecnt(0.U).bits := cycle_count.value

  // Read state machine
  switch (Rstate) {
    is (sIdle) {
      when (io.vcr.launch) {
        cycle_count.value := 0.U
        Rstate := sReq
      }
    }
    is (sReq) {
      when (io.vme.rd(0).cmd.fire()) {
        Rstate := sBusy
      }
    }
  }
  // Write state machine
  switch (Wstate) {
    is (sIdle) {
      when (io.vcr.launch) {
        Wstate := sReq
      }
    }
    is (sReq) {
      when (io.vme.wr(0).cmd.fire()) {
        Wstate := sBusy
      }
    }
  }

  io.vme.rd(0).cmd.bits.addr := io.vcr.ptrs(0)
  io.vme.rd(0).cmd.bits.len := io.vcr.vals(1)
  io.vme.rd(0).cmd.valid := false.B

  io.vme.wr(0).cmd.bits.addr := io.vcr.ptrs(1)
  io.vme.wr(0).cmd.bits.len := io.vcr.vals(1)
  io.vme.wr(0).cmd.valid := false.B

  when(Rstate === sReq) {
    io.vme.rd(0).cmd.valid := true.B
  }

  when(Wstate === sReq) {
    io.vme.wr(0).cmd.valid := true.B
  }

  // Final
  val last = Wstate === sBusy && io.vme.wr(0).ack
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last

  when(io.vme.wr(0).ack) {
    Rstate := sIdle
    Wstate := sIdle
  }


  buffer.io.enq <> io.vme.rd(0).data
  buffer.io.enq.bits := io.vme.rd(0).data.bits + io.vcr.vals(0)
  io.vme.wr(0).data <> buffer.io.deq
}

