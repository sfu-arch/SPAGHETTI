package accel

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import chisel3.util._
import chisel3.{when, _}
import config._
import dnn.memory.ISA._
import dnn.memory.{TensorLoad, TensorStore}
import shell._

/** Core.
  *
  * The core defines the current VTA architecture by connecting memory and
  * compute modules together such as load/store and compute. Most of the
  * connections in the core are bulk (<>), and we should try to keep it this
  * way, because it is easier to understand what is going on.
  *
  * Also, the core must be instantiated by a shell using the
  * VTA Control Register (VCR) and the VTA Memory Engine (VME) interfaces.
  * More info about these interfaces and modules can be found in the shell
  * directory.
  */
class DNNCoreTest2(implicit val p: Parameters) extends Module {
  //  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  val cycle_count = new Counter(2000)


  val tensorLoad1 = Module(new TensorLoad(tensorType = "inp"))
  val tensorLoad2 = Module(new TensorLoad(tensorType = "inp"))
  val tensorStore = Module(new TensorStore(tensorType = "inp"))
  val tl_Inst = Wire(new MemDecode)
  val ts_Inst = Wire(new MemDecode)
  val indexCnt = Counter(100)
  val storeIndex = RegNext(next = indexCnt.value, init = 0.U)


//  val shape = new matNxN(2, false)
//  val mac = Module(new MacNode(NumOuts = 1, lanes = 4, ID = 1)(shape))

//  mac.io.enable.bits.control := true.B
//  mac.io.enable.valid := true.B

//  mac.io.LeftIO.bits.data := tensorLoad1.io.tensor.rd.data.bits.asUInt()

//  val WControl = new WriteTypMemoryController(NumOps = 1, BaseSize = 2, NumEntries = 1)
//  val RControl = new ReadTypMemoryController(NumOps = 2, BaseSize = 2, NumEntries = 2)


  io.vcr.ecnt(0).bits := cycle_count.value

  io.vme.rd(0) <> tensorLoad1.io.vme_rd
  io.vme.rd(1) <> tensorLoad2.io.vme_rd
  io.vme.wr(0) <> tensorStore.io.vme_wr

  tensorLoad1.io.start := false.B
  tensorLoad1.io.baddr := io.vcr.ptrs(0)
  tensorLoad1.io.inst := tl_Inst.asTypeOf(UInt(INST_BITS.W))
  tensorLoad2.io.start := false.B
  tensorLoad2.io.baddr := io.vcr.ptrs(1)
  tensorLoad2.io.inst := tl_Inst.asTypeOf(UInt(INST_BITS.W))


  tensorStore.io.start := false.B
  tensorStore.io.baddr := io.vcr.ptrs(2)
  tensorStore.io.inst := ts_Inst.asTypeOf(UInt(INST_BITS.W))


  tensorLoad1.io.tensor.wr <> DontCare
  tensorLoad2.io.tensor.wr <> DontCare
  tensorStore.io.tensor.rd <> DontCare

  tensorLoad1.io.tensor.rd.idx.bits := indexCnt.value
  tensorLoad1.io.tensor.rd.idx.valid := true.B
  tensorLoad2.io.tensor.rd.idx.bits := indexCnt.value
  tensorLoad2.io.tensor.rd.idx.valid := true.B

  //  tensorStore.io.tensor.wr.bits.data := tensorLoad1.io.tensor.rd.data.bits
  for (i <- 0 until tensorLoad2.tp.tensorLength) {
    for (j <- 0 until tensorLoad2.tp.tensorWidth) {
      tensorStore.io.tensor.wr.bits.data(i)(j) := tensorLoad1.io.tensor.rd.data.bits(i)(j) + tensorLoad2.io.tensor.rd.data.bits(i)(j)
    }
  }
  tensorStore.io.tensor.wr.valid := false.B
  tensorStore.io.tensor.wr.bits.idx := storeIndex

  tl_Inst.xpad_0 := 0.U
  tl_Inst.xpad_1 := 0.U
  tl_Inst.ypad_0 := 0.U
  tl_Inst.ypad_1 := 0.U
  tl_Inst.xstride := 7.U
  tl_Inst.xsize := 7.U
  tl_Inst.ysize := 1.U
  tl_Inst.empty_0 := 0.U
  tl_Inst.dram_offset := 0.U
  tl_Inst.sram_offset := 0.U
  tl_Inst.id := 3.U
  tl_Inst.push_next := 0.U
  tl_Inst.push_prev := 0.U
  tl_Inst.pop_next := 0.U
  tl_Inst.pop_prev := 0.U
  tl_Inst.op := 0.U

  ts_Inst.xpad_0 := 0.U
  ts_Inst.xpad_1 := 0.U
  ts_Inst.ypad_0 := 0.U
  ts_Inst.ypad_1 := 0.U
  ts_Inst.xstride := 7.U
  ts_Inst.xsize := 7.U
  ts_Inst.ysize := 1.U
  ts_Inst.empty_0 := 0.U
  ts_Inst.dram_offset := 0.U
  ts_Inst.sram_offset := 0.U
  ts_Inst.id := 4.U
  ts_Inst.push_next := 0.U
  ts_Inst.push_prev := 0.U
  ts_Inst.pop_next := 0.U
  ts_Inst.pop_prev := 0.U
  ts_Inst.op := 0.U

  val sIdle :: sReadTensor1 :: sReadTensor2 :: sTransferTensor :: sWriteTensor :: Nil = Enum(5)
  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        tensorLoad1.io.start := true.B
        //          tensorLoad2.io.start := true.B
        indexCnt.value := 0.U
        state := sReadTensor1
      }
    }
    is(sReadTensor1) {
      when(tensorLoad1.io.done) { // && tensorLoad2.io.done) {
        tensorLoad2.io.start := true.B
        state := sReadTensor2
      }
    }
    is(sReadTensor2) {
      when(tensorLoad2.io.done) {
        state := sTransferTensor
      }
    }
    is(sTransferTensor) {
      when(indexCnt.value === ts_Inst.xsize) {
        tensorStore.io.start := true.B
        indexCnt.value := 0.U
        state := sWriteTensor
      }.otherwise {
        indexCnt.inc()
      }
    }
    is(sWriteTensor) {
      when(tensorStore.io.done) {
        state := sIdle //sFinish
      }
    }
  }

  when(tensorStore.io.vme_wr.ack && state === sWriteTensor) {
    state := sIdle
  }

  when(state === sTransferTensor) {
    tensorStore.io.tensor.wr.valid := true.B
  }

  val last = state === sWriteTensor && tensorStore.io.vme_wr.ack
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
