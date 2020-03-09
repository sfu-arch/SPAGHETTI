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
import dnn.memory.{ReadTensorController, inDMA_wgt}
import dnn_layers.DW_Block
import node.{matNxN, vecN}
import shell._

/** Core.
  *
  * The DNNcore defines the current DNN accelerator by connecting memory and
  * compute modules together such as load/store and compute. Most of the
  * connections in the core are bulk (<>), and we should try to keep it this
  * way, because it is easier to understand what is going on.
  *
  * Also, the DNNcore must be instantiated by a shell using the
  * VTA Control Register (VCR) and the VTA Memory Engine (VME) interfaces.
  * More info about these interfaces and modules can be found in the shell
  * directory.
  */
class DNNCoreDW(implicit val p: Parameters) extends Module {
  val Hx = 4  //  Number of Rows
  val Wx = 3
  val Fx = 2
  val Px = 2
  val K = 3
  val Cx = 5
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  val cycle_count = new Counter(2000)

  val memShape = new vecN(16, 0, false)
  val macShape = new matNxN(3, false)
  val wgtShape = new vecN(9, 0, false)

  val inDMA_wgt = Module(new inDMA_wgt(20, 100, intWgtTensorType = "wgt", extWgtTensorType = "inp")(wgtShape))
  val readTensorController2 = Module(new ReadTensorController(1, "wgt")(wgtShape))

  val DW_B1 = Module(new DW_Block(3, "wgt", "inp")(memShape)(wgtShape)(macShape))

  /* ================================================================== *
     *                      Basic Block signals                         *
     * ================================================================== */
  inDMA_wgt.io.numWeight := 7.U
  DW_B1.io.wgtIndex := 1.U
  DW_B1.io.rowWidth := 15.U
  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */

  readTensorController2.io.ReadIn(0) <> DW_B1.io.wgtTensorReq
  DW_B1.io.wgtTensorResp <> readTensorController2.io.ReadOut(0)
  inDMA_wgt.io.tensor <> readTensorController2.io.tensor

  /* ================================================================== *
    *                      VME and VCR Connections                      *
    * ================================================================== */
  io.vcr.ecnt(0).bits := cycle_count.value

  io.vme.rd(0) <> DW_B1.io.vme_rd(0)
  io.vme.rd(1) <> DW_B1.io.vme_rd(1)
  io.vme.rd(2) <> DW_B1.io.vme_rd(2)
  io.vme.rd(3) <> DW_B1.io.vme_rd(3)
  io.vme.rd(4) <> DW_B1.io.vme_rd(4)

  io.vme.rd(5) <> inDMA_wgt.io.vme_rd

  io.vme.wr(0) <> DW_B1.io.vme_wr(0)
  io.vme.wr(1) <> DW_B1.io.vme_wr(1)
  io.vme.wr(2) <> DW_B1.io.vme_wr(2)

  DW_B1.io.start := false.B

  DW_B1.io.inBaseAddr := io.vcr.ptrs(0)
  DW_B1.io.outBaseAddr := io.vcr.ptrs(2)

  inDMA_wgt.io.start := false.B
  inDMA_wgt.io.baddr := io.vcr.ptrs(1)

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)

  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        inDMA_wgt.io.start := true.B
        state := sExec
      }
    }
    is(sExec) {
      when(inDMA_wgt.io.done) {
        DW_B1.io.start := true.B
        state := sFinish
      }
    }
  }

  val last = state === sFinish && DW_B1.io.done
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
