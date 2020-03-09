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

import FPU.FType
import chisel3.util._
import chisel3.{when, _}
import config._
import dnn_layers.PW_Block
import node.{FPvecN, matNxN, vecN}
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
class TensorCore(implicit val p: Parameters) extends Module {
  val Hx = 5  //  Number of Rows
  val Wx = 7
  val Fx = 2
  val Px = 10
  val K = 3
  val Cx = 6
  val Cb = 2
  // C = Cx * Cb
  val io = IO(new Bundle {
    val vcr = new VCRClient
    val vme = new VMEMaster
  })

  // nRd = Hx + 3 = 6
  // nWr = Px * (Hx - K + 1) = 10 * 1 = 10
  val cycle_count = new Counter(2000)

//  val NumChannel = 3

  val S = new FType(8, 24)

//  val memShape = new vecN(16, 0, false)
  val memShape = new FPvecN(16, S, 0)

  val macDWShape = new matNxN(K, false)
  val macPW2Shape = new vecN(Fx, 0, false)

  val wgtDWShape = new vecN(K * K, 0, false)
  val wgtPW2Shape = new vecN(Fx, 0, false)

//  val CxShape = new vecN(Cx, 0, false)
  val CxShape = new FPvecN(Cx, S, 0)

//  val DW_B1 = Module(new DW_Block(3, "wgt", "inp")(memShape)(wgtDWShape)(macDWShape))

//  val conv = Module(new DW_PW_Block(NumChannel, MACperCH, NumPWFilter, "wgt", "wgtPW", "inp")
//                   (memShape)(wgtDWShape)(wgtPWShape)(macDWShape)(macPWShape))

  val conv = Module(new PW_Block(Hx, Fx, Cb, "intWgtPW1", "inp")(memShape)(CxShape))
//  val conv = Module(new PDP_Block(Hx, K, Fx, 19, Px,
//                    "intWgtPW1", "intWgtDW", "intWgtPW2", "inp")
//                    (memShape)(CxShape)
//                    (wgtDWShape)(macDWShape)
//                    (wgtPW2Shape)(macPW2Shape))

  /* ================================================================== *
     *                      Basic Block signals                         *
     * ================================================================== */
  conv.io.wgtIndex := 0.U

  conv.io.rowWidth := Wx.U //3.U

  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */

  io.vcr.ecnt(0).bits := cycle_count.value

  io.vcr.ecnt(1).bits := conv.io.inDMA_act_time
  io.vcr.ecnt(2).bits := conv.io.inDMA_wgt_time
  io.vcr.ecnt(3).bits := conv.io.mac_time

  /* ================================================================== *
    *                    VME Reads and writes                           *
    * ================================================================== */

  for (i <- 0 until Hx) {
    io.vme.rd(i) <> conv.io.vme_rd(i)
  }
  io.vme.rd(Hx) <> conv.io.vme_wgt_rd

  for (i <- 0 until Fx * Hx) {
    io.vme.wr(i) <> conv.io.vme_wr(i)
  }


  conv.io.start := false.B

  conv.io.inBaseAddr := io.vcr.ptrs(0)

  conv.io.wgt_baddr := io.vcr.ptrs(1)

  conv.io.outBaseAddr := io.vcr.ptrs(2)

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)

  val state = RegInit(sIdle)
  switch(state) {
    is(sIdle) {
      when(io.vcr.launch) {
        conv.io.start := true.B
        state := sExec
      }
    }
    is(sExec) {
      when(conv.io.done) {
        state := sIdle
      }
    }
  }

  val last = state === sExec && conv.io.done
  io.vcr.finish := last
  io.vcr.ecnt(0).valid := last
  io.vcr.ecnt(1).valid := last
  io.vcr.ecnt(2).valid := last
  io.vcr.ecnt(3).valid := last

  when(state =/= sIdle) {
    cycle_count.inc()
  }
}
