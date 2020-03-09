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

package accel

import chisel3._
import chisel3.MultiIOModule
import vta.dpi._
import shell._
import vta.shell._
import shell.De10Config
import config._
import accel._
import chisel3.util._
import dnn.memory._



/** Test. This generates a testbench file for simulation */
class TestAccel2(implicit p: Parameters) extends MultiIOModule {
  val sim_clock = IO(Input(Clock()))
  val sim_wait = IO(Output(Bool()))
  val sim_shell = Module(new AXISimShell)
  val vta_shell = Module(new DNNAccel())
  sim_shell.sim_clock := sim_clock
  sim_wait := sim_shell.sim_wait

  sim_shell.mem.ar <> vta_shell.io.mem.ar
  sim_shell.mem.aw <> vta_shell.io.mem.aw
  vta_shell.io.mem.r <> sim_shell.mem.r
  vta_shell.io.mem.b <> sim_shell.mem.b
  sim_shell.mem.w <> vta_shell.io.mem.w



  vta_shell.io.host.ar <> sim_shell.host.ar
  vta_shell.io.host.aw <> sim_shell.host.aw
  sim_shell.host.r <> vta_shell.io.host.r
  sim_shell.host.b <> vta_shell.io.host.b
  vta_shell.io.host.w <> sim_shell.host.w

// vta_shell.io.host <> sim_shell.host
}

class DefaultDe10Config extends Config(new De10Config ++ new CoreConfig ++ new MiniConfig)
class  DefaultPynqConfig extends Config(new PynqConfig ++ new CoreConfig ++ new MiniConfig)

object TestXilinxShellMain extends App {
  implicit val p: Parameters = new DefaultPynqConfig
  chisel3.Driver.execute(args, () => new XilinxShell())
}
object TestVTAShell2Main extends App {
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new NoneAccel())
}

object TestAccel2Main extends App {
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new TestAccel2)
}

object DNNAccelMain extends App {
//  implicit val p: Parameters = new DefaultDe10Config ++ new Con
//  implicit val p = new shell.DefaultDe10Config ++ Parameters.root((new MiniConfig).toInstance)
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new DNNAccel())
}

