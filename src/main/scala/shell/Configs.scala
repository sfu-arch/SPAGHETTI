package shell


import chisel3.Module
import config._
import junctions._


class VCRSimParams(val num_ptrs: Int = 9, val num_vals: Int = 2,
                   val num_event: Int = 1, val num_ctrl: Int = 1) extends VCRParams {
  override val nCtrl = num_ctrl
  override val nECnt = num_event
  override val nVals = num_vals
  override val nPtrs = num_ptrs
  override val regBits = 32
  val ptrBits = regBits
  //val ptrBits = 2 * regBits
}

/** VME parameters.
  *
  * These parameters are used on VME interfaces and modules.
  */
class VMESimParams() extends VMEParams {
  override val nReadClients: Int = 6
  override val nWriteClients: Int = 3
  require(nReadClients > 0,
    s"\n\n[Dandelion] [VMEParams] nReadClients must be larger than 0\n\n")
  require(
    nWriteClients > 0,
    s"\n\n[Dandelion] [VMEParams] nWriteClients must be larger than 0\n\n")
}


class TensorBrickParams() {
  val Hx = 3  //  Number of Rows
  val Wx = 3
  val Cx = 2
  val Cb = 2
  // C = Cx * Cb
  val Fx = 1
  val Px = 10
  val K = 3
}


/** De10Config. Shell configuration for De10 */
class De10Config (val num_ptrs: Int = 9, val num_vals: Int = 2, val num_event: Int = 4, val num_ctrl: Int = 1)extends Config((site, here, up) => {
  case ShellKey => ShellParams(
    hostParams = AXIParams(
      addrBits = 16, dataBits = 32, idBits = 13, lenBits = 4),
    memParams = AXIParams(
      addrBits = 32, dataBits = 64, userBits = 5,
      lenBits = 4, // limit to 16 beats, instead of 256 beats in AXI4
      coherent = true),
//    vcrParams = VCRParams( ),
//    vmeParams = VMEParams( ))
    vcrParams = new VCRSimParams(num_ptrs, num_vals, num_event, num_ctrl),
    vmeParams = new VMESimParams(),
    tensorBrickParams = new TensorBrickParams())
})


/** PynqConfig. Shell configuration for Pynq */
class PynqConfig (val num_ptrs: Int = 9, val num_vals: Int = 2, val num_event: Int = 4, val num_ctrl: Int = 1)extends Config((site, here, up) => {
  case ShellKey => ShellParams(
    hostParams = AXIParams(
      coherent = false,
      addrBits = 16,
      dataBits = 32,
      lenBits = 8,
      userBits = 1),
    memParams = AXIParams(
      coherent = true,
      addrBits = 32,
      dataBits = 64,
      lenBits = 8,
      userBits = 1),
//    vcrParams = VCRSParams( ),
//    vmeParams = VMEParams( ))
    vcrParams = new VCRSimParams(num_ptrs, num_vals, num_event, num_ctrl),
    vmeParams = new VMESimParams(),
    tensorBrickParams = new TensorBrickParams())
})


class DefaultDe10Config extends Config(new MiniConfig ++ new De10Config)


object DefaultDe10Config extends App {
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new IntelShell)
}