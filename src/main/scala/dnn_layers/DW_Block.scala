
package dnn_layers

import chisel3._
import chisel3.util.Decoupled
import config._
import dnn.memory.{ReadTensorController, TensorParams, inDMA_act, inDMA_wgt, outDMA_act}
import dnn.types.{OperatorDot, OperatorReduction}
import dnnnode.Mac2dTensor
import interfaces.{ControlBundle, TensorReadReq, TensorReadResp}
import node.{Shapes, vecN}
import shell._
//import vta.util.config._
import dnn.memory.ISA._


/** TensorLoad.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class DW_BlockIO[gen <: vecN, gen2 <: Shapes]
                    (NumMac: Int, wgtTensorType: String = "none", memTensorType: String = "none")
                    (memShape: => gen)(wgtShape: => gen)(macShape: => gen2)(implicit val p: Parameters)
  extends Module {
  val tpWgt = new TensorParams(wgtTensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val inBaseAddr = Input(UInt(mp.addrBits.W))
    val outBaseAddr = Input(UInt(mp.addrBits.W))
    val rowWidth = Input(UInt(mp.addrBits.W))
    val wgtTensorReq = Decoupled(new TensorReadReq())
    val wgtTensorResp = Input(Flipped(new TensorReadResp(memShape.getWidth)))
    val wgtIndex = Input(UInt(tpWgt.memAddrBits.W))
    val vme_rd = Vec(NumMac + macShape.getLength() - 1, new VMEReadMaster)
    val vme_wr = Vec(NumMac, new VMEWriteMaster)
  })
}

class DW_Block[L <: vecN, K <: Shapes : OperatorDot : OperatorReduction]
                    (NumMac: Int, wgtTensorType: String = "none", memTensorType: String = "none")
                    (memShape: => L)(wgtShape: => L)(macShape: => K)(implicit p: Parameters)
  extends DW_BlockIO(NumMac, wgtTensorType, memTensorType)(memShape)(wgtShape)(macShape)(p) {


  val inDMA_act = Module(new inDMA_act(NumMac + macShape.getLength() - 1, 1, memTensorType)(memShape))

  val outDMA_act = Module(new outDMA_act(NumMac, 20, memTensorType))

  val mac2dTensor = Module(new Mac2dTensor(NumMac, wgtTensorType, memTensorType)(memShape)(wgtShape)(macShape))

  mac2dTensor.io.enable.bits <> ControlBundle.active()
  mac2dTensor.io.enable.valid := true.B

  mac2dTensor.io.wgtIndex := io.wgtIndex
  mac2dTensor.io.outRowWidth := io.rowWidth

  inDMA_act.io.rowWidth := io.rowWidth + macShape.getLength().U - 1.U
  inDMA_act.io.baddr := io.inBaseAddr

  outDMA_act.io.rowWidth := io.rowWidth
  outDMA_act.io.baddr := io.outBaseAddr
  /* ================================================================== *
     *                           Connections                            *
     * ================================================================== */
  for (i <- 0 until NumMac) {
    outDMA_act.io.in(i) <> mac2dTensor.io.Out(i)
    io.vme_wr(i) <> outDMA_act.io.vme_wr(i)
  }

  outDMA_act.io.last.foreach(a => a := mac2dTensor.io.last)

  for (i <- 0 until NumMac + macShape.getLength() - 1) {
    inDMA_act.io.ReadIn(i)(0) <> mac2dTensor.io.tensorReq(i)
    mac2dTensor.io.tensorResp(i) <> inDMA_act.io.ReadOut(i)(0)

    io.vme_rd(i) <> inDMA_act.io.vme_rd(i)
  }

  io.wgtTensorReq <> mac2dTensor.io.wgtTensorReq
  mac2dTensor.io.wgtTensorResp <> io.wgtTensorResp

  inDMA_act.io.start := io.start
  mac2dTensor.io.start := inDMA_act.io.done
  outDMA_act.io.start := mac2dTensor.io.done
  io.done := outDMA_act.io.done


}
