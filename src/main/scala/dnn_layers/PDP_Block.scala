
package dnn_layers

import chisel3._
import chisel3.util._
import config._
import dnn.memory._
import dnn.types.{OperatorDot, OperatorReduction}
import dnnnode.{Mac1D, MacPW, PWShapeTransformer, ShapeTransformer}
import interfaces.{ControlBundle, CustomDataBundle}
import node.{Shapes, vecN}
import shell._
//import vta.util.config._


/** TensorLoad.
  *
  * Load 1D and 2D tensors from main memory (DRAM) to input/weight
  * scratchpads (SRAM). Also, there is support for zero padding, while
  * doing the load. Zero-padding works on the y and x axis, and it is
  * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
  * handling the way tensors are stored on the scratchpads.
  */
class PDP_BlockIO[gen <: vecN, gen2 <: Shapes]
(Hx: Int, K:Int, Fx: Int, Px: Int, wgtType: String = "none", memTensorType: String = "none")
(memShape: => gen)(CxShape: => gen2)(implicit val p: Parameters)
  extends Module {
  val tpMem = new TensorParams(memTensorType)
  val tpWgt = new TensorParams(wgtType)

  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val inBaseAddr = Input(UInt(mp.addrBits.W))
    val outBaseAddr = Input(UInt(mp.addrBits.W))


    val rowWidth = Input(UInt(mp.addrBits.W))

    val vme_rd = Vec(Hx, new VMEReadMaster)
    val vme_wr = Vec(Px * (Hx - K + 1), new VMEWriteMaster)

    val wgtPW1index = Input(UInt(tpWgt.memAddrBits.W))
    val wgtDWindex = Input(UInt(tpWgt.memAddrBits.W))
    val wgtPW2index = Input(UInt(tpWgt.memAddrBits.W))
    val vme_wgtPW1_rd = new VMEReadMaster
    val vme_wgtDW_rd = new VMEReadMaster
    val vme_wgtPW2_rd = new VMEReadMaster

    val wgtPW1_baddr = Input(UInt(mp.addrBits.W))
    val wgtDW_baddr = Input(UInt(mp.addrBits.W))
    val wgtPW2_baddr = Input(UInt(mp.addrBits.W))
  })
}

class PDP_Block[L <: vecN, K <: Shapes : OperatorDot : OperatorReduction, M <: Shapes : OperatorDot : OperatorReduction]
(Hx: Int, K: Int, Fx: Int, ChBatch: Int, Px: Int,
 intWgtPW1Type: String = "none",
 intWgtDWType: String = "none",
 intWgtPW2Type: String = "none",
 memTensorType: String = "none")
  (memShape: => L)(CxShape: => K)
  (wgtDWshape: => L)(macDWshape: => M)
  (wgtPW2shape: => L)(macPW2shape: => K)(implicit p: Parameters)
  extends PDP_BlockIO(Hx, K, Fx, Px, intWgtPW1Type, memTensorType)(memShape)(CxShape)(p) {

  val MACperCH = Hx - K + 1

  val inAct =  Module(new inDMA_act_HWC(Hx, 1, memTensorType)(memShape))

  val PW1ShTran = Module(new PWShapeTransformer(Hx, Fx, 20, memTensorType)(CxShape))

  val macPW1 = for (i <- 0 until Fx) yield {
    val mac1d = Module(new Mac1D(Hx, ChBatch, intWgtPW1Type)(CxShape))
    mac1d
  }

  val mac1DshapeOut = new vecN(1, 0, false)

  val DWShTran = for (i <- 0 until Fx) yield {
    for (j <- 0 until Hx - K + 1) yield {
      val shapeTran = Module(new ShapeTransformer(NumIns = K, NumOuts = 1, ID = 0)(mac1DshapeOut)(macDWshape))
      shapeTran
    }
  }

  val macDW = for (i <- 0 until Fx) yield {
    val macDW = Module(new MacPW(Hx - K + 1, intWgtPW1Type)(wgtDWshape)(macDWshape))
    macDW
  }

  val macPW2 = for (i <- 0 until Px) yield {
    val mac2dPW = Module(new MacPW(Hx - K + 1, intWgtPW2Type)(wgtPW2shape)(macPW2shape))
    mac2dPW
  }

  val outAct = for (i <- 0 until Px) yield {
    val outDMA = Module(new outDMA_act(Hx - K + 1, 20, "out"))
    outDMA
  }

  val doneR = for (i <- 0 until Px) yield {
    val doneReg = RegInit(init = false.B)
    doneReg
  }

  val readTensorCnt = Counter(tpMem.memDepth)

  val sIdle :: sWgtRead :: sActRead :: sMacLoadWgt :: sExec :: Nil = Enum(5)
  val state = RegInit(sIdle)

  /* ================================================================== *
   *                     Point-wise1 - inDMA_weight                     *
   * ================================================================== */

  val inWgtP1 = Module(new inDMA_wgt(20, 100, intWgtPW1Type, "extWgtPW1")(CxShape))
  val wgtCtrlPW1 = Module(new ReadTensorController(Fx, intWgtPW1Type)(CxShape))
  inWgtP1.io.tensor <> wgtCtrlPW1.io.tensor
  io.vme_wgtPW1_rd <> inWgtP1.io.vme_rd

  inWgtP1.io.numWeight := 15.U
  inWgtP1.io.start := false.B
  inWgtP1.io.baddr := io.wgtPW1_baddr

  for (i <- 0 until Fx) {
    wgtCtrlPW1.io.ReadIn(i) <> macPW1(i).io.wgtTensorReq
    macPW1(i).io.wgtTensorResp <> wgtCtrlPW1.io.ReadOut(i)
  }

  /* ================================================================== *
   *                     Depth-wise - inDMA_weight                      *
   * ================================================================== */

  val inWgtD = Module(new inDMA_wgt(20, 100, intWgtDWType, "extWgtDW")(wgtDWshape))
  val wgtCtrlDW = Module(new ReadTensorController(Fx, intWgtDWType)(wgtDWshape))
  inWgtD.io.tensor <> wgtCtrlDW.io.tensor
  io.vme_wgtDW_rd <> inWgtD.io.vme_rd

  inWgtD.io.numWeight := 7.U
  inWgtD.io.start := false.B
  inWgtD.io.baddr := io.wgtDW_baddr
  inWgtD.io.start := io.start

  for (i <- 0 until Fx) {
    wgtCtrlDW.io.ReadIn(i) <> macDW(i).io.wgtTensorReq
    macDW(i).io.wgtTensorResp <> wgtCtrlDW.io.ReadOut(i)
  }

  /* ================================================================== *
     *                     Point-wise2 - inDMA_weight                   *
     * ================================================================== */
  val inWgtP2 = Module(new inDMA_wgt(20, 100, intWgtPW2Type, "extWgtPW2")(wgtPW2shape))
  val wgtCtrlPW2 = Module(new ReadTensorController(Px, intWgtPW2Type)(wgtPW2shape))
  inWgtP2.io.tensor <> wgtCtrlPW2.io.tensor
  io.vme_wgtPW2_rd <> inWgtP2.io.vme_rd

  inWgtP2.io.numWeight := 7.U
  inWgtP2.io.start := false.B
  inWgtP2.io.baddr := io.wgtPW2_baddr
  inWgtP2.io.start := inWgtD.io.done

  for (i <- 0 until Px) {
    wgtCtrlPW2.io.ReadIn(i) <> macPW2(i).io.wgtTensorReq
    macPW2(i).io.wgtTensorResp <> wgtCtrlPW2.io.ReadOut(i)
  }

  /* ================================================================== *
    *                   inDMA_acts & PW1ShapeTransformer                 *
    * ================================================================== */

  inAct.io.start := io.start
  inAct.io.rowWidth := io.rowWidth
  inAct.io.depth := CxShape.getLength().U * ChBatch.U
  inAct.io.baddr := io.inBaseAddr

  PW1ShTran.io.start := inAct.io.done
  PW1ShTran.io.rowWidth := io.rowWidth
  PW1ShTran.io.depth := CxShape.getLength().U * ChBatch.U
  for (i <- 0 until Hx) {
    inAct.io.tensor(i) <> PW1ShTran.io.tensor(i)
    io.vme_rd(i) <> inAct.io.vme_rd(i)
  }

  /* ================================================================== *
    *                  Depth-wise MACs & inDMA_acts                     *
    * ================================================================== */
  for (i <- 0 until Fx) {
    macDW(i).io.enable.bits <> ControlBundle.active()
    macDW(i).io.enable.valid := true.B
    macDW(i).io.wgtIndex := io.wgtDWindex + i.U
    macDW(i).io.outRowWidth := io.rowWidth - macDWshape.getLength().U + 1.U
    macDW(i).io.start := inWgtD.io.done

    for (j <- 0 until MACperCH) {
      DWShTran(i)(j).io.enable.bits <> ControlBundle.active()
      DWShTran(i)(j).io.enable.valid := true.B

      macDW(i).io.in(j) <> DWShTran(i)(j).io.Out(0)
      macDW(i).io.Out(j).ready := macPW2.map(_.io.in(j).ready).reduceLeft(_ && _)

      for (k <- 0 until macDWshape.getLength()){

        DWShTran(i)(j).io.in(k) <> macPW1(i).io.Out(j + k)
      }

    }
  }

  /* ================================================================== *
     *                   Point-wise2 MACs & outDMA_acts                  *
     * ================================================================== */
  for (i <- 0 until Px) {
    macPW2(i).io.enable.bits <> ControlBundle.active()
    macPW2(i).io.enable.valid := true.B
    macPW2(i).io.wgtIndex := io.wgtPW2index + i.U
    macPW2(i).io.outRowWidth :=  io.rowWidth - macDWshape.getLength().U + 1.U
    macPW2(i).io.start := inWgtP2.io.done

    outAct(i).io.rowWidth :=  io.rowWidth - macDWshape.getLength().U + 1.U
    outAct(i).io.baddr := io.outBaseAddr + (i.U * (Hx.U * ( io.rowWidth - macDWshape.getLength().U + 1.U)))

    for (j <- 0 until MACperCH) {
      outAct(i).io.in(j) <> macPW2(i).io.Out(j)
      io.vme_wr(i * MACperCH + j) <> outAct(i).io.vme_wr(j)

      macPW2(i).io.in(j).bits.data := VecInit(macDW.map(_.io.Out(j).bits.data.asUInt())).asUInt()
      macPW2(i).io.in(j).bits.taskID := 0.U
      macPW2(i).io.in(j).bits.predicate := true.B
      macPW2(i).io.in(j).bits.valid := true.B
      macPW2(i).io.in(j).valid := macDW.map(_.io.Out(j).valid).reduceLeft(_ && _)


    }
    outAct(i).io.last.foreach(a => a := macPW2(i).io.last)
    outAct(i).io.start := macPW2(i).io.done
    when(outAct(i).io.done) {
      doneR(i) := true.B
    }
  }

  /* ================================================================== *
    *                        loadNodes & mac1Ds                         *
    * ================================================================== */

  for (i <- 0 until Fx) {
    macPW1(i).io.enable.bits <> ControlBundle.active()
    macPW1(i).io.enable.valid := true.B
    macPW1(i).io.wgtIndex := io.wgtPW1index + (i * ChBatch).U
    macPW1(i).io.rowWidth := io.rowWidth

    for (j <- 0 until Hx) {
      macPW1(i).io.in(j) <> PW1ShTran.io.Out(j)(i)

    }
  }

  macPW1.foreach(_.io.startLoadWgt := false.B)
  macPW1.foreach(_.io.startMac := false.B)

  /* ================================================================== *
      *                        Done Signal                              *
      * ================================================================== */

  io.done := false.B
  when (doneR.reduceLeft(_ && _)) {
    doneR.foreach(a => a := false.B)
  }

  val memTensorRows = Mux(io.rowWidth * ChBatch.U * CxShape.getLength().U  % tpMem.tensorWidth.U === 0.U,
    io.rowWidth * ChBatch.U * CxShape.getLength().U / tpMem.tensorWidth.U,
    (io.rowWidth * ChBatch.U * CxShape.getLength().U /tpMem.tensorWidth.U) + 1.U)

  when(macPW1.map(_.io.in.map(_.fire()).reduceLeft(_ && _)).reduceLeft(_ && _) & state === sExec){
    readTensorCnt.inc()
  }

  when(readTensorCnt.value === memTensorRows) {
    readTensorCnt.value := 0.U
  }


  switch(state) {
    is(sIdle) {
      when(io.start) {
        inWgtP1.io.start := true.B
        state := sWgtRead
      }
    }
    is(sWgtRead) {
      when(inWgtP1.io.done) {
        state := sActRead
        inAct.io.start := true.B
        macPW1.foreach(_.io.startLoadWgt := true.B)
      }
    }
    is(sActRead) {
      when(inAct.io.done){
        state := sMacLoadWgt
      }
    }
    is(sMacLoadWgt){
      when(macPW1.map(_.io.doneLoadWgt).reduceLeft(_ && _)){
        macPW1.foreach(_.io.startMac := true.B)
        state := sExec
      }
    }
    is(sExec){
      when(doneR.reduceLeft(_ && _)) {
        io.done := true.B
        state := sIdle
      }

    }
  }

}
