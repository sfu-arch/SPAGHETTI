
package dnnnode

import chisel3._
import chisel3.util._
import config._
import dnn.MacNode
import dnn.memory.TensorParams
import dnn.types.{OperatorDot, OperatorReduction}
import interfaces.{ControlBundle, CustomDataBundle, DataBundle, TensorReadReq, TensorReadResp}
import node.{HandShakingIONPS, HandShakingNPS, Shapes, matNxN, vecN}
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
class Mac2dTensorIO[gen <: vecN, gen2 <: Shapes](NumMac: Int, wgtTensorType: String = "none", memTensorType: String = "none")
                                                (memShape: => gen)(wgtShape: => gen)(macShape: => gen2)(implicit p: Parameters)
  extends HandShakingIONPS(NumMac)(new CustomDataBundle(UInt(p(XLEN).W))) {
  val tpWgt = new TensorParams(wgtTensorType)
  val tpMem = new TensorParams(memTensorType)
  val mp = p(ShellKey).memParams
    val tensorReq = Vec(NumMac + macShape.getLength() - 1, Decoupled(new TensorReadReq()))
    val tensorResp = Vec(NumMac + macShape.getLength() - 1, Input(Flipped(new TensorReadResp(memShape.getWidth))))
    val wgtTensorReq = Decoupled(new TensorReadReq())
    val wgtTensorResp = Input(Flipped(new TensorReadResp(wgtShape.getWidth)))
    val wgtIndex = Input(UInt(tpWgt.memAddrBits.W))
    val outRowWidth = Input(UInt(mp.addrBits.W))
    val last = Output(Bool())
    val start = Input(Bool())
    val done = Output(Bool())

  override def cloneType = new Mac2dTensorIO(NumMac, wgtTensorType, memTensorType)(memShape)(wgtShape)(macShape).asInstanceOf[this.type]
}

class Mac2dTensor[L <: vecN, K <: Shapes : OperatorDot : OperatorReduction](NumMac: Int, wgtTensorType: String = "none", memTensorType: String = "none")
                                                                           (memShape: => L)(wgtShape: => L)(macShape: => K)
                                                                           (implicit p: Parameters)
  extends HandShakingNPS(NumMac, 0)(new CustomDataBundle(UInt(p(XLEN).W)))(p) {
  override lazy val io = IO(new Mac2dTensorIO(NumMac, wgtTensorType, memTensorType)(memShape)(wgtShape)(macShape))

  val readCnt = Counter(io.tpWgt.memDepth)
  val outCnt = Counter(io.tpWgt.memDepth)
  io.done := false.B
  io.last := false.B

  val loadWeight = Module(new TLoad(NumPredOps = 0, NumSuccOps = 0, NumOuts = 1, ID = 0, RouteID = 0)(wgtShape))
  val weight = RegInit(CustomDataBundle.default(0.U(wgtShape.getWidth.W)))
  val weight_valid = RegInit(false.B)

  loadWeight.io.enable.bits <> ControlBundle.active()
  loadWeight.io.enable.valid := true.B
  io.wgtTensorReq <> loadWeight.io.tensorReq
  loadWeight.io.tensorResp <> io.wgtTensorResp
  loadWeight.io.GepAddr.valid := false.B
  loadWeight.io.GepAddr.bits.taskID := 0.U
  loadWeight.io.GepAddr.bits.predicate := true.B
  loadWeight.io.GepAddr.bits.data := io.wgtIndex

  loadWeight.io.Out(0).ready := ~weight_valid
  when(loadWeight.io.Out(0).fire()) {
    weight := loadWeight.io.Out(0).bits
    weight_valid := true.B
  }


  val load = for (i <- 0 until NumMac + macShape.getLength() - 1) yield {
    val loadNode = Module(new TLoad(NumPredOps = 0, NumSuccOps = 0, NumOuts = 1, ID = 0, RouteID = 0)(memShape))
    loadNode
  }

  val mac = for (i <- 0 until NumMac) yield {
    val macNode = Module(new MacNode(NumOuts = 1, ID = 0, lanes = macShape.getLength())(macShape))
    macNode
  }

  val shapeTransformer = for (i <- 0 until NumMac) yield {
    val shapeTran = Module(new ShapeTransformer(NumIns = macShape.getLength(), NumOuts = 1, ID = 0)(memShape)(macShape))
    shapeTran
  }

  val sIdle :: sExec :: sFinish :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val memTensorRows = Mux(io.outRowWidth + macShape.getLength().U - 1.U % io.tpMem.tensorWidth.U === 0.U,
    (io.outRowWidth + macShape.getLength().U - 1.U) / io.tpMem.tensorWidth.U,
    ((io.outRowWidth + macShape.getLength().U - 1.U) /io.tpMem.tensorWidth.U) + 1.U)

  val readTensorCnt = Counter(io.tpWgt.memDepth)


  for (i <- 0 until NumMac + macShape.getLength() - 1)  {
    load(i).io.enable.bits <> ControlBundle.active()
    load(i).io.enable.valid := true.B
    io.tensorReq(i) <> load(i).io.tensorReq
    load(i).io.tensorResp <> io.tensorResp(i)
    load(i).io.GepAddr.valid := false.B
    load(i).io.GepAddr.bits.taskID := 0.U
    load(i).io.GepAddr.bits.predicate := true.B
    load(i).io.GepAddr.bits.data := readTensorCnt.value
  }


  for (i <- 0 until NumMac) {
    for (j <- 0 until macShape.getLength()) {
      shapeTransformer(i).io.in(j) <> load(i + j).io.Out(0)
    }

    val shapeTransformerReady = shapeTransformer.map(_.io.in.map(_.ready).reduceLeft(_ && _)).reduceLeft(_ && _)
    when (shapeTransformerReady && state === sExec && (readTensorCnt.value < memTensorRows) && load.map(_.io.GepAddr.ready).reduceLeft(_ && _)) {
      load.foreach(a => a.io.GepAddr.valid := true.B)
      readTensorCnt.inc()
    }
    when(readTensorCnt.value === memTensorRows && state === sFinish) {readTensorCnt.value := 0.U}

    shapeTransformer(i).io.enable.bits := ControlBundle.active()
    shapeTransformer(i).io.enable.valid := true.B

    mac(i).io.enable.bits <> ControlBundle.active()
    mac(i).io.enable.valid := true.B

//    mac(i).io.LeftIO <> shapeTransformer(i).io.Out(0)
    shapeTransformer(i).io.Out(0).ready := mac(i).io.LeftIO.ready & state === sExec
    mac(i).io.LeftIO.valid := shapeTransformer(i).io.Out(0).valid
    mac(i).io.LeftIO.bits := shapeTransformer(i).io.Out(0).bits

    mac(i).io.RightIO.bits := weight
    mac(i).io.RightIO.valid := weight_valid & state === sExec
    io.Out(i) <> mac(i).io.Out(0)
  }


  when (mac.map(_.io.Out(0).fire()).reduceLeft(_ && _)){
    outCnt.inc()
  }

  when (mac.map(_.io.LeftIO.fire()).reduceLeft(_ && _) & state === sExec){
    readCnt.inc()
  }

  switch(state) {
    is (sIdle) {
      when(io.start) {
        readTensorCnt.value := 0.U
        loadWeight.io.GepAddr.valid := true.B
        state := sExec
      }
    }
    is (sExec) {
        when (readCnt.value === io.outRowWidth) {
        state := sFinish
        readCnt.value := 0.U
      }
    }
    is (sFinish){
      when(outCnt.value === io.outRowWidth) {
        outCnt.value := 0.U
        weight_valid := false.B
        io.done := true.B
        io.last := true.B
        state := sIdle
      }

    }
  }

}
