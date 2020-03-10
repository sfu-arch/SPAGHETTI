package dnn

import chisel3.util.{Decoupled, Enum, Valid}
import chisel3.{Bundle, Flipped, Module, Output, RegInit, UInt, printf, when, _}
import config.{Parameters, XLEN}
import dnn.types.{OperatorCooSCAL, OperatorSCAL}
import interfaces.{CooDataBundle, CustomDataBundle}
//import javafx.scene.chart.PieChart.Data
import node.{HandShakingIONPS, HandShakingNPS, Shapes}

class CooSCALFU[L <: Shapes : OperatorCooSCAL](left: => L, lanes: Int, opCode: String)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val a = Flipped(Valid(left))
    val b = Flipped(Valid(UInt(p(XLEN).W)))
    val o = Output(Valid(left))
  })


  val start = io.a.valid && io.b.valid
  val FU    = OperatorCooSCAL.magic(io.a.bits, io.b.bits, start, lanes, opCode)
  io.o.bits := FU._1
  val latency = FU._2
  io.o.valid := FU._2
}

class CooSCALIO[L <: Shapes](N: Int)(left: => L)(implicit val p: Parameters) extends Module{
  val io = IO(new Bundle() {
    val vec = Vec(N, Flipped(Decoupled(new CooDataBundle(UInt(left.getWidth.W)))))

    val scal = Flipped(Decoupled(new CooDataBundle(UInt(p(XLEN).W))))

    val out = Vec(N, Decoupled(new CooDataBundle(UInt(p(XLEN).W))))
  })
}

class CooSCALNode[L <: Shapes : OperatorCooSCAL](N: Int, ID: Int, opCode: String)(left: => L)(implicit p: Parameters)
  extends CooSCALIO(N = N)(left)(p) {

  require(left.getLength() == N, "shape does not match with number of multipliers")
  /*===========================================*
 *            Registers                      *
 *===========================================*/

  /*==========================================*
   *           Predicate Evaluation           *
   *==========================================*/


  /*===============================================*
   *            Latch inputs. Wire up left       *
   *===============================================*/
  val FU = Module(new CooSCALFU(left, lanes = N, opCode))
  FU.io.a.bits := VecInit(io.vec.map(_.bits.data.asUInt())).asTypeOf(left)
  FU.io.b.bits := io.scal.bits.data

  FU.io.a.valid := io.vec.map(_.valid).reduceLeft(_&&_)
  FU.io.b.valid := io.scal.valid


  for (i <- 0 until N) {
    io.out(i).bits.data := FU.io.o.bits
    io.Out(i).bits.valid := data_R.valid
    io.Out(i).bits.predicate := predicate
    io.Out(i).bits.taskID := left_R.taskID | right_R.taskID | enable_R.taskID
  }

  /*============================================*
 *            ACTIONS (possibly dangerous)    *
 *============================================*/

   //  This is written like this to enable FUs that are dangerous in the future.
  // If you don't start up then no value passed into function

  when(state === s_ACTIVE) {
    when(FU.io.o.valid) {
      ValidOut( )
      data_R.data := (FU.io.o.bits).asTypeOf(UInt(left.getWidth.W))
      data_R.valid := true.B
      state := s_COMPUTE
    }.otherwise {
      state := s_ACTIVE
    }
  }
  when((IsOutReady( )) && (state === s_COMPUTE)) {
    left_R := CustomDataBundle.default(0.U((left.getWidth).W))
    right_R := CustomDataBundle.default(0.U((left.getWidth).W))
    data_R := CustomDataBundle.default(0.U((left.getWidth).W))
    Reset( )
    state := s_idle
  }


  var classname: String = (left.getClass).toString
  var signed            = "S"
  override val printfSigil =
    opCode + "[" + classname.replaceAll("class node.", "") + "]_" + ID + ":"

  if (log == true && (comp contains "TYPOP")) {
    val x = RegInit(0.U(xlen.W))
    x := x + 1.U

    verb match {
      case "high" => {
      }
      case "med" => {
      }
      case "low" => {
        printfInfo("Cycle %d : { \"Inputs\": {\"Left\": %x, \"Right\": %x},", x, (left_R.valid), (right_R.valid))
        printf("\"State\": {\"State\": \"%x\", \"(L,R)\": \"%x,%x\",  \"O(V,D,P)\": \"%x,%x,%x\" },",
          state, left_R.data, right_R.data, io.Out(0).valid, data_R.data, io.Out(0).bits.predicate)
        printf("\"Outputs\": {\"Out\": %x}", io.Out(0).fire( ))
        printf("}")
      }
      case everythingElse => {
      }
    }
  }
}


