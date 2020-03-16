
package tensorKernels

import chisel3.util.{Decoupled}
import chisel3.{Flipped, Module, UInt, _}
import config.{Parameters, XLEN}
import interfaces.CooDataBundle

class AdderIO(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val eopIn = Input(Bool( ))
    val lastIn = Input(Bool( ))
    val in = Flipped(Decoupled(new CooDataBundle(UInt(p(XLEN).W))))
    val out = Decoupled(new CooDataBundle(UInt(p(XLEN).W)))
    val eopOut = Output(Bool( ))
    val lastOut = Output(Bool( ))
  })
}

class Adder(ID: Int)(implicit p: Parameters)
  extends AdderIO()(p) {

  /*===============================================*
   *                Connections                    *
   *===============================================*/
  io.eopOut := RegNext(io.eopIn)
  io.lastOut := RegNext(io.lastIn)

  val data = RegInit(CooDataBundle.default(0.U(p(XLEN).W)))

  when(io.in.valid){
    when(data.row =/= io.in.bits.row || data.col =/= io.in.bits.col) {
      data <> io.in.bits
    }.elsewhen(data.row === io.in.bits.row && data.col === io.in.bits.col){
      data.data := data.data + io.in.bits.data
      data.row := io.in.bits.row
      data.col := io.in.bits.col
      data.valid := io.in.bits.valid
    }
  }

  io.out.bits := data
  io.out.valid := RegNext(io.in.valid) && !(data.row === io.in.bits.row && data.col === io.in.bits.col)
  io.in.ready := io.out.ready
}