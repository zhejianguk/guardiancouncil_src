package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}

case class ANDGATEParams(
  width: Int,
  number: Int
)

class ANDGATEIO(params: ANDGATEParams) extends Bundle {
  val in                                        = Input(Vec(params.number, UInt(params.width.W)))
  val out                                       = Output(UInt(params.width.W))
}

trait HasANDGATEIO extends BaseModule {
  val params: ANDGATEParams
  val io = IO(new ANDGATEIO(params))
}

class GH_ANDGATE (val params: ANDGATEParams) extends Module with HasANDGATEIO {
  io.out                                       := io.in.reduce(_&_)
}

