package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}

case class ORGATEParams(
  width: Int,
  number: Int
)

class ORGATEIO(params: ORGATEParams) extends Bundle {
  val in                                        = Input(Vec(params.number, UInt(params.width.W)))
  val out                                       = Output(UInt(params.width.W))
}

trait HasORGATEIO extends BaseModule {
  val params: ORGATEParams
  val io = IO(new ORGATEIO(params))
}

class GH_ORGATE (val params: ORGATEParams) extends Module with HasORGATEIO {
  io.out                                       := io.in.reduce(_|_)
}

