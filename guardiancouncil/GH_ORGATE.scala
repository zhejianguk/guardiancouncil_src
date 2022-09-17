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
  val out_wire                                  = WireInit (0.U (params.width.W))

  /* This method reports a combinational loop, which is expected.
     We use the mannual method to avoid masking the above error
  */ 

  /* for (i <- 0 to params.number-1) {
      out_wire                                 := out_wire | io.in(i)
    }
  */

  io.out                                       := io.in(0) |
                                                  io.in(1) |
                                                  io.in(2) |
                                                  io.in(3) |
                                                  io.in(4) |
                                                  io.in(5) |
                                                  io.in(6) |
                                                  io.in(7)
}

