package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}
import chisel3.stage.ChiselStage


//==========================================================
// Parameters
//==========================================================
case class GHE_HAPMC_Params (
  xlen: Int
)

//==========================================================
// I/Os
//==========================================================
class GHE_HAPMC_IO (params: GHE_HAPMC_Params) extends Bundle {
  val ghe_hapmc_active = Input(UInt(1.W))
  val ghe_hapmc_hbound = Input(UInt(params.xlen.W))
  val ghe_hapmc_lbound = Input(UInt(params.xlen.W))

  val msgq_empty = Input(UInt(1.W))
  val msgq_data = Input(UInt(params.xlen.W))
  
  val msgq_pop = Output(UInt(1.W))
  val ghe_hapmc_rslt = Output(UInt(params.xlen.W))
}

trait HasGHE_HAPMC_IO extends BaseModule {
  val params: GHE_HAPMC_Params
  val io = IO(new GHE_HAPMC_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHE_HAPMC (val params: GHE_HAPMC_Params) extends Module with HasGHE_HAPMC_IO
{
  val pmc_reg = RegInit(0.U(params.xlen.W))
  when (io.msgq_empty =/= 1.U && io.ghe_hapmc_active === 1.U){
    io.msgq_pop := 1.U
    pmc_reg := Mux((io.msgq_data > io.ghe_hapmc_lbound && io.msgq_data < io.ghe_hapmc_hbound), pmc_reg + 1.U, pmc_reg)
  } .otherwise {
    io.msgq_pop := 0.U
    pmc_reg := Mux((io.ghe_hapmc_active === 0.U), 0.U, pmc_reg)
  }

  io.ghe_hapmc_rslt := pmc_reg
}
