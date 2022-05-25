package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_CFG_Params(
  totalnumber_of_checkers: Int,
)

//==========================================================
// I/Os
//==========================================================
class GHT_CFG_IO (params: GHT_CFG_Params) extends Bundle {
  val pmc_core_s      = Output(UInt(log2Up(params.totalnumber_of_checkers).W))
  val pmc_core_e      = Output(UInt(log2Up(params.totalnumber_of_checkers).W))
  val pmc_inst_1      = Output(UInt(32.W))

  val sani_core_s     = Output(UInt(log2Up(params.totalnumber_of_checkers).W))
  val sani_core_e     = Output(UInt(log2Up(params.totalnumber_of_checkers).W))
  val sani_inst_1     = Output(UInt(32.W))
  val sani_inst_2     = Output(UInt(32.W))
}

trait HasGHT_CFG_IO extends BaseModule {
  val params: GHT_CFG_Params
  val io = IO(new GHT_CFG_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_CFG (val params: GHT_CFG_Params) extends Module with HasGHT_CFG_IO
{
  // Core index for performance counter
  io.pmc_core_s     := 0x01.U
  io.pmc_core_e     := 0x02.U
  io.pmc_inst_1     := 0x01.U // load instructions


  io.sani_core_s    := 0x03.U
  io.sani_core_e    := 0x04.U
  io.sani_inst_1    := 0x01.U // load instructions
  io.sani_inst_2    := 0x02.U // store instructions
}