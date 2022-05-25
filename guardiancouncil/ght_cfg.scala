package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================


//==========================================================
// I/Os
//==========================================================
class GHT_CFG_IO () extends Bundle {
  val pmc_core_s = Output(UInt(5.W))
  val pmc_core_e = Output(UInt(5.W))
  val pmc_inst_1 = Output(UInt(32.W))
}

trait HasGHT_CFG_IO extends BaseModule {
  val io = IO(new GHT_CFG_IO())
}

//==========================================================
// Implementations
//==========================================================
class GHT_CFG () extends Module with HasGHT_CFG_IO
{
  // Core index for performance counter
  io.pmc_core_s := 0x01.U
  io.pmc_core_e := 0x02.U
  io.pmc_inst_1 := 0x01.U // load instructions

}