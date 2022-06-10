package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_STABLE_Params(
)

//==========================================================
// I/Os
//==========================================================
class GHT_STABLE_IO (params: GHT_STABLE_Params) extends Bundle {
  val cfg_sch_end_id                            = Input(UInt(4.W))
  val cfg_sch_policy                            = Input(UInt(7.W))
  val cfg_sch_start_id                          = Input(UInt(4.W))
  val cfg_sch_valid                             = Input(UInt(1.W))
  val sch_end_id                                = Output(UInt(4.W))
  val sch_policy                                = Output(UInt(7.W))
  val sch_start_id                              = Output(UInt(4.W))  
}

trait HasGHT_STABLE_IO extends BaseModule {
  val params: GHT_STABLE_Params
  val io = IO(new GHT_STABLE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_STABLE (val params: GHT_STABLE_Params) extends Module with HasGHT_STABLE_IO
{
  val cfg_sch                                   = WireInit(0.U(1.W))
  val sch_end_id_reg                            = RegInit(0.U(4.W))
  val sch_policy_reg                            = RegInit(0.U(7.W))
  val sch_start_id_reg                          = RegInit(0.U(4.W))

  cfg_sch                                      := io.cfg_sch_valid === 1.U

  when (cfg_sch  === 1.U) {
    sch_end_id_reg                             := io.cfg_sch_end_id
    sch_policy_reg                             := io.cfg_sch_policy
    sch_start_id_reg                           := io.cfg_sch_start_id
  }


  io.sch_end_id                                := sch_end_id_reg
  io.sch_policy                                := sch_policy_reg
  io.sch_start_id                              := sch_start_id_reg
}