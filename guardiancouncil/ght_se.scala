package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_SE_Params(
  totalnumber_of_checkers: Int,
  totalnumber_of_insts: Int,
  number_of_monitored_insts: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_SE_IO (params: GHT_SE_Params) extends Bundle {
  val ght_se_cfg_in                             = Input(UInt(32.W))
  val ght_se_cfg_valid                          = Input(UInt(1.W))
  val ght_se_cfg_ctrl                           = Input(UInt(1.W))

  val inst_type                                 = Input(UInt(32.W))
  val core_d                                    = Output(UInt(params.totalnumber_of_checkers.W))
}



trait HasGHT_SE_IO extends BaseModule {
  val params: GHT_SE_Params
  val io = IO(new GHT_SE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_SE (val params: GHT_SE_Params) extends Module with HasGHT_SE_IO
{
  //==========================================================
  // Configurations
  //==========================================================
  val u_ght_stable                              = Module (new GHT_STABLE(GHT_STABLE_Params (params.number_of_monitored_insts, params.totalnumber_of_insts)))
  u_ght_stable.io.cfg_sch_end_id               := this.io.ght_se_cfg_in(31,28)
  u_ght_stable.io.cfg_sch_policy               := this.io.ght_se_cfg_in(27,21)
  u_ght_stable.io.cfg_sch_start_id             := this.io.ght_se_cfg_in(20,17)
  u_ght_stable.io.cfg_sch_monitor_inst         := this.io.ght_se_cfg_in(27,21)
  u_ght_stable.io.cfg_sch_monitor_index        := this.io.ght_se_cfg_in(20,17)
  u_ght_stable.io.cfg_sch_valid                := this.io.ght_se_cfg_valid
  u_ght_stable.io.cfg_sch_ctrl                 := this.io.ght_se_cfg_ctrl

  val sch_policy                                = WireInit(0.U(7.W))
  val sch_end_id                                = WireInit(0.U(4.W))
  val sch_start_id                              = WireInit(0.U(4.W))
  val monitor_inst                              = WireInit(VecInit(Seq.fill(params.number_of_monitored_insts)(0.U(params.totalnumber_of_insts.W))))

  sch_policy                                   := u_ght_stable.io.sch_policy
  sch_end_id                                   := u_ght_stable.io.sch_end_id
  sch_start_id                                 := u_ght_stable.io.sch_start_id
  
  for (i <- 0 to params.number_of_monitored_insts - 1) {
    monitor_inst(i)                            := u_ght_stable.io.monitor_inst(i)
  }

  //==========================================================
  // Schedulers
  //==========================================================
  // round-robin scheduler
  val u_sch_rr                                  = Module (new GHT_SCH_RR(GHT_SCH_Params (params.totalnumber_of_checkers, params.number_of_monitored_insts)))
  val core_d_rr                                 = WireInit(0.U(params.totalnumber_of_checkers.W))
  u_sch_rr.io.core_s                           := sch_start_id
  u_sch_rr.io.core_e                           := sch_end_id
  u_sch_rr.io.inst_type                        := io.inst_type
  for (i <- 0 to params.number_of_monitored_insts - 1) {
    u_sch_rr.io.monitor_inst(i)                := monitor_inst(i)
  }
  core_d_rr                                    := u_sch_rr.io.core_d

  // round-robin-4 scheduler
  val u_sch_rrf                                 = Module (new GHT_SCH_RRF(GHT_SCH_Params (params.totalnumber_of_checkers, params.number_of_monitored_insts)))
  val core_d_rrf                                = WireInit(0.U(params.totalnumber_of_checkers.W))
  u_sch_rrf.io.core_s                          := sch_start_id
  u_sch_rrf.io.core_e                          := sch_end_id
  u_sch_rrf.io.inst_type                       := io.inst_type
  for (i <- 0 to params.number_of_monitored_insts - 1) {
    u_sch_rrf.io.monitor_inst(i)               := monitor_inst(i)
  }
  core_d_rrf                                   := u_sch_rrf.io.core_d


  io.core_d                                    := MuxCase(0.U, 
                                                  Array((sch_policy === 1.U) -> core_d_rr,
                                                        (sch_policy === 2.U) -> core_d_rrf,
                                                        ))
}