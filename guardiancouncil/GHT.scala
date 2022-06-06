package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHTParams(
  width_core_pc: Int,
  width_data: Int,
  totalnumber_of_checkers: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_IO (params: GHTParams) extends Bundle {
  val resetvector_in                            = Input(UInt(params.width_core_pc.W))
  val ght_pcaddr_in                             = Input(UInt(params.width_core_pc.W))
  val ght_alu_in                                = Input(UInt(params.width_data.W))
  val ght_inst_in                               = Input(UInt(32.W))
  val ght_packet_out                            = Output(UInt(74.W))
  val ght_packet_dest                           = Output(UInt(params.width_core_pc.W))
  val ght_mask_in                               = Input(UInt(1.W))
  val ght_cfg_in                                = Input(UInt(32.W))
  val ght_cfg_valid                             = Input(UInt(1.W))
}

trait HasGHT_IO extends BaseModule {
  val params: GHTParams
  val io = IO(new GHT_IO(params))
}


class GHT (val params: GHTParams) extends Module with HasGHT_IO
{
  val u_ght_cfg                                  = Module (new GHT_CFG(GHT_CFG_Params (params.totalnumber_of_checkers)))

  //==========================================================
  // Commit Counter
  //==========================================================
  val u_ght_cc                                   = Module (new GHT_CC(GHT_CC_Params (params.width_core_pc)))
  u_ght_cc.io.resetvector_in                    := this.io.resetvector_in
  u_ght_cc.io.ght_cc_pcaddr_in                  := this.io.ght_pcaddr_in

  //==========================================================
  // Filters
  //==========================================================
  // Filter: PMC + Sanitiser
  val u_ght_filter                               = Module (new GHT_FILTER(GHT_FILTER_Params (params.width_data, 32)))
  u_ght_filter.io.ght_ft_newcommit_in           := u_ght_cc.io.ght_cc_newcommit_out
  u_ght_filter.io.ght_ft_alu_in                 := this.io.ght_alu_in
  u_ght_filter.io.ght_ft_inst_in                := this.io.ght_inst_in

  // configuration path
  val ght_cfg_in_filter                          = WireInit(0.U(32.W))
  val ght_cfg_valid_filter                       = WireInit(0.U(1.W))
  ght_cfg_in_filter                             := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_filter                          := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_valid, 0.U)
  u_ght_filter.io.ght_ft_cfg_in                 := ght_cfg_in_filter
  u_ght_filter.io.ght_ft_cfg_valid              := ght_cfg_valid_filter
  
  val inst_type                                  = u_ght_filter.io.ght_ft_inst_type
  val ght_pack                                   = u_ght_filter.io.packet_out

  //==========================================================
  // Scheduler
  //==========================================================
  val u_pmc_sch                                  = Module (new GHT_SCH_RR(GHT_SCH_Params (1, params.totalnumber_of_checkers)))
  u_pmc_sch.io.core_s                           := u_ght_cfg.io.pmc_core_s
  u_pmc_sch.io.core_e                           := u_ght_cfg.io.pmc_core_e
  u_pmc_sch.io.monitor_inst(0)                  := u_ght_cfg.io.pmc_inst_1
  u_pmc_sch.io.inst_type                        := inst_type
  val core_d_u_pmc_sch                           = u_pmc_sch.io.core_d


  val u_sani_sch                                 = Module (new GHT_SCH_RR(GHT_SCH_Params (2, params.totalnumber_of_checkers)))
  u_sani_sch.io.core_s                          := u_ght_cfg.io.sani_core_s
  u_sani_sch.io.core_e                          := u_ght_cfg.io.sani_core_e
  u_sani_sch.io.monitor_inst(0)                 := u_ght_cfg.io.sani_inst_1
  u_sani_sch.io.monitor_inst(1)                 := u_ght_cfg.io.sani_inst_2
  u_sani_sch.io.inst_type                       := inst_type
  val core_d_u_sani_sch                          = u_sani_sch.io.core_d



  //==========================================================
  // Output generation
  //==========================================================
  io.ght_packet_out                             := Mux((io.ght_mask_in === 1.U), 0.U, ght_pack)
  io.ght_packet_dest                            := core_d_u_pmc_sch|core_d_u_sani_sch

}
