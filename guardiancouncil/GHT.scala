package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHTParams(
  width_core_pc: Int,
  width_data: Int,
  totalnumber_of_checkers: Int,
  totalnumber_of_insts: Int,
  totalnumber_of_ses: Int,
  number_of_monitored_insts: Int
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
  val u_ght_filter                               = Module (new GHT_FILTER(GHT_FILTER_Params (params.width_data, params.totalnumber_of_insts)))
  u_ght_filter.io.ght_ft_newcommit_in           := u_ght_cc.io.ght_cc_newcommit_out
  u_ght_filter.io.ght_ft_alu_in                 := this.io.ght_alu_in
  u_ght_filter.io.ght_ft_inst_in                := this.io.ght_inst_in

  // configuration path
  val ght_cfg_in_ft_filter                       = WireInit(0.U(32.W))
  val ght_cfg_valid_ft_filter                    = WireInit(0.U(1.W))
  ght_cfg_in_ft_filter                          := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_ft_filter                       := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_valid, 0.U)
  u_ght_filter.io.ght_ft_cfg_in                 := ght_cfg_in_ft_filter
  u_ght_filter.io.ght_ft_cfg_valid              := ght_cfg_valid_ft_filter
  
  // execution path
  // using registers to break the critical path
  val inst_type                                  = RegInit(0.U(params.totalnumber_of_insts.W))
  val ght_pack                                   = RegInit(0.U(74.W))
  inst_type                                     := u_ght_filter.io.ght_ft_inst_type
  ght_pack                                      := u_ght_filter.io.packet_out

  //==========================================================
  // Scheduler
  //==========================================================
  val u_ses = Seq.fill(params.totalnumber_of_ses) {Module(new GHT_SE(GHT_SE_Params(params.totalnumber_of_checkers, params.totalnumber_of_insts, params.number_of_monitored_insts))).io}


  // configuration path
  val ght_cfg_in_ses_filter                      = WireInit(0.U(32.W))
  val ght_cfg_valid_ses_filter                   = WireInit(0.U(1.W))
  val ght_cfg_seid_ses_filter                    = WireInit(0.U(5.W))
  val ght_cfg_ctrl_ses_filter                    = WireInit(0.U(1.W))
  ght_cfg_in_ses_filter                         := Mux(((this.io.ght_cfg_in(3,0) === 3.U) || (this.io.ght_cfg_in(3,0) === 4.U)),
                                                         this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_ses_filter                      := Mux(((this.io.ght_cfg_in(3,0) === 3.U) || (this.io.ght_cfg_in(3,0) === 4.U)),
                                                         this.io.ght_cfg_valid, 0.U)
  ght_cfg_seid_ses_filter                       := Mux(((this.io.ght_cfg_in(3,0) === 3.U) || (this.io.ght_cfg_in(3,0) === 4.U)),
                                                         this.io.ght_cfg_in(8,4), 0.U)
  ght_cfg_ctrl_ses_filter                       := Mux(((this.io.ght_cfg_in(3,0) === 4.U)),
                                                         1.U, 0.U)
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    when ((ght_cfg_valid_ses_filter === 1.U) && (ght_cfg_seid_ses_filter === i.U)){
      u_ses(i).ght_se_cfg_in                    := ght_cfg_in_ses_filter
      u_ses(i).ght_se_cfg_valid                 := ght_cfg_valid_ses_filter
      u_ses(i).ght_se_cfg_ctrl                  := ght_cfg_ctrl_ses_filter
    } otherwise {
      u_ses(i).ght_se_cfg_in                    := 0.U
      u_ses(i).ght_se_cfg_valid                 := 0.U
      u_ses(i).ght_se_cfg_ctrl                  := 0.U
    }
  }

  // execution path
  val core_d                                     = WireInit(VecInit(Seq.fill(params.totalnumber_of_ses)(0.U(params.totalnumber_of_checkers.W))))
  val core_d_all                                 = WireInit(0.U(params.totalnumber_of_checkers.W))
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    u_ses(i).inst_type                          := inst_type
    core_d(i)                                   := u_ses(i).core_d
  }

  val u_core_d_orgate                            = Module (new GH_ORGATE(ORGATEParams (params.totalnumber_of_checkers, params.totalnumber_of_ses)))
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    u_core_d_orgate.io.in(i)                    := core_d(i)
  }
  core_d_all                                    := u_core_d_orgate.io.out

  // val u_pmc_sch                                  = Module (new GHT_SCH_RR(GHT_SCH_Params (params.totalnumber_of_checkers, 1)))
  // u_pmc_sch.io.core_s                           := u_ght_cfg.io.pmc_core_s
  // u_pmc_sch.io.core_e                           := u_ght_cfg.io.pmc_core_e
  // u_pmc_sch.io.monitor_inst(0)                  := u_ght_cfg.io.pmc_inst_1
  // u_pmc_sch.io.inst_type                        := inst_type
  // val core_d_u_pmc_sch                           = u_pmc_sch.io.core_d


  // val u_sani_sch                                 = Module (new GHT_SCH_RR(GHT_SCH_Params (params.totalnumber_of_checkers, 2)))
  // u_sani_sch.io.core_s                          := u_ght_cfg.io.sani_core_s
  // u_sani_sch.io.core_e                          := u_ght_cfg.io.sani_core_e
  // u_sani_sch.io.monitor_inst(0)                 := u_ght_cfg.io.sani_inst_1
  // u_sani_sch.io.monitor_inst(1)                 := u_ght_cfg.io.sani_inst_2
  // u_sani_sch.io.inst_type                       := inst_type
  // val core_d_u_sani_sch                          = u_sani_sch.io.core_d



  //==========================================================
  // Output generation
  //==========================================================
  io.ght_packet_out                             := Mux((io.ght_mask_in === 1.U), 0.U, ght_pack)
  io.ght_packet_dest                            := core_d_all
}
