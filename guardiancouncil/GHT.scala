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
  totaltypes_of_insts: Int,
  totalnumber_of_ses: Int,
  packet_size: Int,
  core_width: Int,
  use_prfs: Boolean
)

//==========================================================
// I/Os
//==========================================================
class GHT_IO (params: GHTParams) extends Bundle {
  val ght_packet_out                            = Output(UInt((params.packet_size).W))
  val ght_packet_dest                           = Output(UInt(params.totalnumber_of_checkers.W))
  val ght_mask_in                               = Input(UInt(1.W))
  val ght_cfg_in                                = Input(UInt(32.W))
  val ght_cfg_valid                             = Input(UInt(1.W))
  val core_na                                   = Input(UInt(params.totalnumber_of_checkers.W))
  val ghm_agg_core_id                           = Output(UInt(16.W))
  
  val ght_inst_in                               = Input(Vec(params.core_width, UInt(32.W)))
  val ght_pcaddr_in                             = Input(Vec(params.core_width, UInt(params.width_core_pc.W)))
  val new_commit                                = Input(Vec(params.core_width, Bool()))
  val ght_alu_in                                = Input(Vec(params.core_width, UInt(params.width_data.W)))
  val ght_prfs_rd                               = Input(Vec(params.core_width, UInt(params.width_data.W)))
  val ght_is_rvc_in                             = Input(Vec(params.core_width, UInt(1.W)))

  val ght_stall                                 = Input(Bool())
  val core_hang_up                              = Output(UInt(1.W))
  val ght_buffer_status                         = Output(UInt(2.W))

  val ght_prfs_forward_ldq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_stq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_ftq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_prf                      = Output(Vec(params.core_width, Bool()))

  val ght_filters_empty                         = Output(UInt(1.W))
  val debug_mcounter                            = Output(UInt(64.W))
  val debug_icounter                            = Output(UInt(64.W))
  val debug_bp_reset                            = Input(UInt(1.W))
  val debug_bp_in                               = Input(UInt(2.W))
  val debug_bp_checker                          = Output(UInt(64.W))
  val debug_bp_cdc                              = Output(UInt(64.W))
  val debug_bp_filter                           = Output(UInt(64.W))
}

trait HasGHT_IO extends BaseModule {
  val params: GHTParams
  val io = IO(new GHT_IO(params))
}


class GHT (val params: GHTParams) extends Module with HasGHT_IO
{
  val sch_hang                                   = WireInit(0.U(1.W))
  //==========================================================
  // Filters
  //==========================================================
  // Filters
  val new_commit_ft                              = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))
  val ght_alu_in_ft                              = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.width_data.W))))
  val ght_inst_in_ft                             = WireInit(VecInit(Seq.fill(params.core_width)(0.U(32.W))))
  val ght_pcaddr_in_ft                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.width_data.W))))
  val ght_prfs_rd_ft                             = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.width_data.W))))
  val ght_is_rvc_ft                              = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))

  for (i<- 0 to params.core_width - 1){
    new_commit_ft(i)                            := Mux((io.ght_mask_in === 1.U), 0.U, this.io.new_commit(i))
    ght_alu_in_ft(i)                            := Mux((io.ght_mask_in === 1.U), 0.U, this.io.ght_alu_in(i))
    ght_inst_in_ft(i)                           := Mux((io.ght_mask_in === 1.U), 0.U, this.io.ght_inst_in(i))
    ght_pcaddr_in_ft(i)                         := Mux((io.ght_mask_in === 1.U), 0.U, this.io.ght_pcaddr_in(i))
    ght_prfs_rd_ft(i)                           := Mux((io.ght_mask_in === 1.U), 0.U, this.io.ght_prfs_rd(i))
    ght_is_rvc_ft(i)                            := Mux((io.ght_mask_in === 1.U), 0.U, this.io.ght_is_rvc_in(i))
  }

  val u_ght_filters                              = Module (new GHT_FILTERS_PRFS(GHT_FILTERS_PRFS_Params (params.width_data, params.totaltypes_of_insts, params.packet_size, params.core_width, params.use_prfs)))

  for (i <- 0 to params.core_width - 1) {  
    u_ght_filters.io.ght_ft_newcommit_in(i)     := new_commit_ft(i)
    u_ght_filters.io.ght_ft_alu_in(i)           := ght_alu_in_ft(i)
    u_ght_filters.io.ght_ft_inst_in(i)          := ght_inst_in_ft(i)
    u_ght_filters.io.ght_ft_pc_in(i)            := ght_pcaddr_in_ft(i)
    u_ght_filters.io.ght_prfs_rd_ft(i)          := ght_prfs_rd_ft(i)
    u_ght_filters.io.ght_ft_is_rvc_in(i)        := ght_is_rvc_ft(i)
  }
  u_ght_filters.io.ght_stall                    := (this.io.ght_stall || (sch_hang === 1.U))
  this.io.ght_buffer_status                     := u_ght_filters.io.ght_buffer_status
  
  // configuration path
  val ght_cfg_in_ft_filter                       = WireInit(0.U(32.W))
  val ght_cfg_valid_ft_filter                    = WireInit(0.U(1.W))
  ght_cfg_in_ft_filter                          := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_ft_filter                       := Mux((this.io.ght_cfg_in(3,0) === 2.U),
                                                        this.io.ght_cfg_valid, 0.U)
  u_ght_filters.io.ght_ft_cfg_in                := ght_cfg_in_ft_filter
  u_ght_filters.io.ght_ft_cfg_valid             := ght_cfg_valid_ft_filter

  // execution path
  // using registers to break the critical path
  val ght_pack                                   = WireInit(0.U((params.packet_size).W))
  val inst_index                                 = WireInit(0.U(5.W))
  ght_pack                                      := u_ght_filters.io.packet_out
  inst_index                                    := u_ght_filters.io.ght_ft_inst_index
  
  
  for (i <- 0 to params.core_width -1){
    if (params.use_prfs){ 
      io.ght_prfs_forward_ldq(i)                := u_ght_filters.io.ght_prfs_forward_ldq(i)
      io.ght_prfs_forward_stq(i)                := u_ght_filters.io.ght_prfs_forward_stq(i)
      io.ght_prfs_forward_ftq(i)                := u_ght_filters.io.ght_prfs_forward_ftq(i)
      io.ght_prfs_forward_prf(i)                := u_ght_filters.io.ght_prfs_forward_prf(i)
    } else {
      io.ght_prfs_forward_ldq(i)                := false.B
      io.ght_prfs_forward_stq(i)                := false.B
      io.ght_prfs_forward_ftq(i)                := false.B
      io.ght_prfs_forward_prf(i)                := false.B
    }
  }

  // Simulating 1/2-width event filter
  val ght_cfg_in_ft_filter_width                 = WireInit(0.U(32.W))
  val ght_cfg_valid_ft_filter_width              = WireInit(0.U(1.W))
  ght_cfg_in_ft_filter_width                    := Mux((this.io.ght_cfg_in(3,0) === 5.U),
                                                        this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_ft_filter_width                 := Mux((this.io.ght_cfg_in(3,0) === 5.U),
                                                        this.io.ght_cfg_valid, 0.U)

  val debug_filter_width_reg                     = RegInit(0.U(4.W))
  when (ght_cfg_valid_ft_filter_width === 1.U) {
    debug_filter_width_reg                      := ght_cfg_in_ft_filter_width(7,4)
  }
  u_ght_filters.io.debug_filter_width           := debug_filter_width_reg

 //==========================================================
  // Mapper
  //==========================================================
  // Mapper
  val u_ght_mapper                               = Module (new GHT_MAPPER(GHT_MAPPER_Params(params.totaltypes_of_insts, params.totalnumber_of_ses)))
  // configuration path
  val ght_cfg_in_mp_filter                       = WireInit(0.U(32.W))
  val ght_cfg_valid_mp_filter                    = WireInit(0.U(1.W))
  ght_cfg_in_mp_filter                          := Mux((this.io.ght_cfg_in(3,0) === 3.U),
                                                        this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_mp_filter                       := Mux((this.io.ght_cfg_in(3,0) === 3.U),
                                                        this.io.ght_cfg_valid, 0.U)
  u_ght_mapper.io.ght_mp_cfg_in                 := ght_cfg_in_mp_filter
  u_ght_mapper.io.ght_mp_cfg_valid              := ght_cfg_valid_mp_filter
  
  // execution path
  val inst_c                                     = WireInit(VecInit(Seq.fill(params.totalnumber_of_ses)(0.U(1.W))))
  u_ght_mapper.io.inst_index                    := inst_index
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    inst_c(i)                                   := u_ght_mapper.io.inst_c(i)
  }



  //==========================================================
  // Scheduler Engines
  //==========================================================
  val u_ses = Seq.fill(params.totalnumber_of_ses) {Module(new GHT_SE(GHT_SE_Params(params.totalnumber_of_checkers))).io}

  // configuration path
  val ght_cfg_in_ses_filter                      = WireInit(0.U(32.W))
  val ght_cfg_valid_ses_filter                   = WireInit(0.U(1.W))
  val ght_cfg_seid_ses_filter                    = WireInit(0.U(5.W))
  ght_cfg_in_ses_filter                         := Mux((this.io.ght_cfg_in(3,0) === 4.U), this.io.ght_cfg_in, 0.U)
  ght_cfg_valid_ses_filter                      := Mux((this.io.ght_cfg_in(3,0) === 4.U), this.io.ght_cfg_valid, 0.U)
  ght_cfg_seid_ses_filter                       := Mux((this.io.ght_cfg_in(3,0) === 4.U), this.io.ght_cfg_in(8,4), 0.U)

  for (i <- 0 to params.totalnumber_of_ses - 1) {
    when ((ght_cfg_valid_ses_filter === 1.U) && (ght_cfg_seid_ses_filter === i.U)){
      u_ses(i).ght_se_cfg_in                    := ght_cfg_in_ses_filter
      u_ses(i).ght_se_cfg_valid                 := ght_cfg_valid_ses_filter
    } otherwise {
      u_ses(i).ght_se_cfg_in                    := 0.U
      u_ses(i).ght_se_cfg_valid                 := 0.U
    }
  }

  // execution path
  val core_d                                     = WireInit(VecInit(Seq.fill(params.totalnumber_of_ses)(0.U(params.totalnumber_of_checkers.W))))
  val core_d_all                                 = WireInit(0.U(params.totalnumber_of_checkers.W))
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    core_d(i)                                   := u_ses(i).core_d
    u_ses(i).inst_c                             := inst_c(i)
    u_ses(i).core_na                            := io.core_na
  }

  val u_core_d_orgate                            = Module (new GH_ORGATE(ORGATEParams (params.totalnumber_of_checkers, params.totalnumber_of_ses)))
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    u_core_d_orgate.io.in(i)                    := core_d(i)
  }
  core_d_all                                    := u_core_d_orgate.io.out

  val sch_hangs                                  = WireInit(VecInit(Seq.fill(params.totalnumber_of_ses)(0.U(1.W))))
  for (i <- 0 to params.totalnumber_of_ses - 1) {
    sch_hangs(i)                                := u_ses(i).sch_hang
  }
  sch_hang                                      := sch_hangs.reduce(_|_)


  //==========================================================
  // AGG Configuration
  //==========================================================
  val agg_core_id                                = RegInit(4.U(16.W))
  val agg_core_id_cfg_filter                     = WireInit(0.U(16.W))
  val agg_core_id_valid_filter                   = WireInit(0.U(1.W))
  agg_core_id_cfg_filter                        := Mux((this.io.ght_cfg_in(3,0) === 0x8.U),
                                                        this.io.ght_cfg_in(31, 16), 0.U)
  agg_core_id_valid_filter                      := Mux((this.io.ght_cfg_in(3,0) === 0x8.U),
                                                        this.io.ght_cfg_valid, 0.U)

  when (agg_core_id_valid_filter === 1.U) {
    agg_core_id                                 := agg_core_id_cfg_filter
  }

  //==========================================================
  // Output generation
  //==========================================================
  io.core_hang_up                               := u_ght_filters.io.core_hang_up
  io.ght_packet_out                             := ght_pack
  io.ght_packet_dest                            := core_d_all
  io.ghm_agg_core_id                            := agg_core_id
  io.ght_filters_empty                          := u_ght_filters.io.ght_filters_empty & (core_d_all === 0.U) & (ght_pack === 0.U)

  val debug_mcounter                             = RegInit(0.U(64.W))
  val debug_icounter                             = RegInit(0.U(64.W))
  when (core_d_all =/= 0.U) {
    debug_mcounter                              := debug_mcounter + 1.U
  }
  when (inst_index =/= 0.U) {
    debug_icounter                              := debug_icounter + 1.U
  }
  
  io.debug_mcounter                             := debug_mcounter
  io.debug_icounter                             := debug_icounter


  val debug_bp_checker                           = RegInit(0.U(64.W))
  val debug_bp_cdc                               = RegInit(0.U(64.W))
  val debug_bp_filter                            = RegInit(0.U(64.W))
  when (io.debug_bp_reset === 1.U) {
    debug_bp_checker                            := 0.U
  } .otherwise {
    when ((u_ght_filters.io.core_hang_up === 1.U) && (io.debug_bp_in(0) === 1.U)){
      debug_bp_checker                          := debug_bp_checker + 1.U
    } .otherwise {
      debug_bp_checker                          := debug_bp_checker
    }
  }

  when (io.debug_bp_reset === 1.U) {
    debug_bp_cdc                                := 0.U
  } .otherwise {
    when ((u_ght_filters.io.core_hang_up === 1.U) && (io.debug_bp_in === 2.U)){
      debug_bp_cdc                              := debug_bp_cdc + 1.U
    } .otherwise {
      debug_bp_cdc                              := debug_bp_cdc
    }
  }

  when (io.debug_bp_reset === 1.U) {
    debug_bp_filter                             := 0.U
  } .otherwise {
    when ((u_ght_filters.io.core_hang_up === 1.U) && (io.debug_bp_in === 0.U)){
      debug_bp_filter                           := debug_bp_filter + 1.U
    } .otherwise {
      debug_bp_filter                           := debug_bp_filter
    }
  }

  io.debug_bp_checker                           := debug_bp_checker
  io.debug_bp_cdc                               := debug_bp_cdc
  io.debug_bp_filter                            := debug_bp_filter
}
