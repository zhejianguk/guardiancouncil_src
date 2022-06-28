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
  packet_size: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_IO (params: GHTParams) extends Bundle {
  val resetvector_in                            = Input(UInt(params.width_core_pc.W))
  val ght_pcaddr_in                             = Input(UInt(params.width_core_pc.W))
  val ght_alu_in                                = Input(UInt(params.width_data.W))
  val ght_inst_in                               = Input(UInt(32.W))
  val ght_packet_out                            = Output(UInt((params.packet_size).W))
  val ght_packet_dest                           = Output(UInt(params.width_core_pc.W))
  val ght_mask_in                               = Input(UInt(1.W))
  val ght_cfg_in                                = Input(UInt(32.W))
  val ght_cfg_valid                             = Input(UInt(1.W))
  val core_na                                   = Input(UInt(params.totalnumber_of_checkers.W)) 
}

trait HasGHT_IO extends BaseModule {
  val params: GHTParams
  val io = IO(new GHT_IO(params))
}


class GHT (val params: GHTParams) extends Module with HasGHT_IO
{
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
  val u_ght_filter                               = Module (new GHT_FILTER(GHT_FILTER_Params (params.width_data, params.totaltypes_of_insts, params.packet_size)))
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
  val inst_type                                  = RegInit(0.U(params.totaltypes_of_insts.W))
  val ght_pack                                   = RegInit(0.U((params.packet_size).W))
  val inst_index                                 = RegInit(0.U(5.W))
  inst_type                                     := u_ght_filter.io.ght_ft_inst_type
  ght_pack                                      := u_ght_filter.io.packet_out
  inst_index                                    := u_ght_filter.io.ght_ft_inst_index

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



  //==========================================================
  // Output generation
  //==========================================================
  io.ght_packet_out                             := Mux((io.ght_mask_in === 1.U), 0.U, ght_pack)
  io.ght_packet_dest                            := core_d_all
}
