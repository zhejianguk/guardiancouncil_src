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
  val resetvector_in = Input(UInt(params.width_core_pc.W))
  val ght_pcaddr_in = Input(UInt(params.width_core_pc.W))
  val ght_alu_in = Input(UInt(params.width_data.W))
  val ght_inst_in = Input(UInt(32.W))
  val ght_packet_out = Output(UInt(74.W))
  val ght_mask_in = Input(UInt(1.W))
}

trait HasGHT_IO extends BaseModule {
  val params: GHTParams
  val io = IO(new GHT_IO(params))
}


class GHT (val params: GHTParams) extends Module with HasGHT_IO
{
  val u_ght_cc                  = Module (new GHT_CC(GHT_CC_Params (params.width_core_pc)))
  val u_ght_cfg                 = Module (new GHT_CFG())

  //==========================================================
  // Filters
  //==========================================================
  // Filter: PMC + Sanitiser
  val u_ght_dc                  = Module (new GHT_Filter_SuperSet(GHT_DC_Params (params.width_data)))

  u_ght_cc.io.resetvector_in   := this.io.resetvector_in
  u_ght_cc.io.ght_cc_pcaddr_in := this.io.ght_pcaddr_in
  u_ght_dc.io.ght_dc_newcommit_in := u_ght_cc.io.ght_cc_newcommit_out
  u_ght_dc.io.ght_dc_alu_in    := this.io.ght_alu_in
  u_ght_dc.io.ght_dc_inst_in   := this.io.ght_inst_in

  val inst_type                 = u_ght_dc.io.ght_dc_inst_type
  val ght_pack                  = u_ght_dc.io.packet_out

  //==========================================================
  // Scheduler
  //==========================================================
  val u_pmc_sch                 = Module (new GHT_SCH_RR(GHT_SCH_Params (1, params.totalnumber_of_checkers)))
  u_pmc_sch.io.core_s          := u_ght_cfg.io.pmc_core_s
  u_pmc_sch.io.core_e          := u_ght_cfg.io.pmc_core_e
  u_pmc_sch.io.monitor_inst(0) := u_ght_cfg.io.pmc_inst_1
  u_pmc_sch.io.inst_type       := inst_type
  val core_d_u_pmc_sch          = u_pmc_sch.io.core_d


  //==========================================================
  // Output generation
  //==========================================================
  this.io.ght_packet_out       := Mux(((io.ght_mask_in === 1.U) | (core_d_u_pmc_sch === 0.U)), 0.U, ght_pack)
}
