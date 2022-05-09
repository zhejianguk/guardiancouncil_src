package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHTParams(
  width_core_pc: Int,
  width_data: Int
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
  val ght_pack                  = WireInit(0.U(74.W))
  
  val u_ght_cc                  = Module (new GHT_CC(GHT_CC_Params (params.width_core_pc)))

  //==========================================================
  // Filters
  //==========================================================
  // Filter: PMC
  // val u_ght_dc                  = Module (new GHT_DC_PMC())

  // Filter: Sanitiser
  val u_ght_dc                  = Module (new GHT_DC_Sanitiser(GHT_DC_Params (params.width_data)))

  u_ght_cc.io.resetvector_in   := this.io.resetvector_in
  u_ght_cc.io.ght_cc_pcaddr_in := this.io.ght_pcaddr_in
  u_ght_dc.io.ght_dc_newcommit_in := u_ght_cc.io.ght_cc_newcommit_out
  u_ght_dc.io.ght_dc_alu_in    := this.io.ght_alu_in
  u_ght_dc.io.ght_dc_inst_in   := this.io.ght_inst_in

  val inst_type                 = u_ght_dc.io.ght_dc_inst_type

  //==========================================================
  // Output generation
  //==========================================================
  ght_pack                     := MuxCase(0.U, 
                                    Array((inst_type === 1.U) -> Cat(u_ght_dc.io.ght_dc_inst_func_opcode, u_ght_dc.io.ght_dc_commit_rd), // load
                                          (inst_type === 2.U) -> Cat(u_ght_dc.io.ght_dc_inst_func_opcode, u_ght_dc.io.ght_dc_commit_rd)  // store
                                          )
                                         ) 

  this.io.ght_packet_out       := Mux((io.ght_mask_in === 1.U), 0.U, ght_pack)
}
