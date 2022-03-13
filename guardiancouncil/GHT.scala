package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHTParams(
  width_core_pc: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_IO (params: GHTParams) extends Bundle {
  val resetvector_in = Input(UInt(params.width_core_pc.W))
  val ght_pcaddr_in = Input(UInt(params.width_core_pc.W))
  val ght_inst_in = Input(UInt(32.W))
  val ght_inst_type = Output(UInt(4.W))
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
  val u_ght_dc                  = Module (new GHT_DC())

  u_ght_cc.io.resetvector_in   := this.io.resetvector_in
  u_ght_cc.io.ght_cc_pcaddr_in := this.io.ght_pcaddr_in

  u_ght_dc.io.ght_dc_inst_in   := this.io.ght_inst_in
  u_ght_dc.io.ght_dc_newcommit_in := u_ght_cc.io.ght_cc_newcommit_out
  this.io.ght_inst_type        := u_ght_dc.io.ght_dc_inst_type
  // val zeros                     = WireInit(0x55555555.U(64.W))
  val counter_reg               = RegInit(1.U(64.W))
  this.io.ght_packet_out       := Mux((io.ght_mask_in === 1.U), 0.U, Cat(u_ght_dc.io.ght_dc_inst_func_opcode, counter_reg))

  // Below code is used for tests
  when (u_ght_dc.io.ght_dc_inst_func_opcode =/= 0.U)
  {
    counter_reg                := counter_reg + 1.U
  }
}
