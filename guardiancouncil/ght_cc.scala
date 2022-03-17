package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}
import chisel3.stage.ChiselStage


//==========================================================
// Parameters
//==========================================================
case class GHT_CC_Params (
  width_core_pc: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_CC_IO (params: GHT_CC_Params) extends Bundle {
  val resetvector_in = Input(UInt(params.width_core_pc.W))
  val ght_cc_pcaddr_in = Input(UInt(params.width_core_pc.W))
  val ght_cc_newcommit_out = Output(Bool())
}

trait HasGHT_CC_IO extends BaseModule {
  val params: GHT_CC_Params
  val io = IO(new GHT_CC_IO(params))
}

//==========================================================
// Implementations
//==========================================================
// GHT Commit Counter
class GHT_CC (val params: GHT_CC_Params) extends Module with HasGHT_CC_IO
{
  val sys_inialised_reg       = RegInit (false.B) // Register used to identify if the system is inialised
  val pcaddr_current_reg      = RegInit (io.resetvector_in)

  val sys_inialised_wire      = WireInit(false.B)
  val new_commit_wire         = WireInit(false.B)

  sys_inialised_wire         := sys_inialised_reg

  new_commit_wire            := Mux(pcaddr_current_reg =/= io.ght_cc_pcaddr_in, 
                                    true.B, 
                                    false.B)

  // Check if the system is initalised
  when (io.ght_cc_pcaddr_in === io.resetvector_in){
    sys_inialised_reg := true.B
  } .otherwise {
    sys_inialised_reg := sys_inialised_reg
  }

  // Core: PC shaddow register
  when (new_commit_wire === true.B) {
    when (sys_inialised_wire === true.B) {
      pcaddr_current_reg := io.ght_cc_pcaddr_in
    } .otherwise {
      pcaddr_current_reg := io.resetvector_in
    }
  } .otherwise {
    pcaddr_current_reg := pcaddr_current_reg
  }


  io.ght_cc_newcommit_out    := new_commit_wire & sys_inialised_wire
}
