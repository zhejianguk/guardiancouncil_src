package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import chisel3.stage.ChiselStage


//==========================================================
// Parameters
//==========================================================
// *Driver is used for verilog generation
object GHT_DC_Driver extends App {
  (new ChiselStage).emitVerilog(new GHT_DC(), args)
}

//==========================================================
// I/Os
//==========================================================
class GHT_DC_IO () extends Bundle {
  val ght_dc_inst_in = Input(UInt(32.W))
  val ght_dc_newcommit_in = Input(Bool())
  val ght_dc_inst_type = Output(UInt(4.W))
  val ght_dc_inst_func_opcode = Output(UInt(10.W))
}

trait HasGHT_DC_IO extends BaseModule {
  val io = IO(new GHT_DC_IO())
}

//==========================================================
// Implementations
//==========================================================
class GHT_DC () extends Module with HasGHT_DC_IO
{
  // Filter of instruction
  val sys_inialised_reg       = RegInit (false.B) // Register used to identify if the system is inialised

  val inst_filtered_wire      = WireInit(0.U(10.W))
  val func                    = io.ght_dc_inst_in(14, 12)
  val opcode                  = io.ght_dc_inst_in(6,0)
  // Filtering the opcode & func
  inst_filtered_wire          := Mux(io.ght_dc_newcommit_in === true.B, 
                                     Cat(func, opcode), 
                                     0x000.U)


  val opcode_LB_wire          = WireInit(false.B)
  val opcode_LH_wire          = WireInit(false.B)
  val opcode_LW_wire          = WireInit(false.B)
  val opcode_LBU_wire         = WireInit(false.B)
  val opcode_LHU_wire         = WireInit(false.B)

  opcode_LB_wire              := Mux(inst_filtered_wire === 0x03.U, true.B, false.B)
  opcode_LH_wire              := Mux(inst_filtered_wire === 0x83.U, true.B, false.B)
  opcode_LW_wire              := Mux(inst_filtered_wire === 0x103.U, true.B, false.B)
  opcode_LBU_wire             := Mux(inst_filtered_wire === 0x203.U, true.B, false.B)
  opcode_LHU_wire             := Mux(inst_filtered_wire === 0x283.U, true.B, false.B)

  val inst_type_load           = opcode_LB_wire |
                                 opcode_LH_wire |
                                 opcode_LW_wire |
                                 opcode_LBU_wire|
                                 opcode_LHU_wire

  val int_type_x               = 0.U; // Revisit
  val int_type_y               = 0.U; // Revisit
  val int_type_z               = 0.U; // Revisit

  io.ght_dc_inst_type         := Cat(int_type_z, 
                                     int_type_y, 
                                     int_type_x, 
                                     inst_type_load)
  io.ght_dc_inst_func_opcode  := inst_filtered_wire

}
