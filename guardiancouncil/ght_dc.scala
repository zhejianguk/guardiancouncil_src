package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_DC_Params(
  width_data: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_DC_IO (params: GHT_DC_Params) extends Bundle {
  val ght_dc_inst_in = Input(UInt(32.W))
  val ght_dc_newcommit_in = Input(Bool())
  val ght_dc_alu_in = Input(UInt(params.width_data.W))
  val ght_dc_inst_type = Output(UInt(32.W))
  // 0x00: load;
  // 0x01: store;
  // 0x04: ..
  // 0x08: ....
  val packet_out = Output(UInt(74.W))
}

trait HasGHT_DC_IO extends BaseModule {
  val params: GHT_DC_Params
  val io = IO(new GHT_DC_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_Filter_SuperSet (val params: GHT_DC_Params) extends Module with HasGHT_DC_IO
{
  val inst_filtered_wire      = WireInit(0.U(10.W))
  val func                    = io.ght_dc_inst_in(14, 12)
  val opcode                  = io.ght_dc_inst_in(6,0)
  // Filtering the opcode & func
  inst_filtered_wire          := Mux(io.ght_dc_newcommit_in === true.B, 
                                     Cat(func, opcode), 
                                     0x000.U)

  val packet_zeros            = WireInit(0.U(64.W))
  val opcode_LB_wire          = WireInit(false.B)
  val opcode_LH_wire          = WireInit(false.B)
  val opcode_LW_wire          = WireInit(false.B)
  val opcode_LD_wire          = WireInit(false.B)
  val opcode_LBU_wire         = WireInit(false.B)
  val opcode_LHU_wire         = WireInit(false.B)

  val opcode_SB_wire          = WireInit(false.B)
  val opcode_SH_wire          = WireInit(false.B)
  val opcode_SW_wire          = WireInit(false.B)
  val opcode_SD_wire          = WireInit(false.B)

  // Filter for Load instructions, inst_type: 0x01
  opcode_LB_wire              := Mux(inst_filtered_wire === 0x03.U, true.B, false.B)
  opcode_LH_wire              := Mux(inst_filtered_wire === 0x83.U, true.B, false.B)
  opcode_LW_wire              := Mux(inst_filtered_wire === 0x103.U, true.B, false.B)
  opcode_LD_wire              := Mux(inst_filtered_wire === 0x183.U, true.B, false.B)
  opcode_LBU_wire             := Mux(inst_filtered_wire === 0x203.U, true.B, false.B)
  opcode_LHU_wire             := Mux(inst_filtered_wire === 0x283.U, true.B, false.B)

  // Filter for Store instructions, inst_type: 0x02
  opcode_SB_wire              := Mux(inst_filtered_wire === 0x23.U, true.B, false.B)
  opcode_SH_wire              := Mux(inst_filtered_wire === 0xA3.U, true.B, false.B)
  opcode_SW_wire              := Mux(inst_filtered_wire === 0x123.U, true.B, false.B)
  opcode_SD_wire              := Mux(inst_filtered_wire === 0x1A3.U, true.B, false.B)


  val inst_type_load           = opcode_LB_wire |
                                 opcode_LH_wire |
                                 opcode_LW_wire |
                                 opcode_LBU_wire|
                                 opcode_LHU_wire|
                                 opcode_LD_wire 
                                 
  val inst_type_store          = opcode_SB_wire |
                                 opcode_SH_wire |
                                 opcode_SW_wire |
                                 opcode_SD_wire 

  val int_type_y               = 0.U; // Revisit
  val int_type_z               = 0.U; // Revisit

  io.ght_dc_inst_type         := Cat(int_type_z, 
                                     int_type_y, 
                                     inst_type_store, 
                                     inst_type_load)

  io.packet_out               := MuxCase(0.U, 
                                   Array(inst_type_load  -> Cat(inst_filtered_wire, packet_zeros), // load
                                         inst_type_store -> Cat(inst_filtered_wire, io.ght_dc_alu_in)  // store
                                        )
                                        ) 
}