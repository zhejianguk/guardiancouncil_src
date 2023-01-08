package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FTABLE_Params(
)

//==========================================================
// I/Os
//==========================================================
class GHT_FTABLE_IO (params: GHT_FTABLE_Params) extends Bundle {
  val cfg_ref_inst_func                         = Input(UInt(4.W))
  val cfg_ref_inst_opcode                       = Input(UInt(7.W))
  val cfg_ref_inst_index                        = Input(UInt(5.W))
  val cfg_ref_inst_sel_d                        = Input(UInt(4.W))
  val cfg_ref_inst_valid                        = Input(UInt(1.W))
  val inst_newcommit                            = Input(Bool())
  val inst_is_rvc                               = Input(UInt(1.W))
  val inst_in_func                              = Input(UInt(3.W))
  val inst_in_opcode                            = Input(UInt(7.W))
  val inst_index                                = Output(UInt(5.W))
  val inst_sel_d                                = Output(UInt(2.W))    
}

trait HasGHT_FTABLE_IO extends BaseModule {
  val params: GHT_FTABLE_Params
  val io = IO(new GHT_FTABLE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_FTABLE (val params: GHT_FTABLE_Params) extends Module with HasGHT_FTABLE_IO
{
  // We set the memory size as 4 bits * 1024 -- this is for Implemenation & Rouring.
  // But we have reserved 9 bits for future upgrade 
  val mem_data                                  = WireInit(0.U((2 + 2).W))
  val mem_raddr                                 = WireInit(0.U(11.W))
  val mem_ren                                   = WireInit(false.B)
  val mem_ren_reg                               = RegInit(false.B)
         
  mem_raddr                                    := Cat(io.inst_is_rvc, io.inst_in_func, io.inst_in_opcode)
  mem_ren                                      := io.inst_newcommit
  mem_ren_reg                                  := mem_ren

  // Size: 2^(is_rvc + width_func + width_opcode)
  // Width: width_index + width_sel_d
  val ref_table                                 = SyncReadMem(2048, UInt((2 + 2).W))


  when (io.cfg_ref_inst_valid === 0x1.U){
      ref_table.write(Cat(io.cfg_ref_inst_func, io.cfg_ref_inst_opcode), // Address 
                      Cat(io.cfg_ref_inst_index(1,0),io.cfg_ref_inst_sel_d(1,0)))
  }

  mem_data                                     := ref_table.read(mem_raddr, mem_ren)
  val zeros_3bits                               = WireInit(0.U(3.W))
  io.inst_index                                := Mux(mem_ren_reg === 1.U, Cat(zeros_3bits, mem_data(3,2)), 0.U)
  io.inst_sel_d                                := Mux(mem_ren_reg === 1.U, mem_data(1,0), 0.U)
}
  
  
  