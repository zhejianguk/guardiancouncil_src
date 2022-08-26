package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FILTER_Params(
  xlen: Int,
  totaltypes_of_insts: Int,
  packet_size: Int,
  use_prfs: Boolean
)

//==========================================================
// I/Os
//==========================================================
class GHT_FILTER_IO (params: GHT_FILTER_Params) extends Bundle {
  val ght_ft_cfg_in                             = Input(UInt(32.W))
  val ght_ft_cfg_valid                          = Input(UInt(1.W))
  val ght_ft_inst_in                            = Input(UInt(32.W))
  val ght_ft_pc_in                              = Input(UInt(32.W))
  val ght_ft_newcommit_in                       = Input(Bool())
  val ght_ft_alu_in                             = Input(UInt(params.xlen.W))
  val ght_ft_inst_index                         = Output(UInt(5.W))
  val packet_out                                = Output(UInt((params.packet_size).W))
  val ght_prfs_rd                               = Input(UInt(params.xlen.W))
}



trait HasGHT_FILTER_IO extends BaseModule {
  val params: GHT_FILTER_Params
  val io = IO(new GHT_FILTER_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_FILTER (val params: GHT_FILTER_Params) extends Module with HasGHT_FILTER_IO
{
  val inst                                      = WireInit(0.U(32.W))
  val func                                      = WireInit(0.U(3.W))
  val opcode                                    = WireInit(0.U(7.W))
  val pc                                        = WireInit(0.U(32.W))


  val inst_reg                                  = RegInit(0.U(32.W))
  val func_reg                                  = RegInit(0.U(3.W))
  val opcode_reg                                = RegInit(0.U(7.W))
  val dp_ldst_reg                               = RegInit(0.U(params.xlen.W))
  val dp_jump_wire                              = WireInit(0.U(params.xlen.W))
  val pc_reg                                    = RegInit(0.U(32.W))
  val inst_ret                                  = ((inst_reg(6,0) === 0x67.U) && (inst_reg(11,7) === 0x0.U))
  val inst_ret_rvc                              = ((inst_reg(6,0) === 0x2.U) && (inst_reg(15) === 0x1.U) && (inst_reg(13, 11) === 0x0.U))


  inst                                         := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in, 0x0.U)
  func                                         := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(14, 12), 0x0.U)
  opcode                                       := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(6,0), 0x0.U)
  pc                                           := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_pc_in(31,0), 0x0.U)

  inst_reg                                     := inst
  func_reg                                     := func
  opcode_reg                                   := opcode
  dp_ldst_reg                                  := io.ght_ft_alu_in
  dp_jump_wire                                 := Mux(inst_ret|inst_ret_rvc, dp_ldst_reg, io.ght_prfs_rd)
  pc_reg                                       := pc

  val u_ght_ftable                              = Module (new GHT_FTABLE(GHT_FTABLE_Params ()))
  u_ght_ftable.io.cfg_ref_inst_func            := this.io.ght_ft_cfg_in(31,28)
  u_ght_ftable.io.cfg_ref_inst_opcode          := this.io.ght_ft_cfg_in(27,21)
  u_ght_ftable.io.cfg_ref_inst_index           := this.io.ght_ft_cfg_in(8,4)
  u_ght_ftable.io.cfg_ref_inst_sel_d           := this.io.ght_ft_cfg_in(20,17)
  u_ght_ftable.io.cfg_ref_inst_valid           := this.io.ght_ft_cfg_valid
  u_ght_ftable.io.inst_newcommit               := this.io.ght_ft_newcommit_in
  u_ght_ftable.io.inst_in_func                 := func
  u_ght_ftable.io.inst_in_opcode               := opcode

  val inst_index                                = WireInit(0.U(5.W))
  val dp_sel                                    = WireInit(0.U(4.W))

  inst_index                                   := u_ght_ftable.io.inst_index
  dp_sel                                       := u_ght_ftable.io.inst_sel_d


  val packet_zeros                              = WireInit(0.U(64.W))

  io.ght_ft_inst_index                         := inst_index

  if (params.use_prfs){ 
  io.packet_out                                := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> 0.U,
                                                          (dp_sel === 1.U)  -> Cat(inst_reg, packet_zeros),
                                                          (dp_sel === 3.U)  -> Cat(pc_reg, inst_reg, dp_ldst_reg),
                                                          (dp_sel === 4.U)  -> Cat(pc_reg, inst_reg, dp_jump_wire)
                                                          )
                                                          )
  }

  if (!params.use_prfs){ 
  io.packet_out                                := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> 0.U,
                                                          (dp_sel === 1.U)  -> Cat(inst_reg, packet_zeros),
                                                          (dp_sel === 3.U)  -> Cat(pc_reg, inst_reg, dp_ldst_reg),
                                                          (dp_sel === 4.U)  -> Cat(pc_reg, inst_reg, dp_ldst_reg)
                                                          )
                                                          )
  }
}