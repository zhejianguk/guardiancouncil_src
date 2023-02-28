package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FILTER_PRFS_Params(
  xlen: Int,
  totaltypes_of_insts: Int,
  packet_size: Int,
  use_prfs: Boolean
)

//==========================================================
// I/Os
//==========================================================
class GHT_FILTER_PRFS_IO (params: GHT_FILTER_PRFS_Params) extends Bundle {
  val ght_ft_cfg_in                             = Input(UInt(32.W))
  val ght_ft_cfg_valid                          = Input(UInt(1.W))
  val ght_ft_inst_in                            = Input(UInt(32.W))
  val ght_ft_pc_in                              = Input(UInt(32.W))
  val ght_ft_newcommit_in                       = Input(Bool())
  val ght_ft_alu_in                             = Input(UInt(params.xlen.W))
  val ght_ft_is_rvc_in                          = Input(UInt(1.W))
  val ght_ft_inst_index                         = Output(UInt(5.W))
  val packet_out                                = Output(UInt((params.packet_size).W))
  val ght_prfs_rd                               = Input(UInt(params.xlen.W))
  
  val ght_prfs_forward_ldq                      = Output(Bool())
  val ght_prfs_forward_stq                      = Output(Bool())
  val ght_prfs_forward_ftq                      = Output(Bool())
  val ght_prfs_forward_prf                      = Output(Bool())
}



trait HasGHT_FILTER_PRFS_IO extends BaseModule {
  val params: GHT_FILTER_PRFS_Params
  val io = IO(new GHT_FILTER_PRFS_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_FILTER_PRFS (val params: GHT_FILTER_PRFS_Params) extends Module with HasGHT_FILTER_PRFS_IO
{
  val inst                                      = WireInit(0.U(32.W))
  val func                                      = WireInit(0.U(3.W))
  val opcode                                    = WireInit(0.U(7.W))
  val pc                                        = WireInit(0.U(32.W))
  val is_rvc                                    = WireInit(0.U(1.W))


  val inst_reg                                  = RegInit(0.U(32.W))
  val func_reg                                  = RegInit(0.U(3.W))
  val opcode_reg                                = RegInit(0.U(7.W))
  val dp_ldst_reg                               = RegInit(0.U(params.xlen.W))
  val dp_jump_wire                              = WireInit(0.U(params.xlen.W))
  val pc_reg                                    = RegInit(0.U(32.W))
  val inst_ret                                  = ((inst_reg(6,0) === 0x67.U) && (inst_reg(11,7) === 0x0.U) && (inst_reg(19,15) === 0x01.U))
  val inst_ret_rvc                              = ((inst_reg(6,0) === 0x2.U) && (inst_reg(11,7) === 0x1.U) && (inst_reg(14,12) === 0x0.U) && (inst_reg(15) === 0x1.U))


  inst                                         := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in, 0x0.U)
  func                                         := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(14, 12), 0x0.U)
  opcode                                       := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(6,0), 0x0.U)
  pc                                           := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_pc_in(31,0), 0x0.U)
  is_rvc                                       := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_is_rvc_in, 0x0.U)

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
  u_ght_ftable.io.inst_is_rvc                  := is_rvc

  val inst_index                                = WireInit(0.U(2.W))
  val dp_sel                                    = WireInit(0.U(2.W))

  inst_index                                   := u_ght_ftable.io.inst_index
  dp_sel                                       := u_ght_ftable.io.inst_sel_d


  val packet_zeros                              = WireInit(0.U(64.W))


  if (params.use_prfs){
  val inst_index_reg                            = RegInit(0.U(2.W))
  val dp_sel_reg                                = RegInit(0.U(2.W))
  val pc_reg_delay                              = RegInit(0.U(32.W))
  val inst_reg_delay                            = RegInit(0.U(32.W))
  val inst_ret_delay                            = RegInit(false.B)
  val inst_ret_rvc_delay                        = RegInit(false.B)
  inst_index_reg                               := inst_index
  dp_sel_reg                                   := dp_sel
  pc_reg_delay                                 := pc_reg
  inst_reg_delay                               := inst_reg
  inst_ret_delay                               := inst_ret
  inst_ret_rvc_delay                           := inst_ret_rvc

  dp_jump_wire                                 := Mux(inst_ret_delay|inst_ret_rvc_delay, dp_ldst_reg, io.ght_prfs_rd)

  val i_opcode                                  = inst_reg_delay(6,0);
  val i_rd                                      = inst_reg_delay(11,7);
  val i_func                                    = inst_reg_delay(14,12);
  val i_rs1                                     = inst_reg_delay(19,15);

  val jump_type                                 = WireInit(0.U(2.W))

  jump_type                                    := MuxCase(0.U,
                                                    Array(((i_opcode === 0x6F.U) && (i_rd === 0x01.U)) -> 1.U,
                                                          ((i_opcode === 0x67.U) && (i_rd === 0x01.U)) -> 1.U,
                                                          ((i_opcode === 0x02.U) && (i_rd =/= 0x00.U) && (i_func === 0x01.U) && ((i_rs1&1.U) === 0x01.U)) -> 1.U,
                                                          ((i_opcode === 0x67.U) && (i_rd === 0x00.U) && (i_rs1 === 0x01.U)) -> 2.U,
                                                          ((i_opcode === 0x02.U) && (i_rd === 0x01.U) && (i_func === 0x0.U) && ((i_rs1&1.U) === 0x01.U)) -> 2.U
                                                         )
                                                         )




  io.ght_prfs_forward_ldq                      := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> false.B,
                                                          (dp_sel === 1.U)  -> false.B,
                                                          (dp_sel === 2.U)  -> true.B,
                                                          (dp_sel === 3.U)  -> false.B
                                                          )
                                                          )

  io.ght_prfs_forward_stq                      := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> false.B,
                                                          (dp_sel === 1.U)  -> false.B,
                                                          (dp_sel === 2.U)  -> false.B,
                                                          (dp_sel === 3.U)  -> true.B
                                                          )
                                                          )

  val ght_prfs_forward_prf                      = MuxCase(0.U,
                                                    Array((dp_sel === 0.U)  -> false.B,
                                                          (dp_sel === 1.U)  -> true.B,
                                                          (dp_sel === 2.U)  -> false.B,
                                                          (dp_sel === 3.U)  -> false.B
                                                          )
                                                          )

  io.ght_prfs_forward_prf                      := (ght_prfs_forward_prf === true.B) && (!(inst_ret|inst_ret_rvc))
  io.ght_prfs_forward_ftq                      := (ght_prfs_forward_prf === true.B) && (inst_ret|inst_ret_rvc)

  io.packet_out                                := MuxCase(0.U, 
                                                    Array((dp_sel_reg === 0.U) -> 0.U,
                                                          (dp_sel_reg === 2.U) -> Cat(pc_reg_delay, inst_reg_delay, dp_ldst_reg),
                                                          (dp_sel_reg === 3.U) -> Cat(pc_reg_delay, inst_reg_delay, dp_ldst_reg),
                                                          (dp_sel_reg === 1.U) -> Cat(pc_reg_delay, inst_reg_delay, Cat(dp_jump_wire(61,0), jump_type))
                                                          )
                                                          )
  io.ght_ft_inst_index                         := inst_index_reg
  }

  if (!params.use_prfs){
  io.ght_prfs_forward_ldq                      := 0.U
  io.ght_prfs_forward_stq                      := 0.U
  io.ght_prfs_forward_prf                      := 0.U
  io.ght_prfs_forward_ftq                      := 0.U
  
  io.packet_out                                := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> 0.U,
                                                          (dp_sel =/= 0.U)  -> Cat(pc_reg, inst_reg, dp_ldst_reg)
                                                          )
                                                          )

  io.ght_ft_inst_index                         := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> 0.U,
                                                          (dp_sel =/= 0.U)  -> inst_index
                                                          )
                                                          )
  }
}