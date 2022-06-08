package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FILTER_Params(
  xlen: Int,
  totalnumber_of_insts: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_FILTER_IO (params: GHT_FILTER_Params) extends Bundle {
  val ght_ft_cfg_in                             = Input(UInt(32.W))
  val ght_ft_cfg_valid                          = Input(UInt(1.W))
  val ght_ft_inst_in                            = Input(UInt(32.W))
  val ght_ft_newcommit_in                       = Input(Bool())
  val ght_ft_alu_in                             = Input(UInt(params.xlen.W))
  val ght_ft_inst_type                          = Output(UInt((params.totalnumber_of_insts).W))
  val packet_out                                = Output(UInt(74.W))
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
  val u_ght_rtable                              = Module (new GHT_RTABLE(GHT_RTABLE_Params (params.totalnumber_of_insts)))
  u_ght_rtable.io.cfg_ref_inst_func            := this.io.ght_ft_cfg_in(31,28)
  u_ght_rtable.io.cfg_ref_inst_opcode          := this.io.ght_ft_cfg_in(27,21)
  u_ght_rtable.io.cfg_ref_inst_sel_d           := this.io.ght_ft_cfg_in(20,17)
  u_ght_rtable.io.cfg_ref_inst_index           := this.io.ght_ft_cfg_in(8,4)
  u_ght_rtable.io.cfg_ref_inst_valid           := this.io.ght_ft_cfg_valid


  val func                                      = WireInit(0.U(3.W))
  val opcode                                    = WireInit(0.U(7.W))
  val dp_sel                                    = WireInit(0.U(4.W))
  val inst_type                                 = WireInit(0.U((params.totalnumber_of_insts).W))
  var index                                     = WireInit(0.U(8.W))

  func                                         := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(14, 12), 0x7.U)
  opcode                                       := Mux((io.ght_ft_newcommit_in === true.B), io.ght_ft_inst_in(6,0), 0x7F.U)

  val hit_func                                  = WireInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(1.W))))
  val hit_opcode                                = WireInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(1.W))))
  val hit_inst                                  = WireInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U((params.totalnumber_of_insts).W))))
  val inst_sel_d                                = WireInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(4.W))))

  val packet_zeros                              = WireInit(0.U(64.W))

  for(i <- 0 to params.totalnumber_of_insts - 1) {

    hit_func(i)                                := Mux((u_ght_rtable.io.ref_inst_func(i)(3) === 1.U) || // do not care the func code 
                                                      (u_ght_rtable.io.ref_inst_func(i)(2,0) === func),
                                                      1.U, 0.U)

    hit_opcode(i)                              := Mux((u_ght_rtable.io.ref_inst_opcode(i) === opcode),
                                                      1.U, 0.U)

    hit_inst(i)                                := Mux(((hit_func(i) & hit_opcode(i)) === 1.U),
                                                      (1.U << i), 0.U)
    
    inst_sel_d(i)                              := Mux(((hit_func(i) & hit_opcode(i)) === 1.U),
                                                      u_ght_rtable.io.ref_inst_sel_d(i), 0.U)
  }

  val u_dp_sel_orgate                           = Module (new GH_ORGATE(ORGATEParams (params.totalnumber_of_insts, params.totalnumber_of_insts)))
  val u_dp_inst_type_orgate                     = Module (new GH_ORGATE(ORGATEParams (params.totalnumber_of_insts, params.totalnumber_of_insts)))
  for(i <- 0 to params.totalnumber_of_insts - 1) {
    u_dp_sel_orgate.io.in(i)                   := inst_sel_d(i)
    u_dp_inst_type_orgate.io.in(i)             := hit_inst(i)
  }
  dp_sel                                       := u_dp_sel_orgate.io.out
  inst_type                                    := u_dp_inst_type_orgate.io.out

  io.ght_ft_inst_type                          := inst_type

  io.packet_out                                := MuxCase(0.U, 
                                                    Array((dp_sel === 0.U)  -> Cat(func, opcode, packet_zeros),
                                                          (dp_sel === 1.U)  -> Cat(func, opcode, io.ght_ft_alu_in) 
                                                          )
                                                          ) 
}