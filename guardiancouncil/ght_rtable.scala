package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_RTABLE_Params(
  totalnumber_of_insts: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_RTABLE_IO (params: GHT_RTABLE_Params) extends Bundle {
  val cfg_ref_inst_func                         = Input(UInt(4.W))
  val cfg_ref_inst_opcode                       = Input(UInt(7.W))
  val cfg_ref_inst_sel_d                        = Input(UInt(4.W))
  val cfg_ref_inst_index                        = Input(UInt(5.W))
  val cfg_ref_inst_valid                        = Input(UInt(1.W))
  val ref_inst_func                             = Output(Vec(params.totalnumber_of_insts, UInt(4.W)))
  val ref_inst_opcode                           = Output(Vec(params.totalnumber_of_insts, UInt(7.W)))
  val ref_inst_sel_d                            = Output(Vec(params.totalnumber_of_insts, UInt(4.W)))
}

trait HasGHT_RTABLE_IO extends BaseModule {
  val params: GHT_RTABLE_Params
  val io = IO(new GHT_RTABLE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_RTABLE (val params: GHT_RTABLE_Params) extends Module with HasGHT_RTABLE_IO
{

  val ref_inst_func_reg                         = RegInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(4.W))))
  val ref_inst_opcode_reg                       = RegInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(7.W))))
  val ref_inst_sel_d_reg                        = RegInit (VecInit(Seq.fill(params.totalnumber_of_insts)(0.U(4.W))))


  when (io.cfg_ref_inst_valid === 1.U){
    ref_inst_func_reg(io.cfg_ref_inst_index)   := io.cfg_ref_inst_func
    ref_inst_opcode_reg(io.cfg_ref_inst_index) := io.cfg_ref_inst_opcode
    ref_inst_sel_d_reg(io.cfg_ref_inst_index)  := io.cfg_ref_inst_sel_d
  }

  for(i <- 0 to params.totalnumber_of_insts - 1) {
    io.ref_inst_func(i)                        := ref_inst_func_reg(i)
    io.ref_inst_opcode(i)                      := ref_inst_opcode_reg(i)
    io.ref_inst_sel_d(i)                       := ref_inst_sel_d_reg(i)
  }

}