package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_MAPPER_Params(
  totaltypes_of_insts: Int,
  totalnumber_of_ses: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_MAPPER_IO (params: GHT_MAPPER_Params) extends Bundle {
  val ght_mp_cfg_in                             = Input(UInt(32.W))
  val ght_mp_cfg_valid                          = Input(UInt(1.W))
  val inst_index                                = Input(UInt(5.W))
  val inst_c                                    = Output(Vec(params.totalnumber_of_ses, UInt(1.W)))
}



trait HasGHT_MAPPER_IO extends BaseModule {
  val params: GHT_MAPPER_Params
  val io = IO(new GHT_MAPPER_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_MAPPER (val params: GHT_MAPPER_Params) extends Module with HasGHT_MAPPER_IO
{
  val u_ght_mtable                              = Module (new GHT_MTABLE(GHT_MTABLE_Params (params.totaltypes_of_insts, params.totalnumber_of_ses)))
  val inst_type_ses                             = WireInit(VecInit(Seq.fill(params.totaltypes_of_insts)(0.U(params.totalnumber_of_ses.W))))
  u_ght_mtable.io.cfg_map_SEs                  := this.io.ght_mp_cfg_in(31,16)
  u_ght_mtable.io.cfg_map_inst_type            := this.io.ght_mp_cfg_in(8,4)
  u_ght_mtable.io.cfg_map_valid                := this.io.ght_mp_cfg_valid
  
  for (i <- 0 to params.totaltypes_of_insts - 1 ){
    inst_type_ses(i)                           := u_ght_mtable.io.inst_type_ses(i)
  }

  for (i <- 0 to params.totalnumber_of_ses - 1){
    io.inst_c(i)                               := inst_type_ses(io.inst_index)(i)
  }

}