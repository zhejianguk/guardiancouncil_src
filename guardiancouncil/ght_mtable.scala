package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_MTABLE_Params(
  totaltypes_of_insts: Int,
  totalnumber_of_ses: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_MTABLE_IO (params: GHT_MTABLE_Params) extends Bundle {
  val cfg_map_inst_type                         = Input(UInt(4.W))
  val cfg_map_SEs                               = Input(UInt(16.W))
  val cfg_map_valid                             = Input(UInt(1.W))

  val inst_type_ses                             = Output(Vec(params.totaltypes_of_insts, UInt(params.totalnumber_of_ses.W)))
}

trait HasGHT_MTABLE_IO extends BaseModule {
  val params: GHT_MTABLE_Params
  val io = IO(new GHT_MTABLE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_MTABLE (val params: GHT_MTABLE_Params) extends Module with HasGHT_MTABLE_IO
{
  val inst_type_ses                             = RegInit(VecInit(Seq.fill(params.totaltypes_of_insts)(0.U(params.totalnumber_of_ses.W))))

  for (i <- 0 to params.totaltypes_of_insts - 1 ){
    when ((io.cfg_map_valid === 1.U) && (io.cfg_map_inst_type  === i.U)) {
      inst_type_ses(i)                         := io.cfg_map_SEs
    }
  }

  for (i <- 0 to params.totaltypes_of_insts - 1 ){
    io.inst_type_ses(i)                       := inst_type_ses(i)
  }
}