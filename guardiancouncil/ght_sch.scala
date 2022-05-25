package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_SCH_Params(
  number_of_monitored_insts: Int,
  totalnumber_of_checkers: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_SCH_IO (params: GHT_SCH_Params) extends Bundle {
  val core_s       = Input(UInt(log2Up(params.totalnumber_of_checkers).W))
  val core_e       = Input(UInt(log2Up(params.totalnumber_of_checkers).W))
  val monitor_inst = Input(Vec(params.number_of_monitored_insts, UInt(32.W)))
  val inst_type    = Input(UInt(32.W))
  val core_d       = Output(UInt(params.totalnumber_of_checkers.W))
}




trait HasGHT_SCH_IO extends BaseModule {
  val params: GHT_SCH_Params
  val io = IO(new GHT_SCH_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_SCH_RR (val params: GHT_SCH_Params) extends Module with HasGHT_SCH_IO
{
  var new_packet   = WireInit(false.B)
  var core_dest    = WireInit(0.U(params.totalnumber_of_checkers.W))

  for (i <- 0 to params.number_of_monitored_insts - 1) {
    new_packet     = new_packet | (io.inst_type === io.monitor_inst(i))
  }

  val dest_last    = RegInit(0.U(log2Up(params.totalnumber_of_checkers).W))
  val dest         = WireInit(0.U(log2Up(params.totalnumber_of_checkers).W))
  dest            := MuxCase(0.U, 
                       Array((new_packet & (dest_last === 0.U)) -> io.core_s,
                             (new_packet & (dest_last === io.core_e)) -> io.core_s,
                             (new_packet & (dest_last =/= io.core_e)) -> (dest_last + 1.U)
                            ))

  when (new_packet === true.B) {
      dest_last := dest
  } .otherwise {
      dest_last := dest_last
  }

  core_dest       := MuxCase(0.U, 
                      Array((dest =/= 0.U) -> (1.U << dest)
                           ))

  io.core_d       := core_dest

}