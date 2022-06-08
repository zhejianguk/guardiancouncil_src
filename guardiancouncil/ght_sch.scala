package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_SCH_Params(
  totalnumber_of_checkers: Int,
  number_of_monitored_insts: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_SCH_IO (params: GHT_SCH_Params) extends Bundle {
  val core_s                                    = Input(UInt(4.W))
  val core_e                                    = Input(UInt(4.W))
  val monitor_inst                              = Input(Vec(params.number_of_monitored_insts, UInt(32.W)))
  val inst_type                                 = Input(UInt(32.W))
  val core_d                                    = Output(UInt(params.totalnumber_of_checkers.W))
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
  var core_dest                                 = WireInit(0.U(params.totalnumber_of_checkers.W))
  var new_packet                                = WireInit(false.B)

  for (i <- 0 to params.number_of_monitored_insts - 1) {
    new_packet                                  = new_packet | (io.inst_type === io.monitor_inst(i))
  }

  val dest_last                                 = RegInit(0.U(4.W))
  val dest                                      = WireInit(0.U(4.W))
  val out_of_range                              = WireInit(0.U(1.W))
  out_of_range                                 := (dest_last < io.core_s) || (dest_last > io.core_e)

  dest                                         := MuxCase(0.U, 
                                                    Array((new_packet & (out_of_range === 1.U)) -> io.core_s,
                                                          (new_packet & (out_of_range === 0.U) & (dest_last === io.core_e)) -> io.core_s,
                                                          (new_packet & (out_of_range === 0.U) & (dest_last =/= io.core_e)) -> (dest_last + 1.U)
                                                          ))

  when (new_packet === true.B) {
      dest_last                                := dest
  } .otherwise {
      dest_last                                := dest_last
  }

  core_dest                                    := MuxCase(0.U, 
                                                    Array((dest =/= 0.U) -> (1.U << (dest - 1.U)),
                                                          (dest === 0.U) -> 0.U
                                                         ))

  io.core_d                                    := core_dest

}

//==========================================================
class GHT_SCH_RRF (val params: GHT_SCH_Params) extends Module with HasGHT_SCH_IO
{
  var core_dest                                 = WireInit(0.U(params.totalnumber_of_checkers.W))
  var new_packet                                = WireInit(false.B)

  for (i <- 0 to params.number_of_monitored_insts - 1) {
    new_packet                                  = new_packet | (io.inst_type === io.monitor_inst(i))
  }

  val t_counter                                 = RegInit(0.U(2.W))
  when (new_packet === true.B) {
    t_counter                                  := t_counter + 1.U
  } .otherwise {
    t_counter                                  := t_counter
  }

  val dest_last                                 = RegInit(0.U(4.W))
  val dest                                      = WireInit(0.U(4.W))
  val out_of_range                              = WireInit(0.U(1.W))
  out_of_range                                 := (dest_last < io.core_s) || (dest_last > io.core_e)

  dest                                         := MuxCase(0.U, 
                                                    Array((new_packet & (out_of_range === 1.U)) -> io.core_s,
                                                          (new_packet & (out_of_range === 0.U) & (t_counter =/= 3.U)) -> dest_last,
                                                          (new_packet & (out_of_range === 0.U) & (dest_last === io.core_e) & (t_counter === 3.U)) -> io.core_s,
                                                          (new_packet & (out_of_range === 0.U) & (dest_last =/= io.core_e) & (t_counter === 3.U)) -> (dest_last + 1.U)
                                                          ))

  when (new_packet === true.B) {
      dest_last                                := dest
  } .otherwise {
      dest_last                                := dest_last
  }

  core_dest                                    := MuxCase(0.U, 
                                                  Array((dest =/= 0.U) -> (1.U << (dest - 1.U)),
                                                        (dest === 0.U) -> 0.U
                                                       ))

  io.core_d                                    := core_dest

}