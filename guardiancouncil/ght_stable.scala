package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_STABLE_Params(
  number_of_monitored_insts: Int,
  totalnumber_of_insts: Int
)

//==========================================================
// I/Os
//==========================================================
class GHT_STABLE_IO (params: GHT_STABLE_Params) extends Bundle {
  val cfg_sch_end_id                            = Input(UInt(4.W))
  val cfg_sch_policy                            = Input(UInt(7.W))
  val cfg_sch_start_id                          = Input(UInt(4.W))
  val cfg_sch_monitor_inst                      = Input(UInt(7.W))
  val cfg_sch_monitor_index                     = Input(UInt(4.W))
  val cfg_sch_valid                             = Input(UInt(1.W))
  val cfg_sch_ctrl                              = Input(UInt(1.W))
  val sch_end_id                                = Output(UInt(4.W))
  val sch_policy                                = Output(UInt(7.W))
  val sch_start_id                              = Output(UInt(4.W))
  val monitor_inst                              = Output(Vec(params.number_of_monitored_insts, UInt(params.totalnumber_of_insts.W)))
  
}

trait HasGHT_STABLE_IO extends BaseModule {
  val params: GHT_STABLE_Params
  val io = IO(new GHT_STABLE_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_STABLE (val params: GHT_STABLE_Params) extends Module with HasGHT_STABLE_IO
{
  val cfg_sch                                   = WireInit(0.U(1.W))
  val cfg_monitor                               = WireInit(0.U(1.W))
  val sch_end_id_reg                            = RegInit(0.U(4.W))
  val sch_policy_reg                            = RegInit(0.U(7.W))
  val sch_start_id_reg                          = RegInit(0.U(4.W))
  val monitor_inst_reg                          = RegInit(VecInit(Seq.fill(params.number_of_monitored_insts)(0x1F.U(params.totalnumber_of_insts.W))))
  val monitor_inst_wire                         = WireInit(VecInit(Seq.fill(params.number_of_monitored_insts)(0.U(params.totalnumber_of_insts.W))))

  cfg_sch                                      := (io.cfg_sch_valid === 1.U) && (io.cfg_sch_ctrl === 0.U)
  cfg_monitor                                  := (io.cfg_sch_valid === 1.U) && (io.cfg_sch_ctrl === 1.U)

  
  for (i <- 0 to params.number_of_monitored_insts - 1 ){
    monitor_inst_wire(i)                       := MuxCase(monitor_inst_reg(i), 
                                                   Array(((cfg_monitor === 1.U) && (io.cfg_sch_monitor_index === i.U) && (io.cfg_sch_monitor_inst =/= 0x7F.U)) -> (1.U << io.cfg_sch_monitor_inst)
                                                        )
                                                   )                                                     
  }

  when (cfg_sch  === 1.U) {
    sch_end_id_reg                             := io.cfg_sch_end_id
    sch_policy_reg                             := io.cfg_sch_policy
    sch_start_id_reg                           := io.cfg_sch_start_id
  }

  when (cfg_monitor === 1.U) {
    monitor_inst_reg                          := monitor_inst_wire 
  }

  for (i <- 0 to params.number_of_monitored_insts - 1) {
    io.monitor_inst(i)                         := monitor_inst_reg(i)
  }
  io.sch_end_id                                := sch_end_id_reg
  io.sch_policy                                := sch_policy_reg
  io.sch_start_id                              := sch_start_id_reg
}