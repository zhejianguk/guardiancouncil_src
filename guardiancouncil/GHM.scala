package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles, TLBusWrapperLocation}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain}



case class GHMParams(
  number_of_little_cores: Int,
  width_GH_packet: Int, 
  xlen: Int,
  id_agg: Int
)


class GHMIO(params: GHMParams) extends Bundle {
  val ghm_packet_in                              = Input(UInt(params.width_GH_packet.W))
  val ghm_packet_dest                            = Input(UInt((params.number_of_little_cores+1).W))
  val ghm_status_in                              = Input(UInt(32.W))
  val ghm_packet_outs                            = Output(Vec(params.number_of_little_cores, UInt(params.width_GH_packet.W)))
  val ghm_status_outs                            = Output(Vec(params.number_of_little_cores, UInt(32.W)))
  val ghe_event_in                               = Input(Vec(params.number_of_little_cores, UInt(3.W)))
  val bigcore_hang                               = Output(UInt(1.W))
  val bigcore_comp                               = Output(UInt(2.W))

  val agg_packet_in                              = Input(Vec(params.number_of_little_cores, UInt(params.width_GH_packet.W)))
  val agg_buffer_full                            = Output(Vec(params.number_of_little_cores, UInt(1.W)))
  val agg_core_status                            = Input(Vec(params.number_of_little_cores,UInt(2.W)))
  val agg_core_id                                = Input(UInt(16.W))

  val sch_na_in                                  = Input(Vec(params.number_of_little_cores, UInt(1.W)))
  val sch_na_out                                 = Output(UInt(params.number_of_little_cores.W))
  val sch_do_refresh                             = Input(Vec(params.number_of_little_cores, UInt(32.W)))
  val sch_refresh_out                            = Output(Vec(params.number_of_little_cores, UInt(1.W)))

  val debug_gcounter                             = Output(UInt(64.W))
}

trait HasGHMIO extends BaseModule {
  val params: GHMParams
  val io = IO(new GHMIO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHM (val params: GHMParams)(implicit p: Parameters) extends LazyModule
{
  lazy val module = new LazyModuleImp(this) {
    val io                                         = IO(new GHMIO(params))
    val u_agg                                      = Module (new GHT_AGG(GHM_AGG_Params(params.number_of_little_cores, params.width_GH_packet)))

    // Adding a register to avoid the critical path
    val packet_in_reg                              = RegInit (0.U(params.width_GH_packet.W))
    val packet_dest_reg                            = RegInit (0.U((params.number_of_little_cores+1).W))
    val packet_out_wires                           = WireInit (VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.width_GH_packet.W))))
    packet_in_reg                                 := io.ghm_packet_in
    packet_dest_reg                               := io.ghm_packet_dest
    
    var warning                                    = WireInit(0.U(1.W))
    var complete                                   = WireInit(1.U(1.W))
    var release                                    = WireInit(1.U(1.W))
    var sch_na                                     = WireInit(0.U(params.number_of_little_cores.W))

    val sch_na_in_wires                            = WireInit(VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.number_of_little_cores.W))))
    val zeros_nbit                                 = WireInit(0.U((params.number_of_little_cores - 1).W))
    val agg_core_id                                = io.agg_core_id - 1.U
    val do_refresh                                 = WireInit(0.U(32.W))

    val debug_gcounter                             = RegInit(0.U(64.W))

    
    
    // Routing
    for (i <- 0 to params.number_of_little_cores - 1) {
      packet_out_wires(i)                         := MuxCase(0.U, 
                                                      Array(((packet_dest_reg(i) === 1.U) && (agg_core_id =/= i.U)) -> packet_in_reg,
                                                            ((packet_dest_reg(i) =/= 1.U) && (agg_core_id =/= i.U)) -> 0.U,
                                                            (agg_core_id === i.U) -> u_agg.io.agg_packet_out
                                                            )
                                                            ) 
      u_agg.io.agg_packet_in(i)                   := io.agg_packet_in(i)
      io.agg_buffer_full(i)                       := u_agg.io.agg_buffer_full(i)
    }
    u_agg.io.agg_core_full                        := io.agg_core_status(agg_core_id)(1)
    do_refresh                                    := io.sch_do_refresh(agg_core_id)

    // 2-cyecle delays are added to ensure all filtering activities are completed.
    val ghm_status_delay1_cycle                    = RegInit(0.U(2.W))
    val ghm_status_delay2_cycle                    = RegInit(0.U(2.W))
    val ghm_status_delay3_cycle                    = RegInit(0.U(2.W))
    val ghm_status_delay4_cycle                    = RegInit(0.U(2.W))
    ghm_status_delay1_cycle                       := io.ghm_status_in(1,0)
    ghm_status_delay2_cycle                       := ghm_status_delay1_cycle
    ghm_status_delay3_cycle                       := ghm_status_delay2_cycle
    ghm_status_delay4_cycle                       := ghm_status_delay3_cycle


    val collecting_checker_status                  = io.agg_core_status.reduce(_&_)
    val if_checkers_empty                          = collecting_checker_status(0)
    val if_filters_empty                           = io.ghm_status_in(31)
    val if_ghm_empty                               = Mux(((packet_dest_reg === 0.U) && (io.ghm_packet_dest === 0.U)), 1.U, 0.U)

    val if_no_inflight_packets                     = if_checkers_empty & if_filters_empty & if_ghm_empty
    val zeros_62bit                                = WireInit(0.U(62.W))
    for(i <- 0 to params.number_of_little_cores - 1) {
      io.ghm_packet_outs(i)                       := packet_out_wires(i)
      io.ghm_status_outs(i)                       := Mux((if_no_inflight_packets === 1.U), Cat(zeros_62bit, ghm_status_delay4_cycle), 1.U)
      sch_na_in_wires(i)                          := Cat(zeros_nbit, io.sch_na_in(i))
      io.sch_refresh_out(i)                       := do_refresh(i)
    }


    for(i <- 0 to params.number_of_little_cores - 1) {
      warning                                     = warning | io.ghe_event_in(i)(0)
      complete                                    = complete & io.ghe_event_in(i)(1)
      release                                     = release & io.ghe_event_in(i)(2)
      sch_na                                      = sch_na | (sch_na_in_wires(i) << i)
    }

    when (io.ghm_packet_dest =/= 0.U) {
      debug_gcounter                             := debug_gcounter + 1.U
    }

    io.bigcore_hang                              := warning
    io.bigcore_comp                              := Cat(release, complete)
    io.sch_na_out                                := sch_na

    io.debug_gcounter                            := debug_gcounter
  }
}

case class GHMCoreLocated(loc: HierarchicalLocation) extends Field[Option[GHMParams]](None)

object GHMCore {

  def attach(params: GHMParams, subsystem: BaseSubsystem with HasTiles, where: TLBusWrapperLocation)
            (implicit p: Parameters) {
    val number_of_ghes                             = subsystem.tile_ghe_packet_in_EPNodes.size
  
    // Creating nodes for connections.
    val bigcore_hang_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(1.W)))
    val bigcore_comp_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(2.W)))
    val ghm_ght_packet_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt((params.width_GH_packet).W)))
    val ghm_agg_core_id_SKNode                     = BundleBridgeSink[UInt](Some(() => UInt(16.W)))
    val ghm_ght_packet_dest_SKNode                 = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    val ghm_ght_status_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    var ghm_ghe_packet_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_status_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_event_in_SKNodes                   = Seq[BundleBridgeSink[UInt]]()

    ghm_ght_packet_in_SKNode                      := subsystem.tile_ght_packet_out_EPNode
    ghm_agg_core_id_SKNode                        := subsystem.tile_ghm_agg_core_id_EPNode
    ghm_ght_packet_dest_SKNode                    := subsystem.tile_ght_packet_dest_EPNode
    ghm_ght_status_in_SKNode                      := subsystem.tile_ght_status_out_EPNode

    for (i <- 0 to number_of_ghes-1) {
      val ghm_ghe_packet_out_SRNode                = BundleBridgeSource[UInt]()
      ghm_ghe_packet_out_SRNodes                   = ghm_ghe_packet_out_SRNodes :+ ghm_ghe_packet_out_SRNode
      subsystem.tile_ghe_packet_in_EPNodes(i)     := ghm_ghe_packet_out_SRNodes(i)

      val ghm_ghe_status_out_SRNode                = BundleBridgeSource[UInt]()
      ghm_ghe_status_out_SRNodes                   = ghm_ghe_status_out_SRNodes :+ ghm_ghe_status_out_SRNode
      subsystem.tile_ghe_status_in_EPNodes(i)     := ghm_ghe_status_out_SRNodes(i)

      val ghm_ghe_event_in_SkNode                  = BundleBridgeSink[UInt]()
      ghm_ghe_event_in_SKNodes                     = ghm_ghe_event_in_SKNodes :+ ghm_ghe_event_in_SkNode
      ghm_ghe_event_in_SKNodes(i)                 := subsystem.tile_ghe_event_out_EPNodes(i)
    }
    subsystem.tile_bigcore_comp_EPNode            := bigcore_comp_SRNode
    subsystem.tile_bigcore_hang_EPNode            := bigcore_hang_SRNode

    // Agg
    var agg_packet_in_SKNodes                      = Seq[BundleBridgeSink[UInt]]()
    var agg_buffer_full_out_SRNodes                = Seq[BundleBridgeSource[UInt]]()
    var agg_core_status_in_SKNodes                 = Seq[BundleBridgeSink[UInt]]()

    var ghm_ght_sch_na_in_SKNodes                  = Seq[BundleBridgeSink[UInt]]()
    var ghm_ghe_sch_refresh_out_SRNodes            = Seq[BundleBridgeSource[UInt]]()
    var ghm_ght_sch_dorefresh_in_SKNodes           = Seq[BundleBridgeSink[UInt]]()

    for (i <- 0 to number_of_ghes-1) {
      val agg_packet_in_SKNode                     = BundleBridgeSink[UInt]()
      agg_packet_in_SKNodes                        = agg_packet_in_SKNodes :+ agg_packet_in_SKNode
      agg_packet_in_SKNodes(i)                    := subsystem.tile_agg_packet_out_EPNodes(i)

      val agg_buffer_full_out_SRNode               = BundleBridgeSource[UInt]()
      agg_buffer_full_out_SRNodes                  = agg_buffer_full_out_SRNodes :+ agg_buffer_full_out_SRNode
      subsystem.tile_agg_buffer_full_in_EPNodes(i):= agg_buffer_full_out_SRNodes(i)

      val agg_core_status_in_SKNode                = BundleBridgeSink[UInt]()
      agg_core_status_in_SKNodes                   = agg_core_status_in_SKNodes :+ agg_core_status_in_SKNode
      agg_core_status_in_SKNodes(i)               := subsystem.tile_agg_core_status_out_EPNodes(i)

      val ghm_ght_sch_na_in_SKNode                 = BundleBridgeSink[UInt]()
      ghm_ght_sch_na_in_SKNodes                    = ghm_ght_sch_na_in_SKNodes :+ ghm_ght_sch_na_in_SKNode
      ghm_ght_sch_na_in_SKNodes(i)                := subsystem.tile_ght_sch_na_out_EPNodes(i)

      val ghm_ghe_sch_refresh_out_SRNode           = BundleBridgeSource[UInt]()
      ghm_ghe_sch_refresh_out_SRNodes              = ghm_ghe_sch_refresh_out_SRNodes :+ ghm_ghe_sch_refresh_out_SRNode
      subsystem.tile_ghe_sch_refresh_in_EPNodes(i):= ghm_ghe_sch_refresh_out_SRNodes(i)

      val ghm_ght_sch_dorefresh_in_SKNode          = BundleBridgeSink[UInt]()
      ghm_ght_sch_dorefresh_in_SKNodes             = ghm_ght_sch_dorefresh_in_SKNodes :+ ghm_ght_sch_dorefresh_in_SKNode
      ghm_ght_sch_dorefresh_in_SKNodes(i)         := subsystem.tile_ght_sch_dorefresh_EPNodes(i)
    }

    val sch_na_SRNode                              = BundleBridgeSource[UInt](Some(() => UInt(16.W)))
    subsystem.tile_sch_na_EPNode                  := sch_na_SRNode

    val debug_gcounter_SRNode                      = BundleBridgeSource[UInt](Some(() => UInt(64.W)))
    subsystem.tile_debug_gcounter_EPNode          := debug_gcounter_SRNode
    
    val bus = subsystem.locateTLBusWrapper(where)
    val ghm = LazyModule (new GHM (GHMParams (params.number_of_little_cores, params.width_GH_packet, params.xlen, params.id_agg)))

    
    InModuleBody {
      ghm.module.clock                            := bus.module.clock
      ghm.module.io.ghm_packet_in                 := ghm_ght_packet_in_SKNode.bundle
      ghm.module.io.ghm_packet_dest               := ghm_ght_packet_dest_SKNode.bundle
      ghm.module.io.ghm_status_in                 := ghm_ght_status_in_SKNode.bundle
      ghm.module.io.agg_core_id                   := ghm_agg_core_id_SKNode.bundle 

      for (i <- 0 to number_of_ghes-1) {
        if (i == 0) { // The big core
          // GHE is not connected to the big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := 0.U 
          ghm_ghe_status_out_SRNodes(i).bundle    := 0.U
          agg_buffer_full_out_SRNodes(i).bundle   := 0.U
        } else {// -1 big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := ghm.module.io.ghm_packet_outs(i-1)
          ghm.module.io.agg_core_status(i-1)      := agg_core_status_in_SKNodes(i).bundle
          ghm.module.io.sch_do_refresh(i-1)       := ghm_ght_sch_dorefresh_in_SKNodes(i).bundle
          ghm_ghe_status_out_SRNodes(i).bundle    := ghm.module.io.ghm_status_outs(i-1)
          ghm.module.io.ghe_event_in(i-1)         := ghm_ghe_event_in_SKNodes(i).bundle
          ghm.module.io.agg_packet_in(i-1)        := agg_packet_in_SKNodes(i).bundle
          agg_buffer_full_out_SRNodes(i).bundle   := ghm.module.io.agg_buffer_full(i-1)
          ghm.module.io.sch_na_in(i-1)            := ghm_ght_sch_na_in_SKNodes(i).bundle
          ghm_ghe_sch_refresh_out_SRNodes(i).bundle:= ghm.module.io.sch_refresh_out(i-1)
        }
      }

      bigcore_hang_SRNode.bundle                  := ghm.module.io.bigcore_hang
      bigcore_comp_SRNode.bundle                  := ghm.module.io.bigcore_comp
      sch_na_SRNode.bundle                        := ghm.module.io.sch_na_out
      debug_gcounter_SRNode.bundle                := ghm.module.io.debug_gcounter
    }
    ghm
  }
}