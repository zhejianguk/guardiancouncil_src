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
  val ghe_event_in                               = Input(Vec(params.number_of_little_cores, UInt(4.W)))
  val bigcore_hang                               = Output(UInt(1.W))
  val bigcore_comp                               = Output(UInt(3.W))

  val debug_gcounter                             = Output(UInt(64.W))
  val if_agg_free                                = Input(UInt(1.W))
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

    // Adding a register to avoid the critical path
    val packet_in_reg                              = RegInit (0.U(params.width_GH_packet.W))
    val packet_dest_reg                            = RegInit (0.U((params.number_of_little_cores+1).W))
    val packet_out_wires                           = WireInit (VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.width_GH_packet.W))))
    packet_in_reg                                 := io.ghm_packet_in
    packet_dest_reg                               := io.ghm_packet_dest
    
    var warning                                    = WireInit(0.U(1.W))
    var complete                                   = WireInit(1.U(1.W))
    var release                                    = WireInit(1.U(1.W))
    var initalised                                 = WireInit(1.U(1.W))

    val debug_gcounter                             = RegInit (0.U(64.W))

    
    
    // Routing
    for (i <- 0 to params.number_of_little_cores - 1) {
      packet_out_wires(i)                         := MuxCase(0.U, 
                                                      Array((packet_dest_reg(i) === 1.U) -> packet_in_reg,
                                                            (packet_dest_reg(i) =/= 1.U) -> 0.U,
                                                            )
                                                            )
    }

    // 2-cyecle delays are added to ensure all filtering activities are completed.
    val ghm_status_delay1_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay2_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay3_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay4_cycle                    = RegInit(0.U(5.W))
    ghm_status_delay1_cycle                       := io.ghm_status_in(4,0)
    ghm_status_delay2_cycle                       := ghm_status_delay1_cycle
    ghm_status_delay3_cycle                       := ghm_status_delay2_cycle
    ghm_status_delay4_cycle                       := ghm_status_delay3_cycle

    val if_filters_empty                           = io.ghm_status_in(31)
    val if_ghm_empty                               = Mux(((packet_dest_reg === 0.U) && (io.ghm_packet_dest === 0.U)), 1.U, 0.U)

    val if_no_inflight_packets                     = if_filters_empty & if_ghm_empty & io.if_agg_free

    val zeros_59bit                                = WireInit(0.U(59.W))
    for(i <- 0 to params.number_of_little_cores - 1) {
      io.ghm_packet_outs(i)                       := packet_out_wires(i)
      io.ghm_status_outs(i)                       := Mux((if_no_inflight_packets === 1.U), Cat(zeros_59bit, ghm_status_delay4_cycle), 1.U)
    }


    for(i <- 0 to params.number_of_little_cores - 1) {
      warning                                     = warning | io.ghe_event_in(i)(0)
      complete                                    = complete & io.ghe_event_in(i)(1)
      release                                     = release & io.ghe_event_in(i)(2)
      initalised                                  = initalised | io.ghe_event_in(i)(3)
    }

    when (io.ghm_packet_dest =/= 0.U) {
      debug_gcounter                             := debug_gcounter + 1.U
    }

    io.bigcore_hang                              := warning
    io.bigcore_comp                              := Cat(initalised, release, complete)

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
    val bigcore_comp_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(3.W)))
    val ghm_ght_packet_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt((params.width_GH_packet).W)))
    val ghm_ght_packet_dest_SKNode                 = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    val ghm_ght_status_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    var ghm_ghe_packet_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_status_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_event_in_SKNodes                   = Seq[BundleBridgeSink[UInt]]()

    val if_agg_free_SKNode                        = BundleBridgeSink[UInt](Some(() => UInt(1.W)))

    ghm_ght_packet_in_SKNode                      := subsystem.tile_ght_packet_out_EPNode
    ghm_ght_packet_dest_SKNode                    := subsystem.tile_ght_packet_dest_EPNode
    ghm_ght_status_in_SKNode                      := subsystem.tile_ght_status_out_EPNode

    if_agg_free_SKNode                            := subsystem.tile_agg_free_EPNode
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

    val debug_gcounter_SRNode                      = BundleBridgeSource[UInt](Some(() => UInt(64.W)))
    subsystem.tile_debug_gcounter_EPNode          := debug_gcounter_SRNode
    
    val bus = subsystem.locateTLBusWrapper(where)
    val ghm = LazyModule (new GHM (GHMParams (params.number_of_little_cores, params.width_GH_packet, params.xlen, params.id_agg)))

    
    InModuleBody {
      ghm.module.clock                            := bus.module.clock
      ghm.module.io.ghm_packet_in                 := ghm_ght_packet_in_SKNode.bundle
      ghm.module.io.ghm_packet_dest               := ghm_ght_packet_dest_SKNode.bundle
      ghm.module.io.ghm_status_in                 := ghm_ght_status_in_SKNode.bundle
      ghm.module.io.if_agg_free                   := if_agg_free_SKNode.bundle 

      for (i <- 0 to number_of_ghes-1) {
        if (i == 0) { // The big core
          // GHE is not connected to the big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := 0.U 
          ghm_ghe_status_out_SRNodes(i).bundle    := 0.U
        } else {// -1 big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := ghm.module.io.ghm_packet_outs(i-1)
          ghm_ghe_status_out_SRNodes(i).bundle    := ghm.module.io.ghm_status_outs(i-1)
          ghm.module.io.ghe_event_in(i-1)         := ghm_ghe_event_in_SKNodes(i).bundle
        }
      }

      bigcore_hang_SRNode.bundle                  := ghm.module.io.bigcore_hang
      bigcore_comp_SRNode.bundle                  := ghm.module.io.bigcore_comp
      debug_gcounter_SRNode.bundle                := ghm.module.io.debug_gcounter
    }
    ghm
  }
}