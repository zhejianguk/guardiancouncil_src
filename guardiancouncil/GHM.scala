package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles}
import freechips.rocketchip.diplomacy._



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
  val agg_packet_out                             = Output(UInt(params.width_GH_packet.W))
  val agg_core_full                              = Input(UInt(1.W))
}

trait HasGHMIO extends BaseModule {
  val params: GHMParams
  val io = IO(new GHMIO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHM (val params: GHMParams) extends Module with HasGHMIO
{
  // Adding a register to avoid the critical path
  val packet_in_reg                              = RegInit (0.U(params.width_GH_packet.W))
  val packet_dest_reg                            = RegInit (0.U((params.number_of_little_cores+1).W))
  val packet_out_wires                           = WireInit (VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.width_GH_packet.W))))
  packet_in_reg                                 := io.ghm_packet_in
  packet_dest_reg                               := io.ghm_packet_dest
  
  var warning                                    = WireInit(0.U(1.W))
  var complete                                   = WireInit(1.U(1.W))
  var release                                    = WireInit(1.U(1.W))
  
  // Routing
  for (i <- 0 to params.number_of_little_cores - 1) {
    when (packet_dest_reg(i) === 1.U) {
      packet_out_wires(i)                       := packet_in_reg
    }
  }

  for(i <- 0 to params.number_of_little_cores - 1) {
    io.ghm_packet_outs(i)                      := packet_out_wires(i)
    io.ghm_status_outs(i)                      := io.ghm_status_in
  }


  for(i <- 0 to params.number_of_little_cores - 1) {
    warning                                     = warning | io.ghe_event_in(i)(0)
    complete                                    = complete & io.ghe_event_in(i)(1)
    release                                     = release & io.ghe_event_in(i)(2)
  }

  io.bigcore_hang                              := warning
  io.bigcore_comp                              := Cat(release, complete)

  // Agg
  val u_agg                                     = Module (new GHT_AGG(GHM_AGG_Params(params.number_of_little_cores, params.width_GH_packet)))
  for (i <- 0 to params.number_of_little_cores - 1) {
    u_agg.io.agg_packet_in(i)                     := io.agg_packet_in(i)
    io.agg_buffer_full(i)                         := u_agg.io.agg_buffer_full(i)
    io.agg_packet_out                             := u_agg.io.agg_packet_out
    u_agg.io.agg_core_full                        := io.agg_core_full
  }

}

case class GHMCoreLocated(loc: HierarchicalLocation) extends Field[Option[GHMParams]](None)

object GHMCore {

  def attach(params: GHMParams, subsystem: BaseSubsystem with HasTiles)
            (implicit p: Parameters) {
    val number_of_ghes                             = subsystem.tile_ghe_packet_in_EPNodes.size

    // Creating nodes for connections.
    val bigcore_hang_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(1.W)))
    val bigcore_comp_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(2.W)))
    val ghm_ght_packet_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt((params.width_GH_packet).W)))
    val ghm_ght_packet_dest_SKNode                 = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    val ghm_ght_status_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    var ghm_ghe_packet_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_status_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_event_in_SKNodes                   = Seq[BundleBridgeSink[UInt]]()

    ghm_ght_packet_in_SKNode                      := subsystem.tile_ght_packet_out_EPNode
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
    var agg_core_full_in_SKNodes                   = Seq[BundleBridgeSink[UInt]]()

    for (i <- 0 to number_of_ghes-1) {
      val agg_packet_in_SKNode                     = BundleBridgeSink[UInt]()
      agg_packet_in_SKNodes                        = agg_packet_in_SKNodes :+ agg_packet_in_SKNode
      agg_packet_in_SKNodes(i)                    := subsystem.tile_agg_packet_out_EPNodes(i)

      val agg_buffer_full_out_SRNode               = BundleBridgeSource[UInt]()
      agg_buffer_full_out_SRNodes                  = agg_buffer_full_out_SRNodes :+ agg_buffer_full_out_SRNode
      subsystem.tile_agg_buffer_full_in_EPNodes(i):= agg_buffer_full_out_SRNodes(i)

      val agg_core_full_in_SKNode                  = BundleBridgeSink[UInt]()
      agg_core_full_in_SKNodes                     = agg_core_full_in_SKNodes :+ agg_core_full_in_SKNode
      agg_core_full_in_SKNodes(i)                 := subsystem.tile_agg_core_full_out_EPNodes(i)
    }


    

    InModuleBody {
      val ghm = Module (new GHM (GHMParams (params.number_of_little_cores, params.width_GH_packet, params.xlen, params.id_agg)))
      ghm.io.ghm_packet_in                        := ghm_ght_packet_in_SKNode.bundle
      ghm.io.ghm_packet_dest                      := ghm_ght_packet_dest_SKNode.bundle
      ghm.io.ghm_status_in                        := ghm_ght_status_in_SKNode.bundle

      for (i <- 0 to number_of_ghes-1) {
        if (i == 0) { // The big core
          // GHE is not connected to the big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := 0.U 
          ghm_ghe_status_out_SRNodes(i).bundle    := 0.U
          agg_buffer_full_out_SRNodes(i).bundle   := 0.U
        } else {// -1 big core
            if ( i == (params.id_agg - 1)) {
              ghm_ghe_packet_out_SRNodes(i).bundle:= ghm.io.agg_packet_out
              ghm.io.agg_core_full                := agg_core_full_in_SKNodes(i).bundle
            }    
            else {
              ghm_ghe_packet_out_SRNodes(i).bundle:= ghm.io.ghm_packet_outs(i-1)
            }
          ghm_ghe_status_out_SRNodes(i).bundle    := ghm.io.ghm_status_outs(i-1)
          ghm.io.ghe_event_in(i-1)                := ghm_ghe_event_in_SKNodes(i).bundle
          ghm.io.agg_packet_in(i-1)               := agg_packet_in_SKNodes(i).bundle
          agg_buffer_full_out_SRNodes(i).bundle   := ghm.io.agg_buffer_full(i-1)
        }
      }

      bigcore_hang_SRNode.bundle                  := ghm.io.bigcore_hang
      bigcore_comp_SRNode.bundle                  := ghm.io.bigcore_comp
    }
  }
}