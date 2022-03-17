package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles}
import freechips.rocketchip.diplomacy._



case class GHMParams(
  number_of_little_cores: Int,
  width_GH_packet: Int, // Default: 202: 3: func; 7: opcode; 64 D, 64 S, 64 S.
  xlen: Int
)


class GHMIO(params: GHMParams) extends Bundle {
  val ghm_packet_in             = Input(UInt(params.width_GH_packet.W))
  val ghm_packet_outs           = Output(Vec(params.number_of_little_cores, UInt(params.width_GH_packet.W)))
  val ghe_status_warning_in     = Input(Vec(params.number_of_little_cores, UInt(1.W)))
  val bigcore_hang              = Output(UInt(1.W))
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
  val packet_in_reg             = RegInit (0.U(params.width_GH_packet.W))
  val packet_out_wires          = WireInit (VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.width_GH_packet.W))))

  val func                      = packet_in_reg(params.xlen+9, params.xlen+7)
  val opcode                    = packet_in_reg(params.xlen+6, params.xlen)
  val big_core_rs2              = packet_in_reg(params.xlen-1,   0)
  val little_core_dest          = WireInit(0.U(log2Up(params.number_of_little_cores).W))
  val new_packet                = WireInit(false.B)
  var warning                   = 0.U;


  val u_ghm_sch                 = Module (new GHM_SCH(GHM_SCH_Params (params.number_of_little_cores)))
  u_ghm_sch.io.big_core_func   := func
  u_ghm_sch.io.big_core_opcode := opcode
  little_core_dest             := u_ghm_sch.io.dest_little_core_id
  new_packet                   := Mux(Cat(func, opcode) =/= 0.U, true.B, false.B)

  when (new_packet === true.B) {
    packet_out_wires(little_core_dest) := packet_in_reg
  }

  packet_in_reg                := io.ghm_packet_in

  for(i <- 0 to params.number_of_little_cores - 1) {
    io.ghm_packet_outs(i)      := packet_out_wires(i)
  }


  for(i <- 0 to params.number_of_little_cores - 1) {
    warning                     = warning | io.ghe_status_warning_in(i) 
  }

  io.bigcore_hang              := Mux(warning =/=0.U, 
                                      1.U,
                                      0.U)
}

case class GHMCoreLocated(loc: HierarchicalLocation) extends Field[Option[GHMParams]](None)

object GHMCore {

  def attach(params: GHMParams, subsystem: BaseSubsystem with HasTiles)
            (implicit p: Parameters) {
    val number_of_ghes            = subsystem.tile_ghe_packet_in_EPNodes.size

    // Creating nodes for connections.
    val bigcore_hang_SRNode       = BundleBridgeSource[UInt](Some(() => UInt(1.W)))
    val ghm_ght_packet_in_SKNode  = BundleBridgeSink[UInt](Some(() => UInt(74.W)))
    var ghm_ghe_packet_out_SRNodes= Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_status_warning_in_SKNodes = Seq[BundleBridgeSink[UInt]]()

    ghm_ght_packet_in_SKNode     := subsystem.tile_ght_packet_out_EPNode
    for (i <- 0 to number_of_ghes-1) {
      val ghe_ghe_packet_out_SRNode = BundleBridgeSource[UInt]()
      ghm_ghe_packet_out_SRNodes = ghm_ghe_packet_out_SRNodes :+ ghe_ghe_packet_out_SRNode
      subsystem.tile_ghe_packet_in_EPNodes(i) := ghm_ghe_packet_out_SRNodes(i)

      val ghm_ghe_status_warning_in_SkNode = BundleBridgeSink[UInt]()
      ghm_ghe_status_warning_in_SKNodes = ghm_ghe_status_warning_in_SKNodes :+ ghm_ghe_status_warning_in_SkNode
      ghm_ghe_status_warning_in_SKNodes(i) := subsystem.tile_ghe_status_warning_out_EPNodes(i)
    }
    subsystem.tile_bigcore_hang_EPNode := bigcore_hang_SRNode

    InModuleBody {
      val ghm = Module (new GHM (GHMParams (params.number_of_little_cores, params.width_GH_packet, params.xlen)))
      ghm.io.ghm_packet_in      := ghm_ght_packet_in_SKNode.bundle

      for (i <- 0 to number_of_ghes-1) {
        if (i == 0) { // The big core
          ghm_ghe_packet_out_SRNodes(i).bundle := 0.U // ghe is not connected to the big core
        } else {
          ghm_ghe_packet_out_SRNodes(i).bundle := ghm.io.ghm_packet_outs(i-1) // -1 big core
          ghm.io.ghe_status_warning_in(i-1)  := ghm_ghe_status_warning_in_SKNodes(i).bundle
        }
      }
      bigcore_hang_SRNode.bundle := ghm.io.bigcore_hang
    }
  }
}