package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles}
import freechips.rocketchip.diplomacy._



case class GHMParams(
  number_of_rockets: Int,
  width_GH_packet: Int, // Default: 202: 3: func; 7: opcode; 64 D, 64 S, 64 S.
  xlen: Int
)


class GHMIO(params: GHMParams) extends Bundle {
  val ghm_packet_in   = Input(UInt(params.width_GH_packet.W))
  val ghm_packet_outs = Output(Vec(params.number_of_rockets, UInt(params.width_GH_packet.W)))
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
  val packet_out_wires          = WireInit (VecInit(Seq.fill(params.number_of_rockets)(0.U(params.width_GH_packet.W))))

  val func                      = packet_in_reg(3*params.xlen+9, 3*params.xlen+7)
  val opcode                    = packet_in_reg(3*params.xlen+6, 3*params.xlen)
  val big_core_rd               = packet_in_reg(3*params.xlen-1, 2*params.xlen)
  val big_core_rs1              = packet_in_reg(2*params.xlen-1, params.xlen)
  val big_core_rs2              = packet_in_reg(params.xlen-1,   0)
  val little_core_dest          = WireInit(0.U(log2Up(params.number_of_rockets).W))
  val new_packet                = WireInit(false.B)


  val u_ghm_sch                 = Module (new GHM_SCH(GHM_SCH_Params (params.number_of_rockets)))
  u_ghm_sch.io.big_core_func   := func
  u_ghm_sch.io.big_core_opcode := opcode
  little_core_dest             := u_ghm_sch.io.dest_little_core_id
  new_packet                   := Mux(Cat(func, opcode) =/= 0.U, true.B, false.B)

  when (new_packet === true.B) {
    packet_out_wires(little_core_dest) := packet_in_reg
  }

  packet_in_reg                := io.ghm_packet_in

  for(i <- 0 to params.number_of_rockets - 1) {
    io.ghm_packet_outs(i)      := packet_out_wires(i)
  }
}

case class GHMCoreLocated(loc: HierarchicalLocation) extends Field[Option[GHMParams]](None)

object GHMCore {

  def attach(params: GHMParams, subsystem: BaseSubsystem with HasTiles)
            (implicit p: Parameters) {


    InModuleBody {
      val ghm = Module (new GHM (GHMParams (params.number_of_rockets, params.width_GH_packet, params.xlen)))
      ghm.io.ghm_packet_in     := 0.U
    }
  }
}