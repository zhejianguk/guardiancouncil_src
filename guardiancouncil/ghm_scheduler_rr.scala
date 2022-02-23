package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHM_SCH_Params (
  number_of_little_cores: Int,
)

// *Driver is used for verilog generation
object GHT_SCH_Driver extends App {
  val p = new GHM_SCH_Params (3)
  (new ChiselStage).emitVerilog(new GHM_SCH(p), args)
}

//==========================================================
// I/Os
//==========================================================
class GHM_SCH_IO (params: GHM_SCH_Params) extends Bundle {
  val big_core_func = Input(UInt(3.W))
  val big_core_opcode = Input(UInt(7.W))
  val dest_little_core_id = Output(UInt(log2Up(params.number_of_little_cores).W))
}

trait HasGHM_SCH_IO extends BaseModule {
  val params: GHM_SCH_Params
  val io = IO(new GHM_SCH_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHM_SCH (val params: GHM_SCH_Params) extends Module with HasGHM_SCH_IO
{
    val func                      = io.big_core_func
    val opcode                    = io.big_core_opcode
    val new_packet                = WireInit(false.B)
    
    val dest                      = RegInit ((params.number_of_little_cores-1).U
                                             (log2Up(params.number_of_little_cores).W))
    val dest_next                 = WireInit(0.U(log2Up(params.number_of_little_cores).W))                                        
    dest_next                    := Mux(dest === (params.number_of_little_cores - 1).U, 0.U, dest + 1.U)
    new_packet                   := Mux(Cat(func, opcode) =/= 0.U, true.B, false.B)
    
    when (new_packet === true.B) {
        dest := dest_next
    } .otherwise {
        dest := dest
    }

    io.dest_little_core_id       := dest_next
}