package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.experimental.{IntParam, BaseModule}

case class FIFOParams(
  width: Int,
  depth: Int
)


class FIFOIO(params: FIFOParams) extends Bundle {
  val enq_valid = Input(Bool())
  val full = Output(Bool())
  val enq_bits = Input(UInt(params.width.W))
  val deq_ready= Input(Bool())
  val empty = Output(Bool())
  val deq_bits = Output(UInt(params.width.W))
}

trait HasFIFOIO extends BaseModule {
  val params: FIFOParams
  val io = IO(new FIFOIO(params))
}

object Reg_Fifo extends App {
  val p = new FIFOParams (64, 8)
  (new ChiselStage).emitVerilog(new GH_FIFO(p), args)
}

class GH_FIFO(val params: FIFOParams) extends Module with HasFIFOIO {

  def counter(depth: Int, incr: Bool): (UInt, UInt) = {
    val cntReg = RegInit(0.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
    when (incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  // the register based memory
  val memReg = RegInit(VecInit(Seq.fill(params.depth)(0.U(params.width.W))))

  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)

  val (readPtr, nextRead) = counter(params.depth, incrRead)
  val (writePtr, nextWrite) = counter(params.depth, incrWrite)

  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)

  when (io.enq_valid && !fullReg) {
    memReg(writePtr) := io.enq_bits
    emptyReg := false.B
    fullReg := nextWrite === readPtr
    incrWrite := true.B
  }

  when (io.deq_ready && !emptyReg) {
    fullReg := false.B
    emptyReg := nextRead === writePtr
    incrRead := true.B
  }

  io.deq_bits := memReg(readPtr)
  io.full     := fullReg
  io.empty    := emptyReg
}
