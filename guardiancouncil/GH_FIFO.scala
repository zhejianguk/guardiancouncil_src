package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

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
  val status_warning = Output(UInt(1.W))
  val status_nearfull = Output(UInt(1.W))
  val status_leftthree = Output(UInt(1.W))
}

trait HasFIFOIO extends BaseModule {
  val params: FIFOParams
  val io = IO(new FIFOIO(params))
}

class GH_FIFO(val params: FIFOParams) extends Module with HasFIFOIO {

  def counter(depth: Int, incr: Bool): (UInt, UInt) = {
    val cntReg                  = RegInit(0.U(log2Ceil(depth).W))
    val nextVal                 = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
    when (incr) {
      cntReg                   := nextVal
    }
    (cntReg, nextVal)
  }

  // the register based memory
  val memReg                    = RegInit(VecInit(Seq.fill(params.depth)(0.U(params.width.W))))
  val incrRead                  = WireInit(false.B)
  val incrWrite                 = WireInit(false.B)

  val (readPtr, nextRead)       = counter(params.depth, incrRead)
  val (writePtr, nextWrite)     = counter(params.depth, incrWrite)

  val emptyReg                  = RegInit(true.B)
  val fullReg                   = RegInit(false.B)

  val num_content               = Mux((writePtr >= readPtr) && !fullReg,
                                      writePtr - readPtr, 
                                      writePtr + params.depth.U - readPtr)

  when ((io.enq_valid && !fullReg) && (io.deq_ready && !emptyReg)) {
    memReg(writePtr)           := io.enq_bits
    emptyReg                   := false.B
    fullReg                    := false.B
    incrWrite                  := true.B
    incrRead                   := true.B
  }

  when ((io.enq_valid && !fullReg) && !(io.deq_ready && !emptyReg)){
    memReg(writePtr)           := io.enq_bits
    emptyReg                   := false.B
    fullReg                    := nextWrite === readPtr
    incrWrite                  := true.B
  }
    
  when (!(io.enq_valid && !fullReg) && (io.deq_ready && !emptyReg)) {
    emptyReg                   := nextRead === writePtr
    fullReg                    := false.B
    incrRead                   := true.B
  }
  
  // If the 
  io.status_warning            := Mux(num_content > ((params.depth).U - 4.U), // Communition latency is 4 cycles, avoding to miss in-flight packet
                                      1.U, 
                                      0.U)

  io.status_nearfull           := Mux(num_content > ((params.depth).U - 4.U - 1.U), // Avoding hang-up the big_core
                                      1.U, 
                                      0.U)

  io.status_leftthree          := Mux(num_content > ((params.depth).U - 3.U),
                                    1.U, 
                                    0.U)

  io.deq_bits                  := memReg(readPtr)
  io.full                      := fullReg
  io.empty                     := emptyReg
}

