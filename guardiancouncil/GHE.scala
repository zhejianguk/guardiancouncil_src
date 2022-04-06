package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._



class GHE(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new GHEImp (this)
}

class GHEImp(outer: GHE)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
    val cmd                     = io.cmd
    val funct                   = cmd.bits.inst.funct
    val rs2                     = cmd.bits.inst.rs2
    val rs1                     = cmd.bits.inst.rs1
    val xd                      = cmd.bits.inst.xd
    val xs1                     = cmd.bits.inst.xs1
    val xs2                     = cmd.bits.inst.xs2
    val rd                      = cmd.bits.inst.rd
    val opcode                  = cmd.bits.inst.opcode

    val rs1_val                 = cmd.bits.rs1
    val rs2_val                 = cmd.bits.rs2
    val rd_val                  = WireInit(0.U(xLen.W))

    // Communication channel
    // Widith: xLen
    // Depth: 256
    val u_channel               = Module (new GH_FIFO(FIFOParams ((xLen+10), 256))) 


    // Internal signals
    val channel_enq_valid       = WireInit(false.B)
    val channel_enq_data        = WireInit(0.U((xLen+10).W))
    val channel_deq_ready       = WireInit(false.B)
    val channel_deq_data        = WireInit(0.U((xLen+10).W))
    val channel_empty           = WireInit(true.B)
    val channel_full            = WireInit(false.B)
    val packet_secondhalf_reg   = RegInit(0.U((xLen).W))
    val channel_warning         = WireInit(0.U(1.W))

    u_channel.io.enq_valid     := channel_enq_valid
    u_channel.io.enq_bits      := channel_enq_data
    u_channel.io.deq_ready     := channel_deq_ready
    channel_deq_data           := u_channel.io.deq_bits
    channel_empty              := u_channel.io.empty
    channel_full               := u_channel.io.full
    channel_warning            := u_channel.io.status_warning

    // Software Funcs
    val doCheck                 = (cmd.fire() && (funct === 0.U))
    val doEvent                 = (cmd.fire() && (funct === 1.U))
    val doTop_FirstHalf         = (cmd.fire() && (funct === 2.U) && !channel_empty)
    val doPop_FirstHalf         = (cmd.fire() && (funct === 3.U) && !channel_empty)
    val doTop_SecondHalf        = (cmd.fire() && (funct === 4.U) && !channel_empty)
    val doPop_SecondHalf        = (cmd.fire() && (funct === 5.U) && !channel_empty)
    val doReportEmpty           = (cmd.fire() && ((funct === 2.U) || (funct === 3.U)) && channel_empty)
    val doCheckStatus           = (cmd.fire() && (funct === 7.U))
    // For big core
    val doMask                  = (cmd.fire() && (funct === 6.U))
    val bigComp                 = io.bigcore_comp


    val ghe_packet_in           = io.ghe_packet_in
    val doPush                  = (ghe_packet_in(73,64) =/= 0.U) && !channel_full
    val doPull                  = (doPop_FirstHalf || doPop_SecondHalf)
    val ghe_status_in           = io.ghe_status_in
    val ghe_status_reg          = RegInit(0x0.U(32.W))
    val ghe_event_reg           = RegInit(0x0.U(1.W))


    // Check status
    // 0b01: empty
    // 0b10: full
    // 0b00: Not empty, not full.
    val channel_status_wire     = Cat(channel_full, channel_empty)


    // Channel Push 
    channel_enq_valid          := Mux(doPush, true.B, false.B)
    channel_enq_data           := Mux(doPush, ghe_packet_in, 0.U) 
    channel_deq_ready          := Mux(doPull, true.B, false.B)

    // Response
    val zeros_channel_status    = WireInit(0.U((xLen-2).W))
    val zeros_pacekt_first_half = WireInit(0.U(54.W))

    rd_val                     := MuxCase(0.U, 
                                    Array(doCheck             -> Cat(zeros_channel_status,    channel_status_wire), 
                                          doTop_FirstHalf     -> Cat(zeros_pacekt_first_half, channel_deq_data(73,64)),
                                          doPop_FirstHalf     -> Cat(zeros_pacekt_first_half, channel_deq_data(73,64)),
                                          doTop_SecondHalf    -> channel_deq_data(63,0),
                                          doPop_SecondHalf    -> channel_deq_data(63,0),
                                          doReportEmpty       -> 0x3FF.U,
                                          doCheckStatus       -> ghe_status_reg,
                                          doMask              -> Cat(bigComp, rs1_val(15, 0))))
    when (doEvent) {
      ghe_event_reg            := rs1_val(0)
    }
    
    ghe_status_reg             := ghe_status_in
    cmd.ready                  := true.B // Currently, it is always ready, because it is never block
    
    io.ghe_event_out           := Cat(ghe_event_reg, channel_warning)
    io.resp.valid              := cmd.valid && xd
    io.resp.bits.rd            := cmd.bits.inst.rd
    io.resp.bits.data          := rd_val
    io.busy                    := cmd.valid // Later add more situations
    io.interrupt               := false.B


    // Big core register
    // 0: test is not start; 
    // 1: test is started
    // 2: test is finished 
    // Registers 
    val ght_status_reg          = RegInit(0.U(32.W)) 
    when (doMask) {
      ght_status_reg           := rs1_val
    }
    io.ght_mask_out            := ~(ght_status_reg(0))
    io.ght_status_out          := ght_status_reg
}



class GHEImp_loopback(outer: GHE)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
    val cmd                     = io.cmd
    val funct                   = cmd.bits.inst.funct
    val rs2                     = cmd.bits.inst.rs2
    val rs1                     = cmd.bits.inst.rs1
    val xd                      = cmd.bits.inst.xd
    val xs1                     = cmd.bits.inst.xs1
    val xs2                     = cmd.bits.inst.xs2
    val rd                      = cmd.bits.inst.rd
    val opcode                  = cmd.bits.inst.opcode

    val rs1_val                 = cmd.bits.rs1
    val rs2_val                 = cmd.bits.rs2
    val rd_val                  = WireInit(0.U(xLen.W))

    // Communication channel
    // Widith: xLen
    // Depth: 64
    val u_channel               = Module (new GH_FIFO(FIFOParams (xLen, 64))) 


    // Internal signals
    val channel_enq_valid       = WireInit(false.B)
    val channel_enq_data        = WireInit(0.U(xLen.W))
    val channel_deq_ready       = WireInit(false.B)
    val channel_deq_data        = WireInit(0.U(xLen.W))
    val channel_empty           = WireInit(true.B)
    val channel_full            = WireInit(false.B)

    u_channel.io.enq_valid     := channel_enq_valid
    u_channel.io.enq_bits      := channel_enq_data
    u_channel.io.deq_ready     := channel_deq_ready
    channel_deq_data           := u_channel.io.deq_bits
    channel_empty              := u_channel.io.empty
    channel_full               := u_channel.io.full

    val doCheck                 = (cmd.fire() && (funct === 0.U))
    val doPush                  = (cmd.fire() && (funct === 1.U) && !channel_full)
    val doPull                  = (cmd.fire() && (funct === 2.U) && !channel_empty) 


    // Check status
    // 0b01: empty
    // 0b10: full
    // 0b00: Not empty, not full.
    val channel_status_wire     = Cat(channel_full, channel_empty)


    // Push 
    channel_enq_valid          := Mux(doPush, true.B, false.B)
    channel_enq_data           := Mux(doPush, rs1_val, 0.U) 

    // Pull
    channel_deq_ready          := Mux(doPull, true.B, false.B)

    // Response
    val zeros                   = WireInit(0.U((xLen-2).W))
    rd_val                     := MuxCase(0.U, 
                                    Array(doCheck -> Cat(zeros, channel_status_wire), 
                                          doPull  -> channel_deq_data))
    
    cmd.ready                  := true.B // Currently, it is always ready, because it is never block
    
    io.resp.valid              := cmd.valid && xd
    io.resp.bits.rd            := cmd.bits.inst.rd
    io.resp.bits.data          := rd_val
    io.busy                    := cmd.valid // Later add more situations
    io.interrupt               := false.B
}
