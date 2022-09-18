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
    // Widith: xLen*2
    // Depth: 32
    val u_channel               = Module (new GH_FIFO(FIFOParams ((2*xLen), 32))) 


    // Internal signals
    val channel_enq_valid       = WireInit(false.B)
    val channel_enq_data        = WireInit(0.U((2*xLen).W))
    val channel_deq_ready       = WireInit(false.B)
    val channel_deq_data        = WireInit(0.U((2*xLen).W))
    val channel_empty           = WireInit(true.B)
    val channel_full            = WireInit(false.B)
    val channel_nearfull        = WireInit(false.B)
    val channel_warning         = WireInit(0.U(1.W))
    val channel_sch_na          = RegInit(0.U(1.W))

    u_channel.io.enq_valid     := channel_enq_valid
    u_channel.io.enq_bits      := channel_enq_data
    u_channel.io.deq_ready     := channel_deq_ready
    channel_deq_data           := u_channel.io.deq_bits
    channel_empty              := u_channel.io.empty
    channel_full               := u_channel.io.full
    channel_nearfull           := u_channel.io.status_nearfull
    channel_warning            := u_channel.io.status_warning

    // Software Funcs
    val doCheck                 = (cmd.fire && (funct === 0x00.U))
    val doEvent                 = (cmd.fire && (funct === 0x01.U))
    val doCheckBigStatus        = (cmd.fire && (funct === 0x07.U))
    val doTop_FirstHalf         = (cmd.fire && (funct === 0x0A.U) && !channel_empty)
    val doPop_FirstHalf         = (cmd.fire && (funct === 0x0B.U) && !channel_empty)
    val doTop_SecondHalf        = (cmd.fire && (funct === 0x0C.U) && !channel_empty)
    val doPop_SecondHalf        = (cmd.fire && (funct === 0x0D.U) && !channel_empty)
    val doCheckAgg              = (cmd.fire && (funct === 0x10.U))
    val doPushAgg               = (cmd.fire && (funct === 0x11.U) && !io.agg_buffer_full)
    val doCheckSch              = (cmd.fire && (funct === 0x20.U))
    val doRefreshSch            = (cmd.fire && (funct === 0x21.U))

    // For big core
    val doBigCheck              = (cmd.fire && (funct === 0x6.U))
    val doMask                  = (cmd.fire && (funct === 0x6.U) && (rs2_val === 1.U))
    val doGHT_Cfg               = (cmd.fire && (funct === 0x6.U) && ((rs2_val === 2.U) || (rs2_val === 3.U) || (rs2_val === 4.U)))
    val doGHTBufferCheck        = (cmd.fire && (funct === 0x8.U)) 
    val bigComp                 = io.bigcore_comp


    val ghe_packet_in           = io.ghe_packet_in
    val doPush                  = (ghe_packet_in =/= 0.U) && !channel_full
    val doPull                  = doPop_FirstHalf || doPop_SecondHalf
    val ghe_status_in           = io.ghe_status_in
    val ghe_status_reg          = RegInit(0x0.U(32.W))
    val ghe_event_reg           = RegInit(0x0.U(2.W))


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
    val zeros_63bits            = WireInit(0.U(63.W))
    val zeros_62bits            = WireInit(0.U(62.W))
    val zeros_1bit              = WireInit(0.U(1.W))

    rd_val                     := MuxCase(0.U, 
                                    Array(doCheck             -> Cat(zeros_channel_status,    channel_status_wire), 
                                          doTop_FirstHalf     -> channel_deq_data(127,64),
                                          doPop_FirstHalf     -> channel_deq_data(127,64),
                                          doTop_SecondHalf    -> channel_deq_data(63,0),
                                          doPop_SecondHalf    -> channel_deq_data(63,0),
                                          doCheckBigStatus    -> ghe_status_reg,
                                          doCheckAgg          -> Cat(zeros_62bits, io.agg_buffer_full, zeros_1bit),
                                          doCheckSch          -> Cat(zeros_63bits, channel_sch_na),
                                          doBigCheck          -> Cat(bigComp, rs1_val(15, 0)),
                                          doGHTBufferCheck    -> Cat(zeros_62bits, io.ght_buffer_status)
                                          )
                                          )
    when (doEvent) {
      ghe_event_reg            := rs1_val(1,0)
    }

    when (channel_nearfull) {
      channel_sch_na           := 1.U
    } otherwise {
      when (io.ght_sch_refresh === 1.U){
        channel_sch_na         := 0.U
      } otherwise {
        channel_sch_na         := channel_sch_na
      }
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
    io.ght_cfg_out             := Mux(doGHT_Cfg, rs1_val(31,0), 0.U) 
    io.ght_cfg_valid           := Mux(doGHT_Cfg, 1.U, 0.U)
    io.ght_mask_out            := ~(ght_status_reg(0))
    io.ght_status_out          := ght_status_reg

    io.agg_packet_out          := Mux(doPushAgg, Cat(rs1_val, rs2_val), 0.U);
    io.agg_core_full           := channel_full
    io.ght_sch_dorefresh       := Mux(doRefreshSch, rs1_val(31, 0), 0.U)
    io.ght_sch_na              := channel_sch_na
}
