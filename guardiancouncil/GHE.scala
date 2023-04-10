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
    channel_nearfull           := u_channel.io.status_threeslots
    channel_warning            := u_channel.io.status_twoslots

    // Software Funcs
    val doCheck                 = (cmd.fire && (funct === 0x00.U))
    val doEvent                 = (cmd.fire && ((funct === 0x40.U) || (funct === 0x41.U) || (funct === 0x42.U) || (funct === 0x43.U)))
    val doCheckBigStatus        = (cmd.fire && (funct === 0x07.U))
    val doTop_FirstHalf         = (cmd.fire && (funct === 0x0A.U) && !channel_empty)
    val doPop_FirstHalf         = (cmd.fire && (funct === 0x0B.U) && !channel_empty)
    val doTop_SecondHalf        = (cmd.fire && (funct === 0x0C.U) && !channel_empty)
    val doPop_SecondHalf        = (cmd.fire && (funct === 0x0D.U) && !channel_empty)
    val doCheckAgg              = (cmd.fire && (funct === 0x10.U))
    val doPushAgg               = (cmd.fire && (funct === 0x11.U) && !io.agg_buffer_full)
    val doCheckSch              = (cmd.fire && (funct === 0x20.U))
    val doRefreshSch            = (cmd.fire && (funct === 0x21.U))
    val doDebug_ECounter        = (cmd.fire && (funct === 0x22.U))
    val doDebug_GCounter        = (cmd.fire && (funct === 0x23.U))
    val doCheckFIFOUsage        = (cmd.fire && (funct === 0x25.U))
    val doCheckFIFOCounter      = (cmd.fire && (funct === 0x26.U))
    val doCheckFIFODCounter     = (cmd.fire && (funct === 0x27.U))
    val doFIFOCache0            = (cmd.fire && (funct === 0x28.U))
    val doFIFOCache1            = (cmd.fire && (funct === 0x29.U))
    val doInitialised           = (cmd.fire && ((funct === 0x50.U) || (funct === 0x51.U)))
    val doHA                    = (cmd.fire && (funct === 0x52.U))
    val doCheckHA               = (cmd.fire && (funct === 0x53.U))
    val ha_rslt                 = WireInit(0x0.U((xLen).W))


    // For big core
    val doBigCheckComp          = (cmd.fire && (funct === 0x6.U))
    val doMask                  = (cmd.fire && ((funct === 0x30.U) || (funct === 0x31.U) || (funct === 0x32.U) || (funct === 0x33.U) || (funct === 0x34.U) || (funct === 0x35.U) || (funct === 0x36.U) || (funct === 0x37.U) || (funct === 0x38.U)))
    val doCritical              = (cmd.fire && (funct === 0x39.U))
    val doCheckCritial          = (cmd.fire && (funct === 0x49.U))
    val doPID_Cfg               = (cmd.fire && (funct === 0x16.U))
    val doGHT_Cfg               = (cmd.fire && (funct === 0x6.U) && ((rs2_val === 2.U) || (rs2_val === 3.U) || (rs2_val === 4.U)))
    val doGHTBufferCheck        = (cmd.fire && (funct === 0x8.U))
    val doCheckM_PPN            = (cmd.fire && (funct === 0x17.U))
    val doCheckM_SysMode        = (cmd.fire && (funct === 0x18.U))
    val bigComp                 = io.bigcore_comp (1,0)
    val bigInialised            = io.bigcore_comp (2)
    val doDebug_MCounter        = (cmd.fire && (funct === 0x19.U))
    val doDebug_ICounter        = (cmd.fire && (funct === 0x1a.U))
    val doBigCheckIni           = (cmd.fire && (funct === 0x1b.U))
    val doSetActivatedCheckers  = (cmd.fire && (funct === 0x1c.U))
    val doDebug_bp_checker      = (cmd.fire && (funct === 0x1d.U))
    val doDebug_bp_cdc          = (cmd.fire && (funct === 0x1e.U))
    val doDebug_bp_filter       = (cmd.fire && (funct === 0x1f.U))
    val doDebug_Reset_bp        = (cmd.fire && (funct === 0x2d.U))

    val ghe_packet_in           = RegInit(0x0.U((2*xLen).W))
    ghe_packet_in              := io.ghe_packet_in
    val doPush                  = (ghe_packet_in =/= 0.U) && !channel_full
    val doHAPull                = WireInit(0.U(1.W))  
    val doPull                  = doPop_FirstHalf || doPop_SecondHalf || (doHAPull === 1.U)
    val ghe_status_in           = io.ghe_status_in
    val ghe_status_reg          = RegInit(0x0.U(32.W))
    val ghe_event_reg           = RegInit(0x0.U(2.W))
    val ghe_initialised_reg     = RegInit(0x0.U(1.W))

    val ght_status_reg          = RegInit(0.U(32.W))
    val ght_critial_reg         = RegInit(0.U(2.W))
    val ght_monitor_satp_ppn    = RegInit(0.U(44.W))
    val ght_monitor_sys_mode    = RegInit(0.U(2.W))
    val has_monitor_target      = RegInit(0.U(1.W))
    val num_activated_cores     = RegInit(0.U(8.W))

    // Debug 
    val ECounter                = RegInit(0.U(64.W))
    when (doPush){
      ECounter                 := ECounter + 1.U
    }

    
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
    val zeros_20bits            = WireInit(0.U(20.W))
    val zeros_1bit              = WireInit(0.U(1.W))

    val fifo_cache              = RegInit(VecInit(Seq.fill(2)(0.U(64.W))))

    when (doPull) {
      fifo_cache(0)            := channel_deq_data(127,64)
      fifo_cache(1)            := fifo_cache(0)
    }

    rd_val                     := MuxCase(0.U, 
                                    Array(doCheck             -> Cat(zeros_channel_status, channel_status_wire), 
                                          doTop_FirstHalf     -> channel_deq_data(127,64),
                                          doPop_FirstHalf     -> channel_deq_data(127,64),
                                          doTop_SecondHalf    -> channel_deq_data(63,0),
                                          doPop_SecondHalf    -> channel_deq_data(63,0),
                                          doCheckBigStatus    -> ghe_status_reg,
                                          doCheckAgg          -> Cat(zeros_62bits, io.agg_buffer_full, zeros_1bit),
                                          doCheckSch          -> Cat(zeros_63bits, channel_sch_na),
                                          doBigCheckComp      -> Cat(bigComp, rs1_val(15, 0)),
                                          doBigCheckIni       -> Cat(bigInialised),
                                          doGHTBufferCheck    -> Cat(zeros_62bits, io.ght_buffer_status),
                                          doCheckM_PPN        -> Cat(zeros_20bits, ght_monitor_satp_ppn),
                                          doCheckM_SysMode    -> Cat(zeros_62bits, ght_monitor_sys_mode),
                                          doDebug_MCounter    -> io.debug_mcounter,
                                          doDebug_ICounter    -> io.debug_icounter,
                                          doDebug_ECounter    -> ECounter,
                                          doDebug_GCounter    -> io.debug_gcounter,
                                          doDebug_bp_checker  -> io.debug_bp_checker,
                                          doDebug_bp_cdc      -> io.debug_bp_cdc,
                                          doDebug_bp_filter   -> io.debug_bp_filter,
                                          doCheckFIFOUsage    -> u_channel.io.num_content,
                                          doCheckFIFOCounter  -> u_channel.io.debug_fcounter,
                                          doCheckFIFODCounter -> u_channel.io.debug_fdcounter,
                                          doFIFOCache0        -> fifo_cache(0),
                                          doFIFOCache1        -> fifo_cache(1),
                                          doCheckCritial      -> Cat(zeros_62bits, ght_critial_reg(1,0)),
                                          doCheckHA           -> ha_rslt
                                          )
                                          )
                                          
    when (doEvent) {
      ghe_event_reg            := (funct & 0x0F.U);
    }

    when (doInitialised){
      ghe_initialised_reg      := (funct & 0x0F.U);
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
    
    io.ghe_event_out           := Cat(doPush, ghe_initialised_reg, ghe_event_reg, channel_warning)
    io.resp.valid              := cmd.valid && xd
    io.resp.bits.rd            := cmd.bits.inst.rd
    io.resp.bits.data          := rd_val
    io.busy                    := cmd.valid // Later add more situations
    io.interrupt               := false.B


    // Big core register
    // 0: test is not start; 
    // 1: test is started
    // 2: test is finished 
    // 30 - 23: number of activated cores
    // Registers 
    when (doMask) {
      ght_status_reg           := (funct & 0x0F.U);
    }

    when (doCritical) {
      ght_critial_reg          := rs1_val(1,0)
    }


    when (doSetActivatedCheckers) {
      num_activated_cores      := rs1_val
    }

    val define_monitor_target   = WireInit(0.U(1.W))
    val undefine_monitor_target = WireInit(0.U(1.W))
    define_monitor_target      := Mux((doPID_Cfg && (rs1_val === 1.U)), 1.U, 0.U)
    undefine_monitor_target    := Mux((doPID_Cfg && (rs1_val === 2.U)), 1.U, 0.U)

    when ((define_monitor_target === 1.U) && (undefine_monitor_target === 0.U)) {
      ght_monitor_satp_ppn     := io.ght_satp_ppn
      ght_monitor_sys_mode     := io.ght_sys_mode
      has_monitor_target       := 1.U
    }

    when ((define_monitor_target === 0.U) && (undefine_monitor_target === 1.U)) {
      ght_monitor_satp_ppn     := 0.U
      ght_monitor_sys_mode     := 0.U
      has_monitor_target       := 0.U
    }

    val hit_satp_ppn            = (ght_monitor_satp_ppn === io.ght_satp_ppn)
    val hit_privi               = (ght_monitor_sys_mode === io.ght_sys_mode)
    io.if_correct_process      := Mux((define_monitor_target === 1.U), 1.U, (hit_satp_ppn & hit_privi & has_monitor_target))

    io.ght_cfg_out             := Mux(doGHT_Cfg, rs1_val(31,0), 0.U) 
    io.ght_cfg_valid           := Mux(doGHT_Cfg, 1.U, 0.U)
    io.debug_bp_reset          := Mux(doDebug_Reset_bp, 1.U, 0.U)
    io.ght_mask_out            := ~(ght_status_reg(0))
    io.ght_status_out          := Cat(0.U, num_activated_cores, ght_status_reg(22,0))

    io.agg_packet_out          := Mux(doPushAgg, Cat(rs1_val, rs2_val), 0.U);
    io.agg_core_status         := Cat(u_channel.io.status_threeslots, (channel_empty & (ghe_packet_in === 0.U)))
    io.ght_sch_dorefresh       := Mux(doRefreshSch, rs1_val(31, 0), 0.U)
    io.ght_sch_na              := channel_sch_na

    val u_pmc                  = Module (new GHE_HAPMC(GHE_HAPMC_Params (xLen)))
    val pmc_active_reg         = RegInit(0.U(1.W))
    when (doHA){
      pmc_active_reg          := rs1_val(0)
    }

    u_pmc.io.ghe_hapmc_active := pmc_active_reg
    // Revist: below could be configured by the software
    u_pmc.io.ghe_hapmc_hbound := 0x20000000.U
    u_pmc.io.ghe_hapmc_lbound := 0x10000000.U
    u_pmc.io.msgq_empty       := Mux(channel_empty === true.B, 1.U, 0.U)
    u_pmc.io.msgq_data        := channel_deq_data(63,0)
    doHAPull                  := u_pmc.io.msgq_pop
    ha_rslt                   := u_pmc.io.ghe_hapmc_rslt
}
