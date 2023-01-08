package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FILTERS_PRFS_Params(
  xlen: Int,
  totaltypes_of_insts: Int,
  packet_size: Int,
  core_width: Int,
  use_prfs: Boolean
)

//==========================================================
// I/Os
//==========================================================
class GHT_FILTERS_PRFS_IO (params: GHT_FILTERS_PRFS_Params) extends Bundle {
  val ght_ft_cfg_in                             = Input(UInt(32.W))
  val ght_ft_cfg_valid                          = Input(UInt(1.W))

  val ght_ft_inst_in                            = Input(Vec(params.core_width, UInt(32.W)))
  val ght_ft_pc_in                              = Input(Vec(params.core_width, UInt(32.W)))
  val ght_ft_newcommit_in                       = Input(Vec(params.core_width, Bool()))
  val ght_ft_alu_in                             = Input(Vec(params.core_width, UInt(params.xlen.W)))
  val ght_ft_is_rvc_in                          = Input(Vec(params.core_width, UInt(1.W)))

  val ght_ft_inst_index                         = Output(UInt(5.W))
  val packet_out                                = Output(UInt((params.packet_size).W))

  val ght_stall                                 = Input(Bool())
  val core_hang_up                              = Output(UInt(1.W))
  val ght_buffer_status                         = Output(UInt(2.W))
  val ght_prfs_rd_ft                            = Input(Vec(params.core_width, UInt(params.xlen.W)))

  val ght_prfs_forward_ldq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_stq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_ftq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_prf                      = Output(Vec(params.core_width, Bool()))

  val ght_filters_empty                         = Output(UInt(1.W))
}



trait HasGHT_FILTERS_PRFS_IO extends BaseModule {
  val params: GHT_FILTERS_PRFS_Params
  val io = IO(new GHT_FILTERS_PRFS_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_FILTERS_PRFS (val params: GHT_FILTERS_PRFS_Params) extends Module with HasGHT_FILTERS_PRFS_IO
{
  val buffer_width                              = 5 + params.packet_size
  val packet                                    = WireInit(0.U(params.packet_size.W))
  val inst_type                                 = WireInit(0.U(5.W))

  val u_ght_filters                             = Seq.fill(params.core_width) {Module(new GHT_FILTER_PRFS(GHT_FILTER_PRFS_Params(params.xlen, params.totaltypes_of_insts, params.packet_size, params.use_prfs)))}
  val u_buffer                                  = Seq.fill(params.core_width) {Module(new GH_FIFO(FIFOParams (buffer_width, 10)))}

  // Connecting filters
  val filter_inst_index                         = WireInit(VecInit(Seq.fill(params.core_width)(0.U(5.W))))
  val filter_packet                             = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.packet_size.W))))
  for (i <- 0 to params.core_width - 1) {
    u_ght_filters(i).io.ght_ft_cfg_in          := this.io.ght_ft_cfg_in
    u_ght_filters(i).io.ght_ft_cfg_valid       := this.io.ght_ft_cfg_valid
    u_ght_filters(i).io.ght_ft_inst_in         := this.io.ght_ft_inst_in(i)
    u_ght_filters(i).io.ght_ft_pc_in           := this.io.ght_ft_pc_in(i)
    u_ght_filters(i).io.ght_ft_newcommit_in    := this.io.ght_ft_newcommit_in(i)
    u_ght_filters(i).io.ght_ft_alu_in          := this.io.ght_ft_alu_in(i)
    u_ght_filters(i).io.ght_ft_is_rvc_in       := this.io.ght_ft_is_rvc_in(i)
    filter_inst_index(i)                       := u_ght_filters(i).io.ght_ft_inst_index
    filter_packet(i)                           := u_ght_filters(i).io.packet_out
    u_ght_filters(i).io.ght_prfs_rd            := this.io.ght_prfs_rd_ft(i)
    if (params.use_prfs) {
      this.io.ght_prfs_forward_ldq(i)          := u_ght_filters(i).io.ght_prfs_forward_ldq
      this.io.ght_prfs_forward_stq(i)          := u_ght_filters(i).io.ght_prfs_forward_stq
      this.io.ght_prfs_forward_ftq(i)          := u_ght_filters(i).io.ght_prfs_forward_ftq
      this.io.ght_prfs_forward_prf(i)          := u_ght_filters(i).io.ght_prfs_forward_prf
    } else {
      this.io.ght_prfs_forward_ldq(i)          := false.B
      this.io.ght_prfs_forward_stq(i)          := false.B
      this.io.ght_prfs_forward_ftq(i)          := false.B
      this.io.ght_prfs_forward_prf(i)          := false.B
    }
  }

  // Connecting buffers: Enqueue Phase
  val buffer_enq_valid                          = WireInit(false.B)
  val buffer_enq_data                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(buffer_width.W))))
  val buffer_empty                              = WireInit(VecInit(Seq.fill(params.core_width)(false.B)))
  val buffer_full                               = WireInit(VecInit(Seq.fill(params.core_width)(false.B)))
  val buffer_deq_data                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(buffer_width.W))))
  val buffer_deq_valid                          = WireInit(false.B)
  val is_valid_packet                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))

  val new_packet                                = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))
  val doPush                                    = WireInit(0.U(1.W))
  val buffer_inst_type                          = WireInit(VecInit(Seq.fill(params.core_width)(0.U(5.W))))
  val buffer_packet                             = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.packet_size.W))))
  val doPull                                    = WireInit(0.U(1.W))
  
  
  for (i <- 0 to params.core_width - 1) {
    new_packet(i)                              := Mux(filter_packet(i) =/= 0.U, 1.U, 0.U)                                             
    buffer_enq_data(i)                         := Mux(((filter_inst_index(i) =/= 0.U) && (filter_packet(i) =/= 0.U)),  
                                                    Cat(filter_inst_index(i), filter_packet(i)),
                                                    0.U)
  }
  doPush                                       := new_packet.reduce(_|_)
  buffer_enq_valid                             := Mux(doPush === 1.U, true.B, false.B)

  
  for (i <- 0 to params.core_width - 1) {
    u_buffer(i).io.enq_valid                   := buffer_enq_valid
    u_buffer(i).io.enq_bits                    := buffer_enq_data(i)
  }

  // Connecting buffers: Dequeue Phase
  /* Buffer Finite State Machine */
  for (i <- 0 to params.core_width - 1) {
    buffer_empty(i)                            := u_buffer(i).io.empty
    buffer_full(i)                             := u_buffer(i).io.full
    buffer_deq_data(i)                         := u_buffer(i).io.deq_bits
    buffer_inst_type(i)                        := u_buffer(i).io.deq_bits(buffer_width - 1, params.packet_size)
    buffer_packet(i)                           := u_buffer(i).io.deq_bits(params.packet_size - 1, 0)
    u_buffer(i).io.deq_ready                   := buffer_deq_valid
    is_valid_packet(i)                         := ((buffer_deq_data(i) =/= 0.U) & (buffer_inst_type(i) =/= 0.U))
  }

  val fsm_reset :: fsm_send_first :: fsm_send_second :: fsm_send_third :: fsm_send_fourth :: Nil = Enum(5)
  val fsm_state                                 = RegInit(fsm_reset)
  val fsm_reset_nxt_state                       = WireInit(fsm_reset)
  val fsm_first_nxt_state                       = WireInit(fsm_reset)
  val fsm_second_nxt_state                      = WireInit(fsm_reset)
  val fsm_third_nxt_state                       = WireInit(fsm_reset)
  val fsm_fourth_nxt_state                      = WireInit(fsm_reset)

  switch (fsm_state) {
    is (fsm_reset){
      buffer_deq_valid                         := false.B
      packet                                   := 0.U
      inst_type                                := 0.U
      fsm_state                                := Mux((!buffer_empty(0)), fsm_reset_nxt_state, fsm_reset)
    }

    is (fsm_send_first){
      when (io.ght_stall) {
        buffer_deq_valid                       := false.B
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_first
      } .otherwise {
        buffer_deq_valid                       := Mux((fsm_first_nxt_state === fsm_reset), true.B, false.B)
        packet                                 := buffer_packet(0)
        inst_type                              := buffer_inst_type(0)
        fsm_state                              := fsm_first_nxt_state
      }
    }

    is (fsm_send_second){
      when (io.ght_stall) {
        buffer_deq_valid                       := false.B
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_second
      } .otherwise {
        buffer_deq_valid                       := Mux((fsm_second_nxt_state === fsm_reset), true.B, false.B)
        packet                                 := buffer_packet(1)
        inst_type                              := buffer_inst_type(1)
        fsm_state                              := fsm_second_nxt_state
      }
    }

    is (fsm_send_third){
      when (io.ght_stall) {
        buffer_deq_valid                       := false.B
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_third
      } .otherwise {
        buffer_deq_valid                       := Mux((fsm_third_nxt_state === fsm_reset), true.B, false.B)
        packet                                 := buffer_packet(2)
        inst_type                              := buffer_inst_type(2)
        fsm_state                              := fsm_third_nxt_state
      }
    }

    is (fsm_send_fourth){
      when (io.ght_stall) {
        buffer_deq_valid                       := false.B
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_fourth
      } .otherwise {
        buffer_deq_valid                       := true.B
        packet                                 := buffer_packet(3)
        inst_type                              := buffer_inst_type(3)
        fsm_state                              := fsm_fourth_nxt_state
      }
    }
  }
    

  // These are used for 4-width Boom
  // There are some work is required to make them generic
  fsm_reset_nxt_state                          := MuxCase(fsm_reset, 
                                                    Array((is_valid_packet(0) =/= 0.U)  -> fsm_send_first,
                                                          ((is_valid_packet(0) === 0.U) && (is_valid_packet(1) =/= 0.U))  -> fsm_send_second,
                                                          ((is_valid_packet(0) === 0.U) && (is_valid_packet(1) === 0.U) && (is_valid_packet(2) =/= 0.U))  -> fsm_send_third,
                                                          ((is_valid_packet(0) === 0.U) && (is_valid_packet(1) === 0.U) && (is_valid_packet(2) === 0.U) && (is_valid_packet(3) =/= 0.U))  -> fsm_send_fourth
                                                          )
                                                          )

  fsm_first_nxt_state                          := MuxCase(fsm_send_first, 
                                                    Array((is_valid_packet(1) =/= 0.U)  -> fsm_send_second,
                                                          ((is_valid_packet(1) === 0.U) && (is_valid_packet(2) =/= 0.U))  -> fsm_send_third,
                                                          ((is_valid_packet(1) === 0.U) && (is_valid_packet(2) === 0.U) && (is_valid_packet(3) =/= 0.U))  -> fsm_send_fourth,
                                                          ((is_valid_packet(1) === 0.U) && (is_valid_packet(2) === 0.U) && (is_valid_packet(3) === 0.U)) -> fsm_reset,
                                                          )
                                                          )

  fsm_second_nxt_state                         := MuxCase(fsm_send_second, 
                                                    Array((is_valid_packet(2) =/= 0.U)  -> fsm_send_third,
                                                          ((is_valid_packet(2) === 0.U) && (is_valid_packet(3) =/= 0.U))  -> fsm_send_fourth,
                                                          ((is_valid_packet(2) === 0.U) && (is_valid_packet(3) === 0.U)) -> fsm_reset,
                                                          )
                                                          )

  fsm_third_nxt_state                         := MuxCase(fsm_send_third, 
                                                    Array((is_valid_packet(3) =/= 0.U)  -> fsm_send_fourth,
                                                          (is_valid_packet(3) === 0.U) -> fsm_reset,
                                                          )
                                                          )

  fsm_fourth_nxt_state                        := fsm_reset

  // Outputs
  io.ght_ft_inst_index                        := inst_type
  io.packet_out                               := packet
  io.core_hang_up                             := u_buffer(params.core_width-1).io.status_threeslots
  io.ght_buffer_status                        := Cat(buffer_full(params.core_width-1), buffer_empty.reduce(_&_))
  io.ght_filters_empty                        := buffer_empty.reduce(_&_)
}
