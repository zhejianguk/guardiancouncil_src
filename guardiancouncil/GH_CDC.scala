package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GH_CDCH2L_Params(
  clkdiv_ratio: Int,
  data_width: Int,
  fifo_depth: Int
)

//==========================================================
// I/Os
//==========================================================
class GH_CDCH2L_IO (params: GH_CDCH2L_Params) extends Bundle {
  val cdc_data_in                                = Input(UInt(params.data_width.W))
  val cdc_push                                   = Input(UInt(1.W))

  val cdc_data_out                               = Output(UInt(params.data_width.W))
  val cdc_pull                                   = Input(UInt(1.W))
  val cdc_slave_busy                             = Input(UInt(1.W))

  val cdc_busy                                   = Output(UInt(1.W))
  val cdc_empty                                  = Output(UInt(1.W))
}



trait HasGH_CDCH2L_IO extends BaseModule {
  val params: GH_CDCH2L_Params
  val io = IO(new GH_CDCH2L_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GH_CDCHS (val params: GH_CDCH2L_Params) extends Module with HasGH_CDCH2L_IO
{
  if (params.clkdiv_ratio == 1){
    io.cdc_data_out                             := io.cdc_data_in
    io.cdc_busy                                 := 0.U
  } else {
    val cdc_buffer                               = RegInit(0.U(params.data_width.W))
    val cdc_data                                 = WireInit(0.U(params.data_width.W))
    val cdc_busy                                 = WireInit(0.U(1.W))

    val fsm_reset :: fsm_sending :: fsm_waiting :: Nil = Enum(3)
    val fsm_state                                = RegInit(fsm_reset)
    switch (fsm_state) {
      is (fsm_reset){
        cdc_buffer                              := Mux((io.cdc_push === 1.U), io.cdc_data_in, 0.U)
        cdc_data                                := Mux((io.cdc_push === 1.U), io.cdc_data_in, 0.U)
        fsm_state                               := Mux((io.cdc_push === 1.U), fsm_sending, fsm_reset)
        cdc_busy                                := 0.U
      }

      is (fsm_sending){
        cdc_buffer                              := Mux((io.cdc_pull === 1.U), 0.U, cdc_buffer)
        cdc_data                                := Mux((io.cdc_pull === 1.U), 0.U, cdc_buffer)
        fsm_state                               := Mux((io.cdc_pull === 1.U), fsm_waiting, fsm_sending)
        cdc_busy                                := 1.U
      }

      is (fsm_waiting){
        cdc_buffer                              := 0.U
        cdc_data                                := 0.U
        fsm_state                               := Mux((io.cdc_pull === 1.U), fsm_waiting, fsm_reset)
        cdc_busy                                := Mux((io.cdc_pull === 1.U), 1.U, 0.U)
      }
    }


    io.cdc_data_out                             := cdc_data
    io.cdc_busy                                 := cdc_busy
    io.cdc_empty                                := 0.U
  }
}


class GH_CDCH2LFIFO_HandShake (val params: GH_CDCH2L_Params) extends Module with HasGH_CDCH2L_IO
{
  if (params.clkdiv_ratio == 1){
    io.cdc_data_out                             := Mux((io.cdc_push === 1.U), io.cdc_data_in, 0.U)
    io.cdc_busy                                 := io.cdc_slave_busy
    io.cdc_empty                                := (io.cdc_data_in === 0.U)
  } else {
    val cdc_channel                              = Module(new GH_FIFO(FIFOParams (params.data_width, params.fifo_depth)))
    val cdc_channel_enq_valid                    = WireInit(false.B)
    val cdc_channel_enq_data                     = WireInit(0.U(params.data_width.W))
    val cdc_channel_deq_ready                    = WireInit(false.B)
    val cdc_channel_deq_data                     = WireInit(0.U(params.data_width.W))
    val cdc_channel_empty                        = WireInit(true.B)
    val cdc_channel_full                         = WireInit(true.B)

    cdc_channel.io.enq_valid                    := cdc_channel_enq_valid
    cdc_channel.io.enq_bits                     := cdc_channel_enq_data
    cdc_channel.io.deq_ready                    := cdc_channel_deq_ready
    cdc_channel_deq_data                        := cdc_channel.io.deq_bits
    cdc_channel_empty                           := cdc_channel.io.empty
    cdc_channel_full                            := cdc_channel.io.full


    // From High_Freq:
    cdc_channel_enq_valid                       := Mux(((io.cdc_push === 1.U) && (!cdc_channel_full)), 1.U, 0.U)
    cdc_channel_enq_data                        := Mux(((io.cdc_push === 1.U) && (!cdc_channel_full)), io.cdc_data_in, 0.U)
    
    // To Low_Freq:
    val cdc_data                                 = WireInit(0.U(params.data_width.W))

    val fsm_reset :: fsm_sending :: fsm_waiting :: Nil = Enum(3)
    val fsm_state                                = RegInit(fsm_reset)
    switch (fsm_state) {
      is (fsm_reset){
        cdc_data                                := Mux(((io.cdc_slave_busy === 0.U) && (!cdc_channel_empty)), cdc_channel_deq_data, 0.U)
        fsm_state                               := Mux(((io.cdc_slave_busy === 0.U) && (!cdc_channel_empty)), fsm_sending, fsm_reset)
        cdc_channel_deq_ready                   := 0.U
      }

      is (fsm_sending){
        cdc_data                                := Mux((io.cdc_pull === 1.U), 0.U, cdc_channel_deq_data)
        fsm_state                               := Mux((io.cdc_pull === 1.U), fsm_waiting, fsm_sending)
        cdc_channel_deq_ready                   := Mux((io.cdc_pull === 1.U), 1.U, 0.U)
      }

      is (fsm_waiting){
        cdc_data                                := 0.U
        fsm_state                               := Mux((io.cdc_pull === 1.U), fsm_waiting, fsm_reset)
        cdc_channel_deq_ready                   := 0.U
      }
    }

    io.cdc_data_out                               := cdc_data
    io.cdc_busy                                   := cdc_channel.io.status_twoslots
    io.cdc_empty                                  := cdc_channel_empty & (cdc_data === 0.U)
  }
}