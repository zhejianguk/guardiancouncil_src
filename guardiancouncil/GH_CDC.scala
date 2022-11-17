package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GH_CDCH2L_Params(
  clkdiv_ratio: Int,
  data_width: Int
)

//==========================================================
// I/Os
//==========================================================
class GH_CDCH2L_IO (params: GH_CDCH2L_Params) extends Bundle {
  val cdc_data_in                                = Input(UInt(params.data_width.W))
  val cdc_push                                   = Input(UInt(1.W))

  val cdc_data_out                               = Output(UInt(params.data_width.W))
  val cdc_pull                                   = Input(UInt(1.W))

  val cdc_busy                                   = Output(UInt(1.W))
}



trait HasGH_CDCH2L_IO extends BaseModule {
  val params: GH_CDCH2L_Params
  val io = IO(new GH_CDCH2L_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GH_CDCH2LFIFO (val params: GH_CDCH2L_Params) extends Module with HasGH_CDCH2L_IO
{
  val cdc_buffer                                 = Module(new GH_FIFO(FIFOParams(params.data_width, 17)))
  val div_counter                                = RegInit((params.clkdiv_ratio).U((log2Ceil(params.clkdiv_ratio)+1).W))

  val cdc_buffer_empty                           = WireInit(false.B)
  val cdc_buffer_full                            = WireInit(false.B)
  val cdc_deq_ready                              = WireInit(false.B)

  cdc_buffer_empty                              := cdc_buffer.io.empty
  cdc_buffer_full                               := cdc_buffer.io.full
  cdc_buffer.io.enq_bits                        := Mux(cdc_buffer_full, 0.U, io.cdc_data_in)
  cdc_buffer.io.enq_valid                       := Mux(cdc_buffer_full, 0.U, io.cdc_push)
  cdc_buffer.io.deq_ready                       := Mux(cdc_buffer_empty, 0.U, cdc_deq_ready)

  val doSend                                     = WireInit(0.U(1.W))
  doSend                                        := Mux(((!cdc_buffer_empty) && (io.cdc_pull === 1.U)), 1.U, 0.U)


  val fsm_reset :: fsm_sending :: Nil = Enum(2)
  val fsm_state                                 = RegInit(fsm_reset)
  val cdc_deq_data                              = WireInit(0.U(params.data_width.W))
  val send_complete                             = WireInit(false.B)
  send_complete                                := Mux((div_counter < (params.clkdiv_ratio - 1).U), false.B, true.B)

  switch (fsm_state) {
    is (fsm_reset){
      cdc_deq_data                             := 0.U
      cdc_deq_ready                            := false.B
      div_counter                              := 0.U
      fsm_state                                := Mux((doSend === 1.U), fsm_sending, fsm_reset)
    }

    is (fsm_sending){
      cdc_deq_data                             := cdc_buffer.io.deq_bits
      cdc_deq_ready                            := Mux(send_complete, true.B, false.B)
      div_counter                              := Mux(send_complete, 0.U, div_counter+1.U)      
      fsm_state                                := Mux(send_complete, fsm_reset, fsm_sending)
    }
  }

  io.cdc_data_out                               := cdc_deq_data
  io.cdc_busy                                   := cdc_buffer.io.status_warning
}


class GH_CDCH2L (val params: GH_CDCH2L_Params) extends Module with HasGH_CDCH2L_IO
{
  if (params.clkdiv_ratio == 1){
    io.cdc_data_out                             := io.cdc_data_in
    io.cdc_busy                                 := 0.U
  } else {
    val cdc_buffer                               = RegInit(0.U(params.data_width.W))
    val div_counter                              = RegInit(0.U((log2Ceil(params.clkdiv_ratio)+1).W))
    val cdc_data                                 = WireInit(0.U(params.data_width.W))
    val cdc_busy                                 = WireInit(0.U(1.W))

    val fsm_reset :: fsm_sending :: Nil = Enum(2)
    val fsm_state                                = RegInit(fsm_reset)
    switch (fsm_state) {
      is (fsm_reset){
        cdc_data                                  := Mux((io.cdc_push === 1.U), io.cdc_data_in, 0.U)
        cdc_busy                                  := Mux((io.cdc_push === 1.U), 1.U, 0.U)
        cdc_buffer                                := Mux((io.cdc_push === 1.U), io.cdc_data_in, 0.U)
        div_counter                               := Mux((io.cdc_push === 1.U), 1.U, 0.U)
        fsm_state                                 := Mux((io.cdc_push === 1.U), fsm_sending, fsm_reset)
      }

      is (fsm_sending){
        cdc_data                                  := cdc_buffer
        cdc_busy                                  := 1.U
        cdc_buffer                                := Mux((div_counter < (params.clkdiv_ratio-1).U), cdc_buffer, 0.U)
        div_counter                               := Mux((div_counter < (params.clkdiv_ratio-1).U), div_counter+1.U, 0.U)
        fsm_state                                 := Mux((div_counter < (params.clkdiv_ratio-1).U), fsm_sending, fsm_reset)
      }
    }
    io.cdc_data_out                               := cdc_data
    io.cdc_busy                                   := cdc_busy

  }
}
