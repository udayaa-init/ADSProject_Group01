// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chisel3.util._


/** controller class */
class Controller extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
     val rxd = Input(UInt(1.W))
    val reset = Input(UInt(1.W))
    val cnt_s = Input(UInt(1.W))
    val cnt_en = Output(UInt(1.W))
    val shift_en = Output(UInt(1.W))
    val valid = Output(UInt(1.W))
    })

  // internal variables
  val sIdle :: sReceive :: Nil = Enum(2)
  val state = RegInit(sIdle)
  
  io.cnt_en := 0.U
  io.shift_en := 0.U
  io.valid := 0.U
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */

  // state machine
  /* 
   * TODO: Describe functionality if the controller as a state machine
   */
   switch(state) {
    is(sIdle) {
       when(io.reset === 1.U) {
        state := sIdle
        io.valid := 0.U
      }.elsewhen((io.reset === 0.U) && (io.rxd =/= 1.U)) { // Start bit detected
        state := sReceive
        io.valid := 0.U
      }.otherwise{
        io.cnt_en := 0.U
        io.valid := 0.U
        io.shift_en := 0.U
        state := sIdle
      }
    }
    is(sReceive) {
      when(io.reset === 1.U) {
        state := sIdle
      }.elsewhen(io.cnt_s === 1.U) { // When 8 bits received
        io.cnt_en := 0.U
        io.valid := 1.U
        io.shift_en := 0.U
        state := sIdle
      }.otherwise{
      io.cnt_en := 1.U
      io.shift_en := 1.U
      io.valid := 0.U
      state := sReceive
      }
    }
  }

}


/** counter class */
class Counter extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val cnt_en = Input(UInt(1.W))
    val cnt_s = Output(UInt(1.W))
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
  val counter = RegInit(0.U(8.W))
  io.cnt_s := (counter === (8).U)

  // state machine
  /* 
   * TODO: Describe functionality if the counter as a state machine
   */
  
  when(io.cnt_en === 1.U) {
    counter := counter + 1.U
  }.otherwise {
    counter := 0.U
  }


}

/** shift register class */
class ShiftRegister extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val in = Input(UInt(1.W))
    val load = Input(UInt(1.W))
    val out = Output(UInt(8.W))
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
  val reg = RegInit(0.U(8.W))

  // functionality
  /* 
   * TODO: Describe functionality if the shift register
   */
  when(io.load === 1.U) {
    reg := Cat(reg(8-2, 0), io.in)
  }

  io.out := reg
}

/** 
  * The last warm-up task deals with a more complex component. Your goal is to design a serial receiver.
  * It scans an input line (“serial bus”) named rxd for serial transmissions of data bytes. A transmission 
  * begins with a start bit ‘0’ followed by 8 data bits. The most significant bit (MSB) is transmitted first. 
  * There is no parity bit and no stop bit. After the last data bit has been transferred a new transmission 
  * (beginning with a start bit, ‘0’) may immediately follow. If there is no new transmission the bus line 
  * goes high (‘1’, this is considered the “idle” bus signal). In this case the receiver waits until the next 
  * transmission begins. The outputs of the design are an 8-bit parallel data signal and a valid signal. 
  * The valid signal goes high (‘1’) for one clock cycle after the last serial bit has been transmitted, 
  * indicating that a new data byte is ready.
  */
class ReadSerial extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val rxd = Input(UInt(1.W))          // Serial input line
    val reset = Input(UInt(1.W))        // Reset signal
    val dataOut = Output(UInt(8.W))     // 8-bit data output
    val valid = Output(UInt(1.W))       // Valid output signal
    })


  // instanciation of modules
  /* 
   * TODO: Instanciate the modules that you need
   */
  val controller = Module(new Controller())
  val counter = Module(new Counter())
  val shiftReg = Module(new ShiftRegister())
  // connections between modules
  /* 
   * TODO: connect the signals between the modules
   */
  controller.io.rxd := io.rxd
  controller.io.reset := io.reset
  counter.io.cnt_en := controller.io.cnt_en
  controller.io.cnt_s :=counter.io.cnt_s
  shiftReg.io.load := controller.io.shift_en
  shiftReg.io.in := io.rxd
  io.dataOut := shiftReg.io.out
  // global I/O 
  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */
  io.valid := controller.io.valid
}

