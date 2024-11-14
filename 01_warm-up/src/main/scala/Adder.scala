// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chisel3.util._


/** 
  * Half Adder Class 
  * 
  * Your task is to implement a basic half adder as presented in the lecture.
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class HalfAdder extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val s = Output(UInt(1.W))
    val cout = Output(UInt(1.W))
    })

  /* 
   * TODO: Describe output behaviour based on the input values
   */
    io.s := io.a ^ io.b
    io.cout := io.a & io.b
}

/** 
  * Full Adder Class 
  * 
  * Your task is to implement a basic full adder. The component's behaviour should 
  * match the characteristics presented in the lecture. In addition, you are only allowed 
  * to use two half adders (use the class that you already implemented) and basic logic 
  * operators (AND, OR, ...).
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FullAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val cin = Input(UInt(1.W))
    val s = Output(UInt(1.W))
    val cout = Output(UInt(1.W))

    })


  /* 
   * TODO: Instanciate the two half adders you want to use based on your HalfAdder class
   */
  val halfAdder1 = Module(new HalfAdder())
  val halfAdder2 = Module(new HalfAdder())

  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */
  halfAdder1.io.a := io.a
  halfAdder1.io.b := io.b
  halfAdder2.io.a := halfAdder1.io.s
  halfAdder2.io.b := io.cin
  io.s := halfAdder2.io.s
  io.cout := halfAdder1.io.cout | halfAdder2.io.cout

}

/** 
  * 4-bit Adder class 
  * 
  * Your task is to implement a 4-bit ripple-carry-adder. The component's behaviour should 
  * match the characteristics presented in the lecture.  Remember: An n-bit adder can be 
  * build using one half adder and n-1 full adders.
  * The inputs and the result should all be 4-bit wide, the carry-out only needs one bit.
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FourBitAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a 4-bit ripple-carry-adder as presented in the lecture
     */
     val a = Input(UInt(4.W))       // 4-bit input A
    val b = Input(UInt(4.W))       // 4-bit input B
    val sum = Output(UInt(4.W))    // 4-bit sum output
    val cout = Output(UInt(1.W))  // Carry-out output
    })

  /* 
   * TODO: Instanciate the full adders and one half adderbased on the previously defined classes
   */
  val halfAdder = Module(new HalfAdder())
  val fullAdder1 = Module(new FullAdder())
  val fullAdder2 = Module(new FullAdder())
  val fullAdder3 = Module(new FullAdder())

  /* 
   * TODO: Describe output behaviour based on the input values and the internal 
   */
  val sumBits = Wire(Vec(4, Bool())) // this is required we can't assign io.sum(0) := halfAdder.io.sum
  //val carryBits = Wire(Vec(4, Bool()))
  // Connect the first half adder for the LSB
  halfAdder.io.a := io.a(0)
  halfAdder.io.b := io.b(0)
  sumBits(0) := halfAdder.io.s

  // Connect the carry-out of the half adder to the first full adder
  fullAdder1.io.a := io.a(1)
  fullAdder1.io.b := io.b(1)
  fullAdder1.io.cin := halfAdder.io.cout
  sumBits(1) := fullAdder1.io.s

  // Connect the remaining full adders in series for each subsequent bit
  fullAdder2.io.a := io.a(2)
  fullAdder2.io.b := io.b(2)
  fullAdder2.io.cin := fullAdder1.io.cout
  sumBits(2) := fullAdder2.io.s

  fullAdder3.io.a := io.a(3)
  fullAdder3.io.b := io.b(3)
  fullAdder3.io.cin := fullAdder2.io.cout
  sumBits(3) := fullAdder3.io.s

  // The final carry-out is the carry from the last full adder
  io.cout := fullAdder3.io.cout
  io.sum := sumBits.asUInt
}
