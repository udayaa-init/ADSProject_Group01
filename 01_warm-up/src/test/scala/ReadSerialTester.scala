// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/** 
  *read serial tester
  */
class ReadSerialTester extends AnyFlatSpec with ChiselScalatestTester {

  "ReadSerial" should "work" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        /*dut.io.rxd.poke(...)
         *dut.clock.step(...)
         *dut.io.valid.expect(...)
         *dut.io.data.expect("b11111111".U) 
         *...
         *TODO: Add your testcases here
         */

      // Case 1: Reset the design
      dut.io.reset.poke(1.U) // Activate reset
      dut.clock.step(1) // Step one clock cycle
      dut.io.reset.poke(0.U) // Deactivate reset
      dut.clock.step(1)

      // Case 2: Verification of idle state (rxd = 1)
      dut.io.rxd.poke(1.U)
      dut.clock.step(1)
      dut.io.valid.expect(0.U) // No valid signal yet
      dut.io.dataOut.expect(0.U) // Data output should be zero initially

      // Case 3: Start bit detection (rxd = 0)
      dut.io.rxd.poke(0.U) // Start bit
      dut.clock.step(1) // Step to transition into the start state
      dut.io.valid.expect(0.U) // No valid signal during start bit

      // Case 4: Transmit 8 data bits (MSB first)
      val data = "b10101100".U(8.W) // Example data to send
      val dataBits = data.asBools.reverse // Convert to individual bits (LSB first for serial)

      for (bit <- dataBits) 
      {
        dut.io.rxd.poke(bit) // Send each bit on rxd
        dut.clock.step(1) // Step one clock cycle per bit
      }

      // Case 5: Check valid signal and output after all 8 bits
      dut.io.rxd.poke(1.U) // Back to idle state
      dut.clock.step(1) // Allow valid to go high
      dut.io.valid.expect(1.U) // Valid should now be high
      dut.io.dataOut.expect(data) // Output should match transmitted data
      dut.clock.step(1) // Step one more cycle to clear the valid signal
      dut.io.valid.expect(0.U) // Valid should now be low again

      // Case 6: Test continuous reception
      val data2 = "b11001101".U(8.W) // Another data byte
      val data2Bits = data2.asBools.reverse // Convert to individual bits (LSB first for serial)

      dut.io.rxd.poke(0.U) // Start bit for second byte
      dut.clock.step(1)

      for (bit <- data2Bits) 
      {
        dut.io.rxd.poke(bit) // Send each bit
        dut.clock.step(1)
      }

      dut.io.rxd.poke(1.U) // Back to idle
      dut.clock.step(1)
      dut.io.valid.expect(1.U) // Valid should go high again
      dut.io.dataOut.expect(data2) // Output should match the second data byte
      dut.clock.step(1)
      dut.io.valid.expect(0.U) // Valid should clear
    }
  }
}
      
