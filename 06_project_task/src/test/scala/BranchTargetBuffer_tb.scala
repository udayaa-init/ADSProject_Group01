package BranchTargetBuffer_Tester

import chisel3._
import chiseltest._
import BranchTargetBuffer._
import org.scalatest.flatspec.AnyFlatSpec

class BranchTargetBufferTest extends AnyFlatSpec with ChiselScalatestTester {

"BranchTargetBuffer_Tester" should "work" in {
    test(new BranchTargetBuffer()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      //////////// TEST Default Case

      // Send a PC for the first time, expect a miss
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(false.B)
      
      dut.clock.step(1)  
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x1004.U) // Default PC + 4 
      dut.io.predictedTaken.expect(false.B)

      
      
      //////////// TEST the 2-bit Prediction FSM

      // Update
      dut.io.PC.poke(0x100A.U)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(0x1000.U)
      dut.io.updateTarget.poke(0x1F80.U)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 11
      
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x100E.U)
      dut.io.predictedTaken.expect(false.B)

      // Check Updated
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x1F80.U)
      dut.io.predictedTaken.expect(true.B)

      // Update
      dut.io.PC.poke(0x100A.U)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(0x1000.U)
      dut.io.updateTarget.poke(0x1F80.U)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 00
      
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x100E.U)
      dut.io.predictedTaken.expect(false.B)

      // Check Updated
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x1F80.U)
      dut.io.predictedTaken.expect(true.B)

      // Update
      dut.io.PC.poke(0x100A.U)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(0x1000.U)
      dut.io.updateTarget.poke(0x1F80.U)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 01
      
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x100E.U)
      dut.io.predictedTaken.expect(false.B)

      // Check Updated
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x1F80.U)
      dut.io.predictedTaken.expect(false.B)

      //////////// Cache Replacement

      // Add a new Entry to the same set
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(0x2000.U)
      dut.io.updateTarget.poke(0x1A80.U)
      dut.io.mispredicted.poke(true.B)
      
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x1F80.U)
      dut.io.predictedTaken.expect(false.B)

      // Add another new Entry to the same set
      dut.io.PC.poke(0x1000.U)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(0x3000.U)
      dut.io.updateTarget.poke(0x1180.U)
      dut.io.mispredicted.poke(true.B)
      
      // Expect a miss
      dut.clock.step(1)  
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x1004.U) // Default PC + 4 
      dut.io.predictedTaken.expect(false.B)

    }
  }
}


