package BranchTargetBuffer_Tester

import chisel3._
import chiseltest._
import BranchTargetBuffer._
import org.scalatest.flatspec.AnyFlatSpec

class BranchTargetBufferTest extends AnyFlatSpec with ChiselScalatestTester {

"BranchTargetBuffer_Tester" should "work" in {
    test(new BranchTargetBuffer()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val PC1 = 0x1000.U(32.W)
      val PC1_Default_Target = 0x1004.U(32.W)
      val PC1_Update_Target = 0x1F80.U(32.W)

      val PC2 = 0x2000.U(32.W)
      val PC2_Default_Target = 0x2004.U(32.W)
      val PC2_Update_Target = 0x1A80.U(32.W)

      val PC3 = 0x3000.U(32.W)
      val PC3_Default_Target = 0x3004.U(32.W)
      val PC3_Update_Target = 0x1180.U(32.W)
      //////////// TEST Default Case

      // Send a new PC for the first time, expect default behavior
      dut.io.PC.poke(PC1)

      dut.io.update.poke(false.B)
      
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Default_Target) // Default PC + 4 
      dut.io.predictedTaken.expect(false.B)

      
      
      //////////// TEST the 2-bit Prediction FSM

      // Update
      dut.io.PC.poke(PC1)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC1)
      dut.io.updateTarget.poke(PC1_Update_Target)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 11
      
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Update_Target)
      dut.io.predictedTaken.expect(true.B)

      // Check Updated
      dut.io.PC.poke(PC1)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Update_Target)
      dut.io.predictedTaken.expect(true.B)

      // Update
      dut.io.PC.poke(PC2)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC1)
      dut.io.updateTarget.poke(PC1_Update_Target)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 00
      
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC2_Default_Target)
      dut.io.predictedTaken.expect(false.B)

      // Check Updated
      dut.io.PC.poke(PC1)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Update_Target)
      dut.io.predictedTaken.expect(true.B)

      // Update
      dut.io.PC.poke(PC2)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC1)
      dut.io.updateTarget.poke(PC1_Update_Target)
      dut.io.mispredicted.poke(true.B)
      // This would make the next state 01
      
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC2_Default_Target)
      dut.io.predictedTaken.expect(false.B)

      // Check Updated
      dut.io.PC.poke(PC1)

      dut.io.update.poke(false.B)
            
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Update_Target)
      dut.io.predictedTaken.expect(false.B)

      //////////// Cache Replacement

      // Add a new Entry to the same set
      dut.io.PC.poke(PC1)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC2)
      dut.io.updateTarget.poke(PC2_Update_Target)
      dut.io.mispredicted.poke(true.B)
      
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(PC1_Update_Target)
      dut.io.predictedTaken.expect(false.B)

      // Add another new Entry to the same set
      dut.io.PC.poke(PC1) // latest used entry is 2000 for writing. So this 1000 should be evicted when updating for 3000

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC3)
      dut.io.updateTarget.poke(PC3_Update_Target)
      dut.io.mispredicted.poke(true.B)
      
      // Expect a miss
      dut.clock.step(1)  
      dut.io.valid.expect(false.B)
      dut.io.target.expect(PC1_Default_Target) // Default PC + 4 
      dut.io.predictedTaken.expect(false.B)

//1000 , 3000-<
    //////////// Cache Replacement 22

      // Add a new Entry to the same set
      dut.io.PC.poke(PC2)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC1)
      dut.io.updateTarget.poke(PC1_Update_Target)
      dut.io.mispredicted.poke(true.B)
      
      dut.clock.step(1)
      dut.io.valid.expect(false.B)  // 2000 is not htere in cache so false
      //dut.io.target.expect(0x1A80.U)
      //dut.io.predictedTaken.expect(true.B)

      // Add another new Entry to the same set
      dut.io.PC.poke(PC3) 

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(PC2)
      dut.io.updateTarget.poke(PC3_Update_Target) // updating the target for the 2000 from EX stage just for testing.
      dut.io.mispredicted.poke(true.B)
      
      dut.clock.step(1)  
      dut.io.valid.expect(false.B)

      dut.io.PC.poke(PC2) 

      dut.io.update.poke(false.B)
      dut.io.updatePC.poke(PC1)
      dut.io.updateTarget.poke(PC3_Update_Target)
      dut.io.mispredicted.poke(true.B)
      
      // Expect a miss
      dut.clock.step(1)  
      dut.io.valid.expect(true.B)

    }
  }
}


