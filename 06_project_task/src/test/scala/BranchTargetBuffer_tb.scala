package BranchTargetBuffer_Tester

import chisel3._
import chiseltest._
import BranchTargetBuffer._
import org.scalatest.flatspec.AnyFlatSpec

class BranchTargetBufferTest extends AnyFlatSpec with ChiselScalatestTester {

"BranchTargetBuffer_Tester" should "work" in {
    test(new BranchTargetBuffer()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // Send a PC for the first time, expect a miss
      dut.io.update.poke(false.B)
      dut.io.PC.poke(0x1000.U)
      dut.clock.step(1)
  
      dut.io.valid.expect(false.B)
      dut.io.target.expect(0x1004.U)
    }
  }
}


