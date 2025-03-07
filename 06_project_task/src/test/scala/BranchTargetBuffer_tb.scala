package BranchTargetBuffer_Tester

import chisel3._
import chiseltest._
import BranchTargetBuffer._
import org.scalatest.flatspec.AnyFlatSpec

class BranchTargetBufferTest extends AnyFlatSpec with ChiselScalatestTester {

"BranchTargetBuffer_Tester" should "work" in {
    test(new BranchTargetBuffer()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        
    }
  }
}


