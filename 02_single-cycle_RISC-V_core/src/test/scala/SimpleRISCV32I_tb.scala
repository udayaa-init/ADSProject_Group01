// ADS I Class Project
// Single-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

package SimpleRV32I_Tester

import chisel3._
import chiseltest._
import SimpleRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class SimpleRISCV32ITest extends AnyFlatSpec with ChiselScalatestTester {

"SimpleRV32I_Tester" should "work" in {
    test(new SimpleRV32I("src/test/programs/BinaryFile")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.clock.step(1)
      dut.io.result.expect(4.U)     // ADDI x1, x0, 4
      dut.clock.step(1)
      dut.io.result.expect(5.U)     // ADDI x2, x0, 5
      dut.clock.step(1)
      dut.io.result.expect(9.U)     // ADD x3, x1, x2
        /* 
         * TODO: Add testcases for all R-type instructions in 'BinaryFile' and check the expected results here
         */

      /* dut.clock.step(1)
      dut.io.result.expect("hFFFFFFFF".U) */

      // SUB x4, x1, x2: x4 = x1 - x2 = 4 - 5 = -1 (two's complement: 0xFFFFFFFF)
      dut.io.result.expect("hFFFFFFFF".U)
      dut.clock.step(1)

      // AND x5, x1, x2: x5 = x1 & x2 = 4 & 5 = 4
      dut.io.result.expect(4.U)
      dut.clock.step(1)

      // OR x6, x1, x2: x6 = x1 | x2 = 4 | 5 = 5
      dut.io.result.expect(5.U)
      dut.clock.step(1)

      // XOR x7, x1, x2: x7 = x1 ^ x2 = 4 ^ 5 = 1
      dut.io.result.expect(1.U)
      dut.clock.step(1)

      // SLL x8, x1, x2: x8 = x1 << x2 = 4 << 5 = 128
      dut.io.result.expect(128.U)
      dut.clock.step(1)

      // SRL x9, x1, x2: x9 = x1 >> x2 = 4 >> 5 = 0
      dut.io.result.expect(0.U)
      dut.clock.step(1)

      // SRA x10, x1, x2: x10 = x1 >>> x2 = 4 >>> 5 = 0
      dut.io.result.expect(0.U)
      dut.clock.step(1)
           
    }
  }
}


