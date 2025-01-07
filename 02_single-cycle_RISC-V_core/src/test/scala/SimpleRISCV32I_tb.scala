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
         * BASIC TESTS
         */

      
      // SUB x4, x1, x2: x4 = x1 - x2 = 4 - 5 = -1 (two's complement: 0xFFFFFFFF)
      dut.clock.step(1)
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

      // SLT x11, x1, x2
      dut.io.result.expect(1.U)
      dut.clock.step(1)

      /* 
         * HARDWIRED ZERO
      */

      // ADD x0, x8, x7: x0 = x8 + x7 = (129 overwritten) 0
      dut.io.result.expect(0.U)
      dut.clock.step(1)

      /* 
         * MAX POSITIVE IMMEDIATE
      */
      // ADDI x1, x0, 1.U
      // ADDI x3, x1, 2047.U
      dut.clock.step(1) // extra clock step to run two instructions
      dut.io.result.expect(2092.U)

      /* 
         * MAX NEGATIVE IMMEDIATE
      */
      // ADDI x3, x1, -1.S
      dut.clock.step(1) // extra clock step to run two instructions
      dut.io.result.expect(44.U)

      /* 
         * OVERFLOW TEST ADD
      */
      // ADDI x1, x0, 1.U
      // ADDI x2, x0, -1.S
      // ADD x3, x1, x2 // x1(1) + x2(-1) = 0
      dut.clock.step(3) // extra clock steps to run three instructions
      dut.io.result.expect(0.U)

      /* 
         * ADDING NEGATIVE NUMBERS
      */
      // ADDI x1, x0, -16 // x1 (-16) + x2 (-1)
      // ADD x3, x1, x2
      dut.clock.step(2)
      dut.io.result.expect(4294967279L.U) //(-17)

      /* 
         * SLT NEGATIVE NUMBERS
      */
      // SLT x3, x1, x2 // x1 (-16) < x2(-1)
      dut.clock.step(1)
      dut.io.result.expect(1.U)

      /* 
         * SLT NEGATIVE and POSITIVE NUMBERS
      */
      // ADDI x1, x0, 1.U // x1 (1) < x2 (-1)
      // SLT x3, x1, x2
      dut.clock.step(2)
      dut.io.result.expect(0.U)

      /* 
         * NEGATIVE SRA
      */
      // SRA x3, x2, x1  // x2 (0xFFFFFFFF) > x1 (1)
      dut.clock.step(1)
      dut.io.result.expect(4294967295L.U) // (0xFFFFFFFF)

       /* 
         * UNDEFINED OPCODE
      */
      // 0x00000000
      dut.clock.step(1)
      dut.io.result.expect(0.U) // 
           
    }
  }
}


