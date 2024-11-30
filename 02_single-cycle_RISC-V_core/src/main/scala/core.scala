// ADS I Class Project
// Single-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a simple 32-bit RISC-V processor supporting parts of the RV32I instruction set architecture. This RV32I core is relatively basic 
and does not include features like memory operations, exception handling, or branch instructions. It is designed for a simplified subset of the RISC-V ISA. It mainly 
focuses on ALU operations and basic instruction execution. 

    Instruction Memory:
        The code initializes an instruction memory "mem" with a capacity of 4096 32-bit words.
        It loads the content of this memory from a binary file specified by the BinaryFile parameter.

    CPU Registers:
        The code initializes a program counter (PC) register "PC" with an initial value of 0.
        The core contains a register file "regFile" with 32 entries. Register x0 is hard-wired to always contain the value 0.

    Fetch Stage:
        The Fetch Stage reads the current instruction from "mem" based on the value of the program counter PC.

    Decode Stage:
        This stage extracts various fields (opcode, rd, funct3, rs1, funct7, and rs2) from the fetched instruction, according to the RISC-V ISA specification. 
        The core determines the operands for ALU operations based on the instruction type and stores them into two variables "operandA" and "operandB". 
        For R-Type instructions, both operands are read from regFile. For I-Type instructions, operandA is read from regFile, while operandB is an 
        immediate value, encoded in the instruction.

        The code defines a set of boolean signals (isADD, isADDI, etc.) to identify the type of instruction based on the opcode and function fields. These 
        signals are then used to determine the operation to be performed in the Execute Stage.

    Execute Stage:
        In the Execute Stage, ALU operations are performed based on the instruction type and the operands. The result is stored in "aluResult".

    Memory Stage:
        This core does not include memory operations, leave this section empty.

    Write Back Stage:
        The result of the ALU operation ("aluResult") is propagated to "writeBackData".
        The code writes the value contained ba "writeBackData" to the destination register in the regFile specified by "rd".

    Check Result Signal:
        The result of the ALU operation is also provided as output to "io.check_res". This signal will be used by our test cases to validate the actions performed by our core.

    Update PC:
        The program counter PC is incremented by 4 in every cycle, as this core does not include jumps or branches. It simply fetches instructions sequentially.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class RV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })


  // -----------------------------------------
  // Instruction Memory
  // -----------------------------------------

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  // -----------------------------------------
  // CPU Registers
  // -----------------------------------------

  /*
   * TODO: Implement the program counter as a register, initialize with zero
   */
  val PC = RegInit(0.U(32.W))
  val regFile = Mem(32, UInt(32.W))
  /*
   * TODO: hard-wire register x0 to zero
   */
  regFile.write(0.U,0.U)

  // val x0 = Wire(UInt(32.W))
  // x0 := regFile.read(0.U) // regFile(0.U)
  // -----------------------------------------
  // Fetch
  // -----------------------------------------

  val instr  = Wire(UInt(32.W)) 
  instr := IMem(PC>>2.U)                     

  // -----------------------------------------
  // Decode
  // -----------------------------------------

  val opcode = instr(6, 0)

  // debug purpose printf("The opcode is %b", opcode) // sbt test
  /*
   * TODO: Add missing fields from fetched instructions for decoding
   */
  val funct3 = instr(14,12)  // from pdf pg no. 16 instr(High, low)
  val funct7 = instr(31,25)
  //printf("the value Ais %b",operandA )
  //printf("the value B is %d",operandB )

  val isADD  = (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0000000".U)
  /*
   * TODO: Add missing R-Type instructions here
   */
  val isSUB  = (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0100000".U)
  val isXOR  = (opcode === "b0110011".U && funct3 === "b100".U && funct7 === "b0000000".U)
  val isOR   = (opcode === "b0110011".U && funct3 === "b110".U && funct7 === "b0000000".U)
  val isAND  = (opcode === "b0110011".U && funct3 === "b111".U && funct7 === "b0000000".U)
  val isSLL  = (opcode === "b0110011".U && funct3 === "b001".U && funct7 === "b0000000".U)
  val isSRL  = (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0000000".U)
  val isSRA  = (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0100000".U)
  val isSLT  = (opcode === "b0110011".U && funct3 === "b010".U && funct7 === "b0000000".U)


  val isADDI = (opcode === "b0010011".U && funct3 === "b000".U)


  // Operands

   /*
   * TODO: Add operand signals accoring to specification
   */
  val operandA = Wire(UInt(32.W)) 
  operandA := regFile.read(instr(19,15))
  val operandB = Wire(UInt(32.W)) 
  when(isADDI){
    val extendedValue = instr(31, 20)
    operandB := extendedValue.asSInt.asUInt()
   // printf("THe extendedValue is %d \n",operandB)
  }.otherwise{
    operandB := regFile.read(instr(24,20))
  }
  // -----------------------------------------
  // Execute
  // -----------------------------------------

  val aluResult = Wire(UInt(32.W)) 
  aluResult := 0.U //uncomment this to see printf result
//   when(isADDI) { 
//     aluResult := operandA + operandB 
//     //printf("THe output is %d \n",aluResult)
//   }.elsewhen(isADD) {                           
//     aluResult := operandA + operandB 
//   }
  /*
   * TODO: Add missing R-Type instructions here. Do not forget to implement a suitable default case for
   *       fetched instructions that are neither R-Type nor ADDI. 
   */
  when(isADDI) { 
    aluResult := operandA + operandB 
  }.elsewhen(isADD) {                           
    aluResult := operandA + operandB 
  }.elsewhen(isSUB) {
    aluResult := operandA - operandB
  }.elsewhen(isXOR) {
    aluResult := operandA ^ operandB
  }.elsewhen(isOR) {
    aluResult := operandA | operandB
  }.elsewhen(isAND) {
    aluResult := operandA & operandB
  }.elsewhen(isSLL) {
    aluResult := operandA << operandB(4, 0) 
  }.elsewhen(isSRL) {
    aluResult := operandA >> operandB(4, 0) 
  }.elsewhen(isSRA) {
    aluResult := (operandA.asSInt >> operandB(4, 0)).asUInt 
  }.elsewhen(isSLT) {
    aluResult := (operandA.asSInt < operandB.asSInt).asUInt 
  }.otherwise {
    aluResult := 0.U // Default value if no operation 
  }

  // -----------------------------------------
  // Memory
  // -----------------------------------------

  // No memory operations implemented in this basic CPU


  // -----------------------------------------
  // Write Back 
  // -----------------------------------------

  val writeBackData = Wire(UInt(32.W))  // comment this to see printf result
  writeBackData := aluResult // comment this to see printf result

  /*
   * TODO: Store "writeBackData" in register "rd" in regFile
   */
  // Only write if the destination is not x0
  when(instr(11, 7) =/= 0.U) { // instr(11,7) is the destination register rd
    regFile.write(instr(11, 7), writeBackData)
  }
  // Check Result
  /*
   * TODO: Propagate "writeBackData" to the "check_res" output for testing purposes
   */
  io.check_res := writeBackData
  when(instr(11, 7) === 0.U) { // instr(11,7) is the destination register rd
    io.check_res := 0.U
  }

  // Update PC
  // no jumps or branches, next PC always reads next address from IMEM
  /*
   * TODO: Increment PC
   */
   PC := PC + 4.U
   //printf("The aluResult is %d \n", aluResult )
   //printf("The PC is %d \n", PC )

}

