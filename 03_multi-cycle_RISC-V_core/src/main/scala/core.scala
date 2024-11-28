// ADS I Class Project
// Multi-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage multi-cycle 32-bit RISC-V processor (without pipelining) supporting parts of the RV32I instruction set architecture. The RV32I core is relatively basic 
and does not include features like memory operations, exception handling, or branch instructions. It is designed for a simplified subset of the RISC-V ISA. It mainly 
focuses on ALU operations and basic instruction execution. 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        The current stage is stored in a register named stage.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.
            The program counter (PC) is updated for the next instruction.

        Other:
            If the processor state is not in any of the defined stages, an assertion is triggered to indicate an error.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class MultiCycleRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })

  val fetch :: decode :: execute :: memory :: writeback :: Nil = Enum(5) // Enum datatype to define the stages of the processor FSM
  val stage = RegInit(fetch) 

  // -----------------------------------------
  // Instruction Memory
  // -----------------------------------------

  /*
   * DONE: Implement the memory as described above
   */

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  // -----------------------------------------
  // CPU Registers
  // -----------------------------------------

  /*
   * DONE: Implement the program counter as a register, initialize with zero
   */
  val PC = RegInit(0.U(32.W))

  /*
   * DONE: Implement the Register File as described above
   */

  // Create a Vec for the register file
  val regFile = Mem(32, UInt(32.W))

  // Hardwire the first register to 0
  when(true.B) {
    regFile.write(0.U,0.U)
  }
  // -----------------------------------------
  // Microarchitectural Registers / Wires
  // -----------------------------------------

  // if signal is processed in the same cycle --> wire
  // is signal is used in a later cycle       --> register

  /*
   * DONE: Implement the registers and wires you need in the individual stages of the processor 
   */
  
  // Written in the Fetch stage, used in decode stage
  val instr = RegInit(0.U(32.W))

  // Read in the Decode Stage
  val opcode = Wire(UInt(7.W))
  opcode := 0.U //Wires must me initialized
  val funct3 = Wire(UInt(3.W))
  funct3 := 0.U //Wires must me initialized
  val funct7 = Wire(UInt(7.W))
  funct7 := 0.U //Wires must me initialized

  val imm    = Wire(SInt(12.W))
  imm := 0.S //Wires must me initialized


  // Written in the decode stage, used in execute and writeback stage
  val rs1       = Reg(UInt(5.W))
  val rd        = Reg(UInt(5.W))
  val rs2       = Reg(UInt(5.W))

  val operandA  = Reg(SInt(32.W))
  val operandB  = Reg(SInt(32.W))

  val isADD  = Reg(Bool())
  val isSUB  = Reg(Bool())
  val isXOR  = Reg(Bool())
  val isOR   = Reg(Bool())
  val isAND  = Reg(Bool())
  val isSLL  = Reg(Bool())
  val isSRL  = Reg(Bool())
  val isSRA  = Reg(Bool())
  val isSLT  = Reg(Bool())
  val isADDI = Reg(Bool())

  // Written in execute, used in writeback stage
  val aluResult = Reg(UInt(32.W)) 

  val writeBackData = Wire(UInt(32.W))
  writeBackData :=  0.U //Wires must me initialized

  // IOs need default case
  io.check_res := "h_0000_0000".U


  // -----------------------------------------
  // Processor Stages : FETCH
  // -----------------------------------------

  when (stage === fetch)
  {

  /*
   * DONE: Implement fetch stage
   */

   // Fetch the instruction
    instr := IMem(PC >> 2) // Divide by 4 to align to word boundaries
    io.check_res := "h_0000_0000".U

    // printf("[FETCH] PC = %d\n",PC>>2)

    stage := decode

  } 
  // -----------------------------------------
  // Processor Stages : DECODE
  // -----------------------------------------

    .elsewhen (stage === decode)
  {

  /*
   * DONE: Implement decode stage
   */

    // Instrction format
    opcode := instr(6, 0)
    funct3 := instr(14,12)
    funct7 := instr(31,25)

    // Get Registers
    rd  := instr(11,7)
    rs1 := instr(19,15)
    rs2 := instr(24,20)
    imm := instr(31,20).asSInt()

    // printf("[DECODE] imm = %d\n",imm)

    // Get operands
    operandA := (regFile.read(instr(19,15))).asSInt()

    when (opcode === "b0110011".U ){
      // R-Type Instruction
      operandB := (regFile.read(instr(24,20))).asSInt()
    }
    .elsewhen (opcode === "b0010011".U ){
      // I-Type Instruction
      operandB := Cat(Fill(20, imm(11)), imm(11,0)).asSInt()  // Manually sign-extend to 32 bits  
    }

    // Decode operation
    isADD  := (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0000000".U)
    isSUB  := (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0100000".U)
    isXOR  := (opcode === "b0110011".U && funct3 === "b100".U && funct7 === "b0000000".U)
    isOR   := (opcode === "b0110011".U && funct3 === "b110".U && funct7 === "b0000000".U)
    isAND  := (opcode === "b0110011".U && funct3 === "b111".U && funct7 === "b0000000".U)
    isSLL  := (opcode === "b0110011".U && funct3 === "b001".U && funct7 === "b0000000".U)
    isSRL  := (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0000000".U)
    isSRA  := (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0100000".U)
    isSLT  := (opcode === "b0110011".U && funct3 === "b010".U && funct7 === "b0000000".U)
    isADDI := (opcode === "b0010011".U && funct3 === "b000".U)


    stage := execute
  } 
  // -----------------------------------------
  // Processor Stages : EXECUTE
  // -----------------------------------------

    .elsewhen (stage === execute)
  {

  /*
   * DONE: Implement execute stage
   */
    // printf("[DECODE] rd = %d, rs1 = %d,rs2 = %d\n",rd,rs1,rs2)
    // printf("[DECODE] operandA = %d, operandB = %d\n",operandA,operandB)

    when(isADD) { 
      // printf("[EXECUTE] ADD \n")
      aluResult := (operandA.asSInt + operandB.asSInt).asUInt()
    }.elsewhen(isADDI) { 
      // printf("[EXECUTE] ADDI \n")
      aluResult := (operandA.asSInt + operandB.asSInt).asUInt()
    }.elsewhen(isSUB) {
      // printf("[EXECUTE] SUB \n")
      aluResult := (operandA.asSInt - operandB.asSInt).asUInt()
    }.elsewhen(isXOR) {
      // printf("[EXECUTE] XOR \n")
      aluResult := (operandA.asUInt ^ operandB.asUInt)
    }.elsewhen(isOR) {
      // printf("[EXECUTE] OR \n")
      aluResult := (operandA.asUInt | operandB.asUInt)
    }.elsewhen(isAND) {
      // printf("[EXECUTE] AND \n")
      aluResult := (operandA.asUInt & operandB.asUInt)
    }.elsewhen(isSLL) {
      // printf("[EXECUTE] SLL \n")
      aluResult := (operandA.asUInt << operandB(4, 0).asUInt)
    }.elsewhen(isSRL) {
      // printf("[EXECUTE] SRL \n")
      aluResult := (operandA.asUInt >> operandB(4, 0).asUInt)
    }.elsewhen(isSRA) {
      // printf("[EXECUTE] SRA \n")
      aluResult := (operandA.asSInt >> operandB(4, 0).asUInt).asUInt()
    }.elsewhen(isSLT) {
      // printf("[EXECUTE] SLT \n")
      aluResult := (operandA.asSInt < operandB.asSInt).asUInt()
    }.otherwise {
      aluResult := 0.U // Default value if no operation 
    }

    stage := memory
  }
  // -----------------------------------------
  // Processor Stages : MEMORY
  // -----------------------------------------
    .elsewhen (stage === memory)
  {

    // No memory operations implemented in this basic CPU

    // DONE: There might still something be missing here
    // printf("[MEMORY] \n")

    stage := writeback

  } 
  // -----------------------------------------
  // Processor Stages : WRITEBACK
  // -----------------------------------------
    .elsewhen (stage === writeback)
  {

  /*
   * DONE: Implement Writeback stage
   */

    writeBackData := aluResult 
    regFile.write(instr(11,7),writeBackData)
    // Increment the PC (a simple increment by 4)
    PC := PC + 4.U
  

  /*
   * DONE: Write result to output
   */
    io.check_res := writeBackData

    // printf("[WRITEBACK] writeBackData = %d\n", writeBackData)

    stage := fetch

  }
    .otherwise 
  {

     // default case (needed for RTL-generation but should never be reached   

     assert(true.B, "Pipeline FSM must never be left")

  }

}

