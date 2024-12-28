// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to extend the 5-stage multi-cycle 32-bit RISC-V core from the previous task to a pipelined processor. 
All steps and stages have the same functionality as in the multi-cycle version from task 03, but are supposed to handle different instructions in each stage simultaneously.
This design implements a pipelined RISC-V 32-bit core with five stages: IF (Fetch), ID (Decode), EX (Execute), MEM (Memory), and WB (Writeback).

    Data Types:
        The uopc enumeration data type (enum) defines micro-operation codes representing ALU operations according to the RV32I subset used in the previous tasks.

    Register File (regFile):
        The regFile module represents the register file, which has read and write ports.
        It consists of a 32-entry register file (x0 is hard-wired to zero).
        Reading from and writing to the register file is controlled by the read request (regFileReadReq), read response (regFileReadResp), and write request (regFileWriteReq) interfaces.

    Fetch Stage (IF Module):
        The IF module represents the instruction fetch stage.
        It includes an instruction memory (IMem) of size 4096 words (32-bit each).
        Instructions are loaded from a binary file (provided to the testbench as a parameter) during initialization.
        The program counter (PC) is used as an address to access the instruction memory, and one instruction is fetched in each cycle.

    Decode Stage (ID Module):
        The ID module performs instruction decoding and generates control signals.
        It extracts opcode, operands, and immediate values from the instruction.
        It uses the uopc (micro-operation code) Enum to determine the micro-operation (uop) and sets control signals accordingly.
        The register file requests are generated based on the operands in the instruction.

    Execute Stage (EX Module):
        The EX module performs the arithmetic or logic operation based on the micro-operation code.
        It takes two operands and produces the result (aluResult).

    Memory Stage (MEM Module):
        The MEM module does not perform any memory operations in this basic CPU design.

    Writeback Stage (WB Module):
        The WB module writes the result back to the register file.

    IF, ID, EX, MEM, WB Barriers:
        IFBarrier, IDBarrier, EXBarrier, MEMBarrier, and WBBarrier modules serve as pipeline registers to separate the pipeline stages.
        They hold the intermediate results of each stage until the next clock cycle.

    PipelinedRV32Icore (PipelinedRV32Icore Module):
        The top-level module that connects all the pipeline stages, barriers and the register file.
        It interfaces with the external world through check_res, which is the result produced by the core.

Overall Execution Flow:

    1) Instructions are fetched from the instruction memory in the IF stage.
    2) The fetched instruction is decoded in the ID stage, and the corresponding micro-operation code is determined.
    3) The EX stage executes the operation using the operands.
    4) The MEM stage does not perform any memory operations in this design.
    5) The result is written back to the register file in the WB stage.

Note that this design only represents a simplified RISC-V pipeline. The structure could be equipped with further instructions and extension to support a real RISC-V ISA.
*/

package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile


// -----------------------------------------
// Global Definitions and Data Types
// -----------------------------------------

object uopc extends ChiselEnum {

  val isADD   = Value(0x01.U)
  val isSUB   = Value(0x02.U)
  val isXOR   = Value(0x03.U)
  val isOR    = Value(0x04.U)
  val isAND   = Value(0x05.U)
  val isSLL   = Value(0x06.U)
  val isSRL   = Value(0x07.U)
  val isSRA   = Value(0x08.U)
  val isSLT   = Value(0x09.U)
  val isSLTU  = Value(0x0A.U)

  val isADDI  = Value(0x10.U)

  val invalid = Value(0xFF.U)
}

import uopc._


// -----------------------------------------
// Register File
// -----------------------------------------

class regFileReadReq extends Bundle {
    // what signals does a read request need?
    // Answer - Register index 
    val reg =  UInt(5.W)
}

class regFileReadResp extends Bundle {
    // what signals does a read response need?
    // Answer - The 32 bit output data
    val data =  SInt(32.W)
}

class regFileWriteReq extends Bundle {
    // what signals does a write request need?
    // Answer - Register index,data to be written and write Enable Signal
    val reg =  UInt(5.W)
    val data =  UInt(32.W)
    val RegWrite = Bool()  

}

class regFile extends Module {
  val io = IO(new Bundle {
    val req       = Input(new regFileReadReq)
    val resp      = Output(new regFileReadResp)
    // how many read and write ports do you need to handle all requests
    // from the pipeline to the register file simultaneously?
    // Answer - 2 read (request and response) and 1 write

    val req2      = Input(new regFileReadReq)
    val resp2     = Output(new regFileReadResp)

    val writeReq  = Input(new regFileWriteReq)
})
  
  /* 
    TODO: Initialize the register file as described in the task 
          and handle the read and write requests
   */

  // Create a Vec for the register file
  val regFileMem = Mem(32, UInt(32.W))

  // Hardwire the first register to 0
  regFileMem.write(0.U,0.U)

  when (io.req.reg === 0.U) {
    io.resp.data := 0.S
  } .otherwise {
    io.resp.data := regFileMem.read(io.req.reg).asSInt()
  }

  when (io.req2.reg === 0.U) {
    io.resp2.data := 0.S
  } .otherwise {
    io.resp2.data := regFileMem.read(io.req2.reg).asSInt()
  }  

  when (io.writeReq.RegWrite && (io.writeReq.reg =/= 0.U)){
    regFileMem.write(io.writeReq.reg, io.writeReq.data)
  }
  
}


// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IFBarrierInput extends Bundle {
    // what signals does a read request need?
    // Answer - Register index 
    val instr = UInt(32.W)
}

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    // Answer - Outputs to the IF Barrier
    val out = Output(new IFBarrierInput)
  })

  /* 
    TODO: Initialize the IMEM as described in the task 
          and handle the instruction fetch.

    TODO: Update the program counter (no jumps or branches, 
          next PC always reads next address from IMEM)
   */

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  val PC = RegInit(0.U(32.W))

  io.out.instr := IMem(PC >> 2) // Divide by 4 to align to word boundaries

  PC := PC + 4.U // Increment the PC (a simple increment by 4)
  
}


// -----------------------------------------
// Decode Stage
// -----------------------------------------

class IFBarrierOutput extends Bundle {
    // what signals does a read request need?
    // Answer - Register index 
    val rs1        = UInt(5.W)
    val rs2        = UInt(5.W)
    val rd         = UInt(5.W)
    val imm        = SInt(32.W)
    val opcode     = UInt(7.W)
    val funct3     = UInt(3.W)
    val funct7     = UInt(7.W)
}

class IDBarrierIO extends Bundle {
    // what signals does a read request need?
    // Answer - Register index 
    val rd        = UInt(5.W)
    val upo       = uopc.Type()
    val operandA  = SInt(32.W)
    val operandB  = SInt(32.W)
}

class ID extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    // Answer - Inputs from IF barrier, outputs to ID barrier

    val in   = Input(new IFBarrierOutput)
    val out  = Output(new IDBarrierIO)

    // Also read ports to the Reg File
    val req  = Output(new regFileReadReq)
    val resp = Input(new regFileReadResp)
    
    val req2  = Output(new regFileReadReq)
    val resp2 = Input(new regFileReadResp)

    val InwriteReq = Input(new regFileWriteReq)
    val OutwriteReq = Output(new regFileWriteReq)
  })

  /* 
   * TODO: Any internal signals needed?
   */


  /* 
    Determine the uop based on the disassembled instruction

    when( condition ){
      when( next condition ){
        io.upo := isXYZ
      }.otherwise{
        maybe declare a case to catch invalid instructions
      } 
    }.elsewhen( different condition ){
      when( next condition ){
        io.upo := isXYZ
      }.otherwise{
        maybe declare a case to catch invalid instructions
      } 
    }.otherwise{
      maybe declare a case to catch invalid instructions
    }
  */

  

    // Decode operation
    // isADD
    when      (io.in.opcode === "b0110011".U && io.in.funct3 === "b000".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isADD
    }
    // isSUB
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b000".U && io.in.funct7 === "b0100000".U){
      io.out.upo := uopc.isSUB
    }
    // isXOR  
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b100".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isXOR
    }
    // isOR    
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b110".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isOR
    }
    // isAND  
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b111".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isAND
    }
    // isSLL   
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b001".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isSLL
    }
    // isSRL   
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b101".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isSRL
    }
    //  isSRA  
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b101".U && io.in.funct7 === "b0100000".U){
      io.out.upo := uopc.isSRA
    }
    // isSLT  
    .elsewhen (io.in.opcode === "b0110011".U && io.in.funct3 === "b010".U && io.in.funct7 === "b0000000".U){
      io.out.upo := uopc.isSLT
    }
    // isADDI 
    .elsewhen (io.in.opcode === "b0010011".U && io.in.funct3 === "b000".U){
      io.out.upo := uopc.isADDI
    }
    // invalid
    .otherwise {
      io.out.upo := uopc.invalid
    }

  /* 
   * TODO: Read the operands from the register file
   */

   // Get operands
    io.req.reg := io.in.rs1
    io.out.operandA := io.resp.data

    when (io.in.opcode === "b0110011".U ){
      // R-Type Instruction
      io.req2.reg := io.in.rs2
      io.out.operandB := io.resp2.data
    }
    .elsewhen (io.in.opcode === "b0010011".U ){
      // I-Type Instruction
      io.out.operandB := io.in.imm  // Manually sign-extend to 32 bits 
      io.req2.reg := 0.U 
    }
    .otherwise {
      io.req2.reg := 0.U
      io.out.operandB := 0.S
    }

    io.out.rd := io.in.rd

    io.OutwriteReq.RegWrite := io.InwriteReq.RegWrite
    io.OutwriteReq.reg      := io.InwriteReq.reg
    io.OutwriteReq.data     := io.InwriteReq.data
  
}

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class ExBarrierIO extends Bundle {    
    val rd        = UInt(5.W)
    val aluResult = UInt(32.W)
}

class EX extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val in       = Input(new IDBarrierIO) 
    val out      = Output(new ExBarrierIO)
    
  })

  /* 
    TODO: Perform the ALU operation based on the uopc

    when( uopc === isXYZ ){
      result := operandA + operandB
    }.elsewhen( uopc === isABC ){
      result := operandA - operandB
    }.otherwise{
      maybe also declare a case to catch invalid instructions
    }
  */
  // Decode operation
    // isADD
    when (io.in.upo === uopc.isADD){
      io.out.aluResult := (io.in.operandA.asSInt + io.in.operandB.asSInt).asUInt()
    }
    // isSUB
    .elsewhen (io.in.upo === uopc.isSUB){
      io.out.aluResult := (io.in.operandA.asSInt - io.in.operandB.asSInt).asUInt()
    }
    // isXOR  
    .elsewhen (io.in.upo === uopc.isXOR){
      io.out.aluResult := (io.in.operandA.asUInt ^ io.in.operandB.asUInt)
    }
    // isOR    
    .elsewhen (io.in.upo === uopc.isOR){
      io.out.aluResult := (io.in.operandA.asUInt | io.in.operandB.asUInt)
    }
    // isAND  
    .elsewhen (io.in.upo === uopc.isAND){
      io.out.aluResult := (io.in.operandA.asUInt & io.in.operandB.asUInt)
    }
    // isSLL   
    .elsewhen (io.in.upo === uopc.isSLL){
      io.out.aluResult := (io.in.operandA.asUInt << io.in.operandB(4, 0).asUInt)
    }
    // isSRL   
    .elsewhen (io.in.upo === uopc.isSRL){
      io.out.aluResult := (io.in.operandA.asUInt >> io.in.operandB(4, 0).asUInt)
    }
    //  isSRA  
    .elsewhen (io.in.upo === uopc.isSRA){
      io.out.aluResult := (io.in.operandA.asSInt >> io.in.operandB(4, 0).asUInt).asUInt()
    }
    // isSLT  
    .elsewhen (io.in.upo === uopc.isSLT){
      io.out.aluResult := (io.in.operandA.asSInt < io.in.operandB.asSInt).asUInt()
    }
    // isADDI 
    .elsewhen (io.in.upo === uopc.isADDI){
      io.out.aluResult := (io.in.operandA.asSInt + io.in.operandB.asSInt).asUInt()
    }
    // invalid
    .otherwise {
      io.out.aluResult := 0.U // Default value if no operation
    }

    io.out.rd := io.in.rd
}

// -----------------------------------------
// Memory Stage
// -----------------------------------------

class MEM extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val in       = Input(new ExBarrierIO) 
    val out      = Output(new ExBarrierIO)
  })

  // No memory operations implemented in this basic CPU
  io.out.rd := io.in.rd
  io.out.aluResult := io.in.aluResult
}


// -----------------------------------------
// Writeback Stage
// -----------------------------------------

class WB extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val in    = Input(new ExBarrierIO)
    val out   = Output(new regFileWriteReq)
  })

  /* 
   * TODO: Perform the write back to the register file and set 
   *       the check_res signal for the testbench.
   */

   io.out.RegWrite := true.B
   io.out.reg := io.in.rd
   when (io.in.rd =/= 0.U){
    io.out.data := io.in.aluResult
  } .otherwise {
    io.out.data := 0.U
  }
  
}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------
class IFBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val in  = Input(new IFBarrierInput)

    val out = Output(new IFBarrierOutput)
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
   */

    val rs1Reg        = Reg(UInt(5.W))
    val rs2Reg        = Reg(UInt(5.W))
    val rdReg         = Reg(UInt(5.W))
    val immReg        = Reg(SInt(32.W))
    val opcodeReg     = Reg(UInt(7.W))
    val funct3Reg     = Reg(UInt(3.W))
    val funct7Reg     = Reg(UInt(7.W))

    rs1Reg    := io.in.instr(19,15)
    rs2Reg    := io.in.instr(24,20)
    rdReg     := io.in.instr(11,7)
    immReg    := Cat(Fill(20, io.in.instr(31)), io.in.instr(31,20)).asSInt()
    opcodeReg := io.in.instr(6, 0)
    funct3Reg := io.in.instr(14,12)
    funct7Reg := io.in.instr(31,25)


    io.out.rs1    := rs1Reg
    io.out.rs2    := rs2Reg
    io.out.rd     := rdReg
    io.out.imm    := immReg
    io.out.opcode := opcodeReg
    io.out.funct3 := funct3Reg
    io.out.funct7 := funct7Reg    

    // printf("[IF] instr = %x\n",io.in.instr)

}


// -----------------------------------------
// ID-Barrier
// -----------------------------------------

class IDBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val in   = Input(new IDBarrierIO)
    val out  = Output(new IDBarrierIO)
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
   */

  val rdReg        = Reg(UInt(5.W))
  val upoReg       = Reg(uopc.Type())
  val operandAReg  = Reg(SInt(32.W))
  val operandBReg  = Reg(SInt(32.W))

  rdReg        := io.in.rd
  upoReg       := io.in.upo
  operandAReg  := io.in.operandA
  operandBReg  := io.in.operandB

  io.out.rd        := rdReg        
  io.out.upo       := upoReg 
  io.out.operandA  := operandAReg 
  io.out.operandB  := operandBReg  

  // printf(p"[ID] upo = ${io.in.upo}\n")
  // printf("[ID] operandA = %d\n",io.in.operandA)
  // printf("[ID] operandB = %d\n",io.in.operandB)
}


// -----------------------------------------
// EX-Barrier
// -----------------------------------------

class EXBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val in   = Input(new ExBarrierIO)
    val out  = Output(new ExBarrierIO)
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
  */
  val rdReg        = Reg(UInt(5.W))
  val aluResultReg = Reg(UInt(32.W))

  rdReg         := io.in.rd
  aluResultReg  := io.in.aluResult

  io.out.rd         := rdReg
  io.out.aluResult  := aluResultReg

  // printf("[EX] aluResult = %d\n",io.in.aluResult)
}


// -----------------------------------------
// MEM-Barrier
// -----------------------------------------

class MEMBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val in   = Input(new ExBarrierIO)
    val out  = Output(new ExBarrierIO)
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
  */
  val rdReg        = Reg(UInt(5.W))
  val aluResultReg = Reg(UInt(32.W))

  rdReg         := io.in.rd
  aluResultReg  := io.in.aluResult

  io.out.rd         := rdReg
  io.out.aluResult  := aluResultReg

  // printf("[MEM]\n")
}


// -----------------------------------------
// WB-Barrier
// -----------------------------------------

class WBBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val in   = Input(new regFileWriteReq)
    val out = Output(new regFileWriteReq)
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
  */

  val regReg       = Reg(UInt(5.W))
  val dataReg      = Reg(UInt(32.W))
  val RegWriteReg  = Reg(Bool())

  regReg       := io.in.reg
  dataReg      := io.in.data
  RegWriteReg  := io.in.RegWrite

  io.out.reg      := regReg
  io.out.data     := dataReg
  io.out.RegWrite := RegWriteReg

  // printf("[WB] reg  = %d\n",io.in.reg)
  // printf("[WB] data = %d\n",io.in.data)
}



class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })


  /* 
   * TODO: Instantiate Barriers
   */
  val ifBarrier   = Module(new IFBarrier())
  val idBarrier   = Module(new IDBarrier())
  val exBarrier   = Module(new EXBarrier())
  val memBarrier  = Module(new MEMBarrier())
  val wbBarrier   = Module(new WBBarrier())

  /* 
   * TODO: Instantiate Pipeline Stages
   */

  val ifStage   = Module(new IF(BinaryFile))
  val idStage   = Module(new ID())
  val exStage   = Module(new EX())
  val memStage  = Module(new MEM())
  val wbStage   = Module(new WB())


  /* 
   * TODO: Instantiate Register File
   */
  val registers = Module(new regFile())

  io.check_res := 0.U // necessary to make the empty design buildable TODO: change this

  /* 
   * TODO: Connect all IOs between the stages, barriers and register file.
   * Do not forget the global output of the core module
   */

  ifBarrier.io.in       := ifStage.io.out

  idStage.io.in         := ifBarrier.io.out

  idStage.io.resp       := registers.io.resp
  idStage.io.resp2      := registers.io.resp2
  registers.io.req      := idStage.io.req
  registers.io.req2     := idStage.io.req2

  registers.io.writeReq := idStage.io.OutwriteReq
  idStage.io.InwriteReq := wbBarrier.io.out

  idBarrier.io.in       := idStage.io.out

  exStage.io.in         := idBarrier.io.out

  exBarrier.io.in       := exStage.io.out

  memStage.io.in        := exBarrier.io.out

  memBarrier.io.in      := memStage.io.out

  wbStage.io.in         := memBarrier.io.out

  wbBarrier.io.in       := wbStage.io.out

  io.check_res          := wbBarrier.io.out.data
}

