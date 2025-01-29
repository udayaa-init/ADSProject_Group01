// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detetcion and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/21/2024 by Andro Mazmishvili (@Andrew8846)

/*
The goal of this task is to equip the pipelined 5-stage 32-bit RISC-V core from the previous task with a forwarding unit that takes care of hazard detetction and hazard resolution.
The functionality is the same as in task 4, but the core should now also be able to also process instructions with operands depending on the outcome of a previous instruction without stalling.

In addition to the pipelined design from task 4, you need to implement the following modules and functionality:

    Hazard Detection and Forwarding:
        Forwarding Unit: Determines if and from where data should be forwarded to resolve hazards. 
                         Resolves data hazards by forwarding the correct values from later pipeline stages to earlier ones.
                         - Inputs: Register identifiers from the ID, EX, MEM, and WB stages.
                         - Outputs: Forwarding select signals (forwardA and forwardB) indicating where to forward the values from.

        The forwarding logic utilizes multiplexers to select the correct operand values based on forwarding decisions.

Make sure that data hazards (dependencies between instructions in the pipeline) are detected and resolved without stalling the pipeline. For additional information, you can revise the ADS I lecture slides (6-25ff).

Note this design only represents a simplified RISC-V pipeline. The structure could be equipped with further instructions and extension to support a real RISC-V ISA.
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
    val addr  = Input(UInt(5.W))
}

class regFileReadResp extends Bundle {
    val data  = Output(UInt(32.W))
}

class regFileWriteReq extends Bundle {
    val addr  = Input(UInt(5.W))
    val data  = Input(UInt(32.W))
    val wr_en = Input(Bool())
}

class regFile extends Module {
  val io = IO(new Bundle {
    val req_1  = new regFileReadReq
    val resp_1 = new regFileReadResp
    val req_2  = new regFileReadReq
    val resp_2 = new regFileReadResp
    val req_3  = new regFileWriteReq
})

  val regFile = Mem(32, UInt(32.W))
  regFile(0) := 0.U                           // hard-wired zero for x0

  when(io.req_3.wr_en){
    when(io.req_3.addr =/= 0.U){
      regFile(io.req_3.addr) := io.req_3.data
    }
  }

  io.resp_1.data := Mux(io.req_1.addr === 0.U, 0.U, regFile(io.req_1.addr))
  io.resp_2.data := Mux(io.req_2.addr === 0.U, 0.U, regFile(io.req_2.addr))

  // Special Register File
  when(io.req_3.wr_en && (io.req_1.addr === io.req_3.addr)){
    io.resp_1.data := Mux(io.req_1.addr === 0.U, 0.U, io.req_3.data) 
  }

  when(io.req_3.wr_en && (io.req_2.addr === io.req_3.addr)){
    io.resp_2.data := Mux(io.req_2.addr === 0.U, 0.U, io.req_3.data)
  }

}

/* TODO:
     Hazard detetction logic:
     Which pipeline stages are affected and how can a potential hazard be detetced there?
     Affected Stages: ID, EX, MEM, and WB.
	   Detection: Compare rs1/rs2 of the current instruction with rd of instructions in EX and MEM
  */

  class HazardDetectionUnit extends Module {
  val io = IO(new Bundle {
    val rs1ID = Input(UInt(5.W))
    val rs2ID = Input(UInt(5.W))
    val rdEX = Input(UInt(5.W))
    val rdMEM = Input(UInt(5.W))
    val rdWB = Input(UInt(5.W))
    val wr_en = Input(Bool())
    val stall = Output(Bool()) // Stall signal for load-use hazard
  })

  // Load-Use Hazard Detection
  val hazardEXRS1 = (io.rs1ID === io.rdEX) && (io.rdEX =/= 0.U)
  val hazardEXRS2 = (io.rs2ID === io.rdEX) && (io.rdEX =/= 0.U)

  val hazardMEMRS1 = (io.rs1ID === io.rdMEM) && (io.rdMEM =/= 0.U)
  val hazardMEMRS2 = (io.rs2ID === io.rdMEM) && (io.rdMEM =/= 0.U)

  io.stall := hazardEXRS1 || hazardEXRS2 || hazardMEMRS1 || hazardMEMRS2
}

  /* TODO:
     Forwarding Selection:
     Select the appropriate value to forward from one stage to another based on the hazard checks.
     Use forwarding signals (forwardA, forwardB) and multiplexers to select data from EX (EXBarrier.io.outAluResult), MEM (MEMBarrier.io.outAluResult), or ID (IDBarrier.io.outOperandA/B) stages.
  */


class ForwardingUnit extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does the forwarding unit need?
    val rs1ID = Input(UInt(5.W))   // Source register 1 identifier from ID stage
    val rs2ID = Input(UInt(5.W))   // Source register 2 identifier from ID stage
    val rdEX = Input(UInt(5.W))    // Destination register identifier from EX stage
    val rdMEM = Input(UInt(5.W))   // Destination register identifier from MEM stage
    val forwardA = Output(UInt(2.W)) // Forwarding select signal for operandA
    val forwardB = Output(UInt(2.W)) // Forwarding select signal for operandB
  })

  // Forwarding Logic for operandA
  // Check if the source register rs1 in the ID stage matches rdEX or rdMEM
  when(io.rs1ID === io.rdEX && io.rdEX =/= 0.U) {
    // Forward from EX stage if the register matches and EX writes back
    io.forwardA := "b10".U
  }.elsewhen(io.rs1ID === io.rdMEM && io.rdMEM =/= 0.U) {
    // Forward from MEM stage if the register matches and MEM writes back
    io.forwardA := "b01".U
  }.otherwise {
    // No forwarding for operandA
    io.forwardA := "b00".U
  }

  // Forwarding Logic for operandB
  // Check if the source register rs2 in the ID stage matches rdEX or rdMEM
  when(io.rs2ID === io.rdEX && io.rdEX =/= 0.U) {
    // Forward from EX stage if the register matches and EX writes back
    io.forwardB := "b10".U
  }.elsewhen(io.rs2ID === io.rdMEM && io.rdMEM =/= 0.U) {
    // Forward from MEM stage if the register matches and MEM writes back
    io.forwardB := "b01".U
  }.otherwise {
    // No forwarding for operandB
    io.forwardB := "b00".U
  }
} 

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val instr = Output(UInt(32.W))
  })

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  val PC = RegInit(0.U(32.W))
  
  io.instr := IMem(PC>>2.U)

  // Update PC
  // no jumps or branches, next PC always reads next address from IMEM
  PC := PC + 4.U
  // printf("[FETCH] PC = %d\n",PC>>2)
  // printf("[FETCH] instr = %x\n",IMem(PC>>2.U))
  
}


// -----------------------------------------
// Decode Stage
// -----------------------------------------

class ID extends Module {
  val io = IO(new Bundle {
    val regFileReq_A  = Flipped(new regFileReadReq) 
    val regFileResp_A = Flipped(new regFileReadResp) 
    val regFileReq_B  = Flipped(new regFileReadReq) 
    val regFileResp_B = Flipped(new regFileReadResp) 
    val instr         = Input(UInt(32.W))
    val uop           = Output(uopc())
    val rd            = Output(UInt(5.W))
    val rs1           = Output(UInt(5.W))
    val rs2           = Output(UInt(5.W))
    val operandA      = Output(UInt(32.W))
    val operandB      = Output(UInt(32.W))
  })

  val opcode  = io.instr(6, 0)
  io.rd      := io.instr(11, 7)
  val funct3  = io.instr(14, 12)
  val rs1     = io.instr(19, 15)

  // R-Type
  val funct7  = io.instr(31, 25)
  val rs2     = io.instr(24, 20)

  // I-Type
  val imm     = io.instr(31, 20) 

  when(opcode === "b0110011".U){
    when(funct3 === "b000".U){
      when(funct7 === "b0000000".U){
        io.uop := isADD
        // printf("[DECODE] uop isADD\n")
      }.elsewhen(funct7 === "b0100000".U){
        io.uop := isSUB
        // printf("[DECODE] uop isSUB\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b100".U){
      when(funct7 === "b0000000".U){
        io.uop := isXOR
        // printf("[DECODE] uop isXOR\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b110".U){
      when(funct7 === "b0000000".U){
        io.uop := isOR
        // printf("[DECODE] uop isOR\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b111".U){
      when(funct7 === "b0000000".U){
        io.uop := isAND
        // printf("[DECODE] uop isAND\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b001".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLL
        // printf("[DECODE] uop isSLL\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b101".U){
      when(funct7 === "b0000000".U){
        io.uop := isSRL
        // printf("[DECODE] uop isSRL\n")
      }.elsewhen(funct7 === "b0100000".U){
        io.uop := isSRA
        // printf("[DECODE] uop isSRA\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b010".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLT
        // printf("[DECODE] uop isSLT\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.elsewhen(funct3 === "b011".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLTU
        // printf("[DECODE] uop isSLTU\n")
      }.otherwise{
        io.uop := invalid
        // printf("[DECODE] uop invalid\n")
      }
    }.otherwise{
      io.uop := invalid
      // printf("[DECODE] uop invalid\n")
    }
  }.elsewhen(opcode === "b0010011".U){
    when(funct3 === "b000".U){
      io.uop := isADDI
      // printf("[DECODE] uop isADDI\n")
    }.otherwise{
      io.uop := invalid
      // printf("[DECODE] uop invalid\n")
    }
  }.otherwise{
    io.uop := invalid
    // printf("[DECODE] uop invalid\n")
  }

  // Operands
  io.regFileReq_A.addr := rs1
  io.regFileReq_B.addr := rs2

  io.operandA := io.regFileResp_A.data
  io.operandB := Mux(opcode === "b0110011".U, io.regFileResp_B.data, Mux(opcode === "b0010011".U, imm, 0.U))

  io.rs1     := rs1
  io.rs2     := rs2
  
}

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EX extends Module {
  val io = IO(new Bundle {
    val uop       = Input(uopc())
    val operandA  = Input(UInt(32.W))
    val operandB  = Input(UInt(32.W))
    val aluResult = Output(UInt(32.W))
  })

  val operandA = io.operandA
  val operandB = io.operandB
  val uop      = io.uop

  // printf("[EXECUTE] operandA = %d, operandB = %d\n",operandA,operandB)

  when(uop === isADDI) { 
      io.aluResult := operandA + operandB 
      // printf("[EXECUTE] uop isADDI\n")
    }.elsewhen(uop === isADD) {                           
      io.aluResult := operandA + operandB 
      // printf("[EXECUTE] uop isADD\n")
    }.elsewhen(uop === isSUB) {  
      io.aluResult := operandA - operandB
      // printf("[EXECUTE] uop isSUB\n") 
    }.elsewhen(uop === isXOR) {  
      io.aluResult := operandA ^ operandB
      // printf("[EXECUTE] uop isXOR\n") 
    }.elsewhen(uop === isOR) {  
      io.aluResult := operandA | operandB
      // printf("[EXECUTE] uop isOR\n") 
    }.elsewhen(uop === isAND) {  
      io.aluResult := operandA & operandB
      // printf("[EXECUTE] uop isAND\n") 
    }.elsewhen(uop === isSLL) {  
      io.aluResult := operandA << operandB(4, 0)
      // printf("[EXECUTE] uop isSLL\n") 
    }.elsewhen(uop === isSRL) {  
      io.aluResult := operandA >> operandB(4, 0)
      // printf("[EXECUTE] uop isSRL\n") 
    }.elsewhen(uop === isSRA) {  
      io.aluResult := (operandA.asSInt >> operandB(4, 0)).asUInt        // automatic sign extension, if SInt datatype is used
      // printf("[EXECUTE] uop isSRA\n")
    }.elsewhen(uop === isSLT) {  
      io.aluResult := (operandA.asSInt < operandB.asSInt).asUInt  // automatic sign extension, if SInt datatype is used
      // printf("[EXECUTE] uop isSLT\n")
    }.elsewhen(uop === isSLTU) {  
      io.aluResult := Mux(operandA < operandB, 1.U, 0.U)
      // printf("[EXECUTE] uop isSLTU\n") 
    }.otherwise{
      io.aluResult := "h_FFFF_FFFF".U // = 2^32 - 1; self-defined encoding for invalid operation, value is unlikely to be reached in a regular arithmetic operation
      // printf("[EXECUTE] uop invalid\n")
    } 

}

// -----------------------------------------
// Memory Stage
// -----------------------------------------

class MEM extends Module {
  val io = IO(new Bundle {

  })

  // No memory operations implemented in this basic CPU

}

// -----------------------------------------
// Writeback Stage
// -----------------------------------------

class WB extends Module {
  val io = IO(new Bundle {
    val regFileReq = Flipped(new regFileWriteReq) 
    val rd         = Input(UInt(5.W))
    val aluResult  = Input(UInt(32.W))
    val check_res  = Output(UInt(32.W))
  })

 io.regFileReq.addr  := io.rd
 io.regFileReq.data  := io.aluResult
 io.regFileReq.wr_en := io.aluResult =/= "h_FFFF_FFFF".U  // could depend on the current uopc, if ISA is extendet beyond R-type and I-type instructions

 io.check_res := io.aluResult
// printf("[WRITEBACK] writeBackData = %d\n", io.aluResult)
}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
  val io = IO(new Bundle {
    val inInstr  = Input(UInt(32.W))
    val outInstr = Output(UInt(32.W))
  })

  val instrReg = RegInit(0.U(32.W))

  io.outInstr := instrReg
  instrReg    := io.inInstr

}


// -----------------------------------------
// ID-Barrier
// -----------------------------------------

class IDBarrier extends Module {
  val io = IO(new Bundle {
    val inUOP       = Input(uopc())
    val inRD        = Input(UInt(5.W))
    val inRS1       = Input(UInt(5.W))
    val inRS2       = Input(UInt(5.W))
    val inOperandA  = Input(UInt(32.W))
    val inOperandB  = Input(UInt(32.W))
    val outUOP      = Output(uopc())
    val outRD       = Output(UInt(5.W))
    val outRS1      = Output(UInt(5.W))
    val outRS2      = Output(UInt(5.W))
    val outOperandA = Output(UInt(32.W))
    val outOperandB = Output(UInt(32.W))
  })

  val uop      = Reg(uopc())
  val rd       = RegInit(0.U(5.W))
  val rs1      = RegInit(0.U(5.W))
  val rs2      = RegInit(0.U(5.W))
  val operandA = RegInit(0.U(32.W))
  val operandB = RegInit(0.U(32.W))

  io.outUOP := uop
  uop := io.inUOP
  io.outRD := rd
  rd := io.inRD
  io.outRS1 := rs1
  rs1 := io.inRS1
  io.outRS2 := rs2
  rs2 := io.inRS2
  io.outOperandA := operandA
  operandA := io.inOperandA
  io.outOperandB := operandB
  operandB := io.inOperandB

}


// -----------------------------------------
// EX-Barrier
// -----------------------------------------

class EXBarrier extends Module {
  val io = IO(new Bundle {
    val inAluResult  = Input(UInt(32.W))
    val outAluResult = Output(UInt(32.W))
    val inRD         = Input(UInt(5.W))
    val outRD        = Output(UInt(5.W))
  })

  val aluResult = RegInit(0.U(32.W))
  val rd       = RegInit(0.U(5.W))

  io.outAluResult := aluResult
  aluResult       := io.inAluResult

  io.outRD := rd
  rd := io.inRD

}


// -----------------------------------------
// MEM-Barrier
// -----------------------------------------

class MEMBarrier extends Module {
  val io = IO(new Bundle {
    val inAluResult  = Input(UInt(32.W))
    val outAluResult = Output(UInt(32.W))
    val inRD         = Input(UInt(5.W))
    val outRD        = Output(UInt(5.W))
  })

  val aluResult = RegInit(0.U(32.W))
  val rd        = RegInit(0.U(5.W))

  io.outAluResult := aluResult
  aluResult       := io.inAluResult

  io.outRD := rd
  rd := io.inRD

}


// -----------------------------------------
// WB-Barrier
// -----------------------------------------

class WBBarrier extends Module {
  val io = IO(new Bundle {
    val inCheckRes   = Input(UInt(32.W))
    val outCheckRes  = Output(UInt(32.W))
  })

  val check_res   = RegInit(0.U(32.W))

  io.outCheckRes := check_res
  check_res      := io.inCheckRes
}


// -----------------------------------------
// Main Class
// -----------------------------------------


class HazardDetectionRV32Icore(BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W)) // Output to check the result from WB stage
  })

 // Pipeline Registers
  val IFBarrier  = Module(new IFBarrier)  // IF/ID Barrier
  val IDBarrier  = Module(new IDBarrier)  // ID/EX Barrier
  val EXBarrier  = Module(new EXBarrier)  // EX/MEM Barrier
  val MEMBarrier = Module(new MEMBarrier) // MEM/WB Barrier
  val WBBarrier  = Module(new WBBarrier)  // WB Barrier

  // Pipeline Stages
  val IF  = Module(new IF(BinaryFile))    // Fetch Stage
  val ID  = Module(new ID)                // Decode Stage
  val EX  = Module(new EX)                // Execute Stage
  val MEM = Module(new MEM)               // Memory Stage
  val WB  = Module(new WB)                // Writeback Stage


  /* 
    TODO: Instantiate the forwarding unit.
  */
  val forwardingUnit = Module(new ForwardingUnit)

  // Register File
  val regFile = Module(new regFile)

  // Connections for IOs
  // IF --> IF Barrier
  IFBarrier.io.inInstr := IF.io.instr
  printf("[IF] instr = %x\n", IF.io.instr)

  // IF Barrier --> ID
  ID.io.instr := IFBarrier.io.outInstr  

  // Reg File --> ID
  ID.io.regFileReq_A <> regFile.io.req_1
  ID.io.regFileReq_B <> regFile.io.req_2
  ID.io.regFileResp_A <> regFile.io.resp_1
  ID.io.regFileResp_B <> regFile.io.resp_2

  // ID --> ID Barrier
  IDBarrier.io.inUOP := ID.io.uop
  IDBarrier.io.inRD := ID.io.rd
  IDBarrier.io.inRS1 := ID.io.rs1
  IDBarrier.io.inRS2 := ID.io.rs2
  IDBarrier.io.inOperandA := ID.io.operandA
  IDBarrier.io.inOperandB := ID.io.operandB
  printf("[ID] operandA = %d\n", ID.io.operandA)
  printf("[ID] OperandB = %d\n", ID.io.operandB)

  /* 
    TODO: Connect the I/Os of the forwarding unit 
  */

  // Forwarding Unit Connections
  forwardingUnit.io.rs1ID := IDBarrier.io.outRS1
  forwardingUnit.io.rs2ID := IDBarrier.io.outRS2
  forwardingUnit.io.rdEX := EXBarrier.io.outRD
  forwardingUnit.io.rdMEM := MEMBarrier.io.outRD

  // Forwarding signals
  val forwardA = forwardingUnit.io.forwardA
  val forwardB = forwardingUnit.io.forwardB
  printf("[FWD] forwardA = %x\n", forwardingUnit.io.forwardA)
  printf("[FWD] forwardB = %x\n", forwardingUnit.io.forwardB)
  
  // // Hazard Detection Unit Connections
  // hazardDetectionUnit.io.rs1ID := IDBarrier.io.outRS1
  // hazardDetectionUnit.io.rs2ID := IDBarrier.io.outRS2
  // hazardDetectionUnit.io.rdEX := EXBarrier.io.outRD
  // hazardDetectionUnit.io.rdMEM := MEMBarrier.io.outRD

  //   // Stall Logic
  //   when(hazardDetectionUnit.io.stall) {
  //   IFBarrier.io.inInstr := 0.U // NOP
  //   IDBarrier.io.inUOP := uopc.invalid
  //   IF.io.PC := IF.io.PC // Freeze PC
  // }

  /* 
    TODO: Implement MUXes to select which values are sent to the EX stage as operands
  */

  // EX.io.operandA := IDBarrier.io.inOperandA // just there to make empty project buildable
  // EX.io.operandB := IDBarrier.io.inOperandB // just there to make empty project buildable

  EX.io.operandA := MuxLookup(forwardA, IDBarrier.io.outOperandA, Seq(
    "b10".U -> EXBarrier.io.outAluResult,  // Forward from EX stage
    "b01".U -> MEMBarrier.io.outAluResult // Forward from MEM stage
  ))
  EX.io.operandB := MuxLookup(forwardB, IDBarrier.io.outOperandB, Seq(
    "b10".U -> EXBarrier.io.outAluResult,  // Forward from EX stage
    "b01".U -> MEMBarrier.io.outAluResult // Forward from MEM stage
  ))  

  // IDBarrier --> EX
  EX.io.uop := IDBarrier.io.outUOP  

  // EX --> EXBarrier
  EXBarrier.io.inRD := IDBarrier.io.outRD
  EXBarrier.io.inAluResult := EX.io.aluResult
  printf("[EX] aluResult = %d\n", EX.io.aluResult)

  // EXBarrier --> MEMBarrier
  MEMBarrier.io.inRD := EXBarrier.io.outRD
  MEMBarrier.io.inAluResult := EXBarrier.io.outAluResult

  // MEMBarrier --> WB
  WB.io.rd := MEMBarrier.io.outRD
  WB.io.aluResult := MEMBarrier.io.outAluResult
  WB.io.regFileReq <> regFile.io.req_3

  // WB --> WBBarrier
  WBBarrier.io.inCheckRes := WB.io.check_res

  // Output result
  io.check_res := WBBarrier.io.outCheckRes
  printf("-------------------------------------------------------------------------\n")
}