/*
The goal of this task is to implement a Branch Target Buffer (BTB).

The BTB consists of 8 sets, each containing 2 ways, making it a 2-way set-associative structure. Each entry in the BTB should include:
• A valid bit             - to indicate whether the entry contains usable data.
• A tag                   - to identify the branch instruction associated with the entry.
• A branch target address - which provides the predicted next program counter (PC) when the branch is taken.
• A 2-bit predictor state - indicate whether the branch is predicted to be taken or not.

1. How does a higher associativity affect the performance of a cache?
2. What’s the best initial state for the 2-bit predictor FSM? What effects would different initial states have in regular program patterns (e.g., loops)?
3. Find a way to implement a structure of registers with 8 sets and 2 ways. Standard memory classes might not be a good option here.
4. How can the number of index bits be determined?
5. How many bits does the tag of each BTB entry need in a configuration with 8 sets and 2 ways?

*/

package BranchTargetBuffer

import chisel3._
import chisel3.util._

class BranchTargetBuffer () extends Module {

  // val io = IO(new Bundle {
  //   val PC = Input(UInt(32.W))
  //   val update = Input(Bool())
  //   val updatePC = Input(UInt(32.W))
  //   val updateTarget = Input(UInt(32.W))
  //   val mispredicted = Input(Bool())

  //   val valid = Output(Bool())
  //   val target = Output(UInt(32.W))
  //   val predictedTaken = Output(Bool())
  // })

}