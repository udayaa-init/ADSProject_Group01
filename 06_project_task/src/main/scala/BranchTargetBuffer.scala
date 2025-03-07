/*
The goal of this task is to implement a Branch Target Buffer (BTB).


1. How does a higher associativity affect the performance of a cache?
  - The increased associativity improves cache hit rates but at the cost of more complex logic and possibly higher latency.
2. What’s the best initial state for the 2-bit predictor FSM? What effects would different initial states have in regular program patterns (e.g., loops)?
3. Find a way to implement a structure of registers with 8 sets and 2 ways. Standard memory classes might not be a good option here.
4. How can the number of index bits be determined?
  - We will use 3 bits to address the 8 sets
5. How many bits does the tag of each BTB entry need in a configuration with 8 sets and 2 ways?
  - The tag should be 32 - 2(always 0) -3(Index) = 27

*/

package BranchTargetBuffer

import chisel3._
import chisel3.util._

/*
Each entry in the BTB should include:
• A valid bit             - to indicate whether the entry contains usable data.
• A tag                   - to identify the branch instruction associated with the entry.
• A branch target address - which provides the predicted next program counter (PC) when the branch is taken.
• A 2-bit predictor state - indicate whether the branch is predicted to be taken or not.

-   We are finding the entry for a 32-bit PC
  -   Last two bits [0:1] are always zero
  -   We will use 3 bits [2:4] to address the 8 sets
  -   The tag [31:5] should be 32 - 2(always 0) -3(Index) = 27
*/

class BTBEntry extends Bundle{
  val valid = Bool()
  val tag = UInt(27.W) 
  val target = UInt(32.W)
  val predictor = UInt(2.W)
}

class BranchTargetBuffer () extends Module {

  val io = IO(new Bundle {
    val PC = Input(UInt(32.W))
    val update = Input(Bool())
    val updatePC = Input(UInt(32.W))
    val updateTarget = Input(UInt(32.W))
    val mispredicted = Input(Bool())

    val valid = Output(Bool())
    val target = Output(UInt(32.W))
    val predictedTaken = Output(Bool())
  })

  // Initialize Outputs
  io.valid  := 0.U
  io.target := 0.U
  io.predictedTaken := 0.U

  // The BTB consists of 8 sets, each containing 2 ways, making it a 2-way set-associative structure.  
  val numSets = 8
  val numWays = 2
  val cache = Mem(numSets, Vec(numWays, new BTBEntry))
 
  // LRU bit for each set (false = way0, true = way1)
  val lru = RegInit(VecInit(Seq.fill(8)(false.B))) 

  // Extracting the index, tag, and block offset from the address
  printf("PC  = %d\n",io.PC)
  
  val index = io.PC(4,2)  
  val tag = io.PC(31,5)  
  val target = io.PC + 4.U  // Assuming branch not taken

  printf("index  = %d\n",io.PC(4,2))
  printf("tag  = %d\n",io.PC(31,5))

  // Assign values to output
  io.target := target

}