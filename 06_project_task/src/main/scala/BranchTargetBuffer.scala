/*
The goal of this task is to implement a Branch Target Buffer (BTB).


1. How does a higher associativity affect the performance of a cache?
  - The increased associativity improves cache hit rates but at the cost of more complex logic and possibly higher latency.
2. What’s the best initial state for the 2-bit predictor FSM? What effects would different initial states have in regular program patterns (e.g., loops)?
  - Weak Not taken
    Refernece [https://www.youtube.com/watch?v=AWv8DCm_UYE]
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

  printf("//// Read Cache\n")

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
  
  printf("index  = %d\n",io.PC(4,2))
  printf("tag  = %d\n",io.PC(31,5))

  // Fetching the BTB entry based on the index
  val set = cache(index)
  
  // Logic to check for a hit in the cache (valid and tag comparison)
  val hit_vector = set.map { way =>
    // Check for valid entry and matching tag
    way.valid && (way.tag === tag)  
  }
  

  

  
  //printf("predictedTaken  = %d\n",predictedTaken)

  printf("//// Update Cache\n")


  // Determine if there's a matching entry
  val IFway0 = cache(index)(0)
  val IFway1 = cache(index)(1)

  val IFhit0 = IFway0.valid && (IFway0.tag === tag)
  val IFhit1 = IFway1.valid && (IFway1.tag === tag)

  val IFupdateWay = Mux(IFhit0, 0.U, Mux(IFhit1, 1.U, Mux(lru(index), 1.U, 0.U)))

  // Cache hit if any way is valid and matches the tag
  val valid = hit_vector.reduce(_ || _)  

  // set the taget from the cache, if not set PC + 4
  val target = Mux(hit_vector(0), set(0).target, Mux(hit_vector(1), set(1).target, io.PC + 4.U))

  // Check for cache hit and set prediction state (taken or not)
  val predictedTaken = Mux(hit_vector(0), set(0).predictor(1), Mux(hit_vector(1), set(1).predictor(1), false.B))

  when(valid){
    //Update the LRU here
    val Ind  = io.PC(4,2)
    lru(Ind) := !IFupdateWay 

  }.otherwise{
    // Make an entry here
    cache(index)(IFupdateWay).valid := true.B
    cache(index)(IFupdateWay).tag := tag
    cache(index)(IFupdateWay).target := io.PC + 4.U // if no entry in the BTB when fecting we set target to PC+4
    // We are starting at Weak Not Taken, so the next state is set accordingly
    cache(index)(IFupdateWay).predictor := 1.U
  }

  // BTB Update Logic (Occurs 2 cycles later at the EX stage)
  when(io.update) {
    val updateIndex = io.updatePC(4, 2)
    val updateTag = io.updatePC(31, 5)

    // Determine if there's a matching entry
    val way0 = cache(updateIndex)(0)
    val way1 = cache(updateIndex)(1)

    val hit0 = way0.valid && (way0.tag === updateTag)
    val hit1 = way1.valid && (way1.tag === updateTag)

    val updateWay = Mux(hit0, 0.U, Mux(hit1, 1.U, Mux(lru(updateIndex), 1.U, 0.U)))

    /* In a 2-bit predictor state machine, we have the following states:
        - 00 (Strong Not Taken)
            The next states are:
            - 00, when mispredicted 0
            - 01, when mispredicted 1
        - 01 (Weak Not Taken)
            The next states are:
            - 00, when mispredicted 0
            - 10, when mispredicted 1
        - 10 (Strong Taken)
            The next states are:
            - 10, when mispredicted 0
            - 11, when mispredicted 1            
        - 11 (Weak Taken)
            The next states are:
            - 10, when mispredicted 0
            - 00, when mispredicted 1            
        
        io.mispredicted tells us if our prediction, as defined by MSB was wrong

        We also notice that it acts like a two bit counter when misprediction is always 1
        Additionaly, for weak convictions we can do a -1 when the prediction was correct
    */

    // If no existing entry, create a new one
    when(!hit0 && !hit1) {
      cache(updateIndex)(updateWay).valid := true.B
      cache(updateIndex)(updateWay).tag := updateTag
      cache(updateIndex)(updateWay).target := io.updateTarget

      printf("new prediction set\n")

      // We are starting at Weak Not Taken, so the next state is set accordingly
      cache(updateIndex)(updateWay).predictor := Mux(io.mispredicted,
        2.U, // Move towards Strongly Taken
        0.U // Move towards Strongly Not Taken
      )
    }.otherwise {
      // Update predictor FSM for existing entry
      val oldState = cache(updateIndex)(updateWay).predictor
      cache(updateIndex)(updateWay).valid := true.B
      cache(updateIndex)(updateWay).target := io.updateTarget
      printf("current prediction  = %d\n",oldState)
      
      cache(updateIndex)(updateWay).predictor := Mux(io.mispredicted,
        oldState + 1.U,                                      // 2-bit counter behaviour
        Mux(oldState(0) === 1.U, oldState, oldState - 1.U) // -1 for weak convictions

      )
    }
        
    
    // since this way was just updated the other way is lru
    // Update LRU bit
    lru(updateIndex) := !updateWay  // happens for the latest updatePC if collision with the IF reading
  }

  // Assign values to output
  io.target := target
  io.valid :=valid
  io.predictedTaken := predictedTaken

}