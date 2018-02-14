import spatial.dsl._
import org.virtualized._

object P4 extends SpatialApp {
  import spatial.targets.DE1
  override val target = targets.Default



  /* Packet format:
   *
   *
   *	  |-------------------- 32 bits wide -------------------|
   *
   *	   -----------------------------------------------------
   *	  |  SRC Addr: 16 bits       |  DST Addr: 16 bits       | 
   *	   -----------------------------------------------------
   *
   */
 

  type UInt16 = FixPt[FALSE,_16,_0]
  type UInt32 = FixPt[FALSE,_32,_0]
  @struct case class Packet(src_addr: UInt16, dst_addr: UInt16)

  @virtualize
  def main() = {
    


	/* SRAM Dimensions
	 *
	 * - Dimension in X direction is the number of 
	 *   streams that can be operated on simultaneously.
	 *   This should be 16 (based on SIMD lanes in a PCU)
	 *   but is only 2 in this example.
	 * 
	 * - Dimension in Y direction is the number of fields
	 *   that can be held in a packet. This is 2 for now
	 *   but will be metaprogrammed based on the maximum 
	 *   packet size (1534 bytes from 802.1Q std)
	 *
	 */
	val num_fields = 2
	val max_streams = 2
	val num_pkts = 2

	val Parser_SRAM_dim_X = max_streams
	val Parser_SRAM_dim_Y = num_fields

	val Deparser_SRAM_dim_X = max_streams
	val Deparser_SRAM_dim_Y = num_fields


	/* Match Action Tables
	 *
	 * Match action tables are split into match tables which have
	 * which have lists of headers to match against, mask tables 
	 * and action tables which have a list of actions associated
	 * with each header.
	 *
	 * Ex.) If Src Addr == 1 {
	 * 		Src Addr = 10
	 *	}
	 *	Else If Src Addr == 2 {
	 *		Src Addr = 20
	 *	}
	 *
	 * Match Table: 
	 *
	 *	 ------------------
	 *	|    1   |    2    |
	 *       ------------------
	 *
	 * Src Addr == 1 so
	 *
	 * Mask Table:
	 *
	 *       ------------------
	 *	|    1   |    0    |
	 *	 ------------------
	 *
	 * Action Table (0 == set src addr to 10, 1 == set src addr to 20)
	 *
	 *	 --------
	 *	|   0    |
	 * 	 --------
	 *	|   1    |
	 *       --------
	 * 
	 * Mask Table has entry 0 set to 1 so action 0 is chosen
	 * and the src addr field is set to 10
	 *
	 */

	// Maximum number of actions allowed per match
	val max_actions = 1




	// Setup streams
	val stream_in0  = StreamIn[UInt32](DE1.GPInput1)
	val stream_in1  = StreamIn[UInt32](DE1.GPInput1)
    
	val stream_out0 = StreamOut[UInt32](DE1.GPOutput1)
	val stream_out1 = StreamOut[UInt32](DE1.GPOutput1)
    
    
    Accel(*) {
	
        val Parser_SRAM = SRAM[UInt16](Parser_SRAM_dim_X, Parser_SRAM_dim_Y)
        val Deparser_SRAM = SRAM[UInt16](Parser_SRAM_dim_X, Parser_SRAM_dim_Y)
 	
	val pkt0 = Reg[UInt32](0) 
	val pkt1 = Reg[UInt32](0) 

	// Pipeline parser stages

	Foreach(0 until num_fields, 0 until num_pkts ){ (i,j) =>
		val pkt0 = stream_in0.value
		val pkt1 = stream_in1.value
		val pkt = if (j == 0) pkt0 else pkt1 
		//val fld = pkt(i::i+16).as[UInt16]
		//val fld = pkt >> (16.to[UInt32]*i.to[UInt32]) | 0.to[UInt16] 
		//val fld = (pkt >> (i * 16.to[UInt32])).to[UInt16]
		//val fld_opt0 = pkt(0::16)
		//val fld_opt1 = pkt(16::16)
		//val fld = mux(pkt(0::16).to[UInt16], pkt(16::16).to[UInt16], i)
		val fld = if (i == 0) pkt(0::16).as[UInt16] else pkt(16::16).as[UInt16]



		Parser_SRAM(i, j) = fld
	}

	// Match Action Table (Add 1 to Src Addr or 2 to Src Addr)
	val num_matches = 2
	val match_table = LUT[UInt16](num_matches)(0.to[UInt16], 1.to[UInt16])
	val mask_table = SRAM[Boolean](max_streams, num_matches)
	val action_table = LUT[UInt16](num_matches, max_actions)(0.to[UInt16], 1.to[UInt16])
	
	//val match_table = SRAM[UInt16](num_matches)
	//val action_table = SRAM[UInt16](num_matches, max_actions)

	// Read all matches simultaneously
	Foreach(0 until num_matches, 0 until max_streams) { (i, j) =>
			
		// Hardcode field location in SRAM ???
		mask_table(i, j) = Parser_SRAM(0, i) == match_table(j)
	}

	// Check mask table 
	//Sequential.Foreach(0 until num_matches) {i=>
	//	Foreach(0 until max_streams) {j=>
	Foreach(0 until num_matches, 0 until max_streams) {(i,j)=>

			// Check if mask is 0 or 1 for a given match 
			val mask = mask_table(i, j)
			val header = Parser_SRAM(0, j)
			val action = action_table(i, 0)
			val new_header =  if (!mask) header else if (action==0.to[UInt16]) header + 1 else if(action==1.to[UInt16]) header + 2 else header + 3
				
			// Read result into deparser SRAM
			Deparser_SRAM(0, i) = new_header
	}
	


	
	Foreach(0 until num_fields, 0 until num_pkts){ (i,j) =>
		val fld = Deparser_SRAM(i, j)
		if (j == 0){
			stream_out0 := fld.to[UInt32]
		}
		else{
			stream_out1 := fld.to[UInt32]
		}


	}
	
	
   // End Accel
   }


  // End Main
  } 


// End Spatial App
}
