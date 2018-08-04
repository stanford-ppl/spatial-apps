import spatial.dsl._
import virtualized._

object ecn extends SpatialApp with Hashes{
  override val target = targets.Plasticine
  import target._


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
  type UInt64 = FixPt[FALSE,_64,_0]
  @struct case class Packet(src_addr: UInt16, dst_addr: UInt16)

  @virtualize
  def main() = {
    

	// Setup streams
	//val stream_in0  = StreamIn[UInt32](GPInput1)
	//val stream_out0 = StreamOut[UInt32](GPOutput1)
    	
	val stream_count = 1024l
	
	//val ipv4_src_addr = ArgIn[UInt32]
	val ipv4_src_addr_in  = StreamIn[UInt32](GPInput1); countOf(ipv4_src_addr_in) = stream_count
	
	//val ipv4_dst_addr = ArgIn[UInt32]
	val ipv4_dst_addr_in  = StreamIn[UInt32](GPInput1); countOf(ipv4_dst_addr_in) = stream_count
	
	//val ttl = ArgIn[UInt32]
	val ttl_in  = StreamIn[UInt32](GPInput1); countOf(ttl_in) = stream_count

	//val mac_src_addr_1 = ArgIn[UInt32]
	//val mac_src_addr_2 = ArgIn[UInt32]
	val mac_src_addr_1_in  = StreamIn[UInt32](GPInput1); countOf(mac_src_addr_1_in) = stream_count
	val mac_src_addr_2_in  = StreamIn[UInt32](GPInput1); countOf(mac_src_addr_2_in) = stream_count

	//val mac_dst_addr_1 = ArgIn[UInt32]
	//val mac_dst_addr_2 = ArgIn[UInt32]
	val mac_dst_addr_1_in  = StreamIn[UInt32](GPInput1); countOf(mac_dst_addr_1_in) = stream_count
	val mac_dst_addr_2_in  = StreamIn[UInt32](GPInput1); countOf(mac_dst_addr_2_in) = stream_count

	//val egress_spec = ArgIn[UInt32]
	val egress_spec_in  = StreamIn[UInt32](GPInput1); countOf(egress_spec_in) = stream_count
	
	//val ecn = ArgIn[UInt32]
	val ecn_in  = StreamIn[UInt32](GPInput1); countOf(ecn_in) = stream_count
	
	//val queue_delay = ArgIn[UInt32]
	val queue_delay_in  = StreamIn[UInt32](GPInput1); countOf(queue_delay_in) = stream_count
	
	//val drop = ArgIn[UInt32]
	val drop_in  = StreamIn[UInt32](GPInput1); countOf(drop_in) = stream_count

	/*
	val ipv4_src_addr_out = ArgOut[UInt32]
	val egress_spec_out = ArgOut[UInt32]
	val ecn_out = ArgOut[UInt32]
	val drop_out = ArgOut[UInt32]
	*/

	val ipv4_src_addr_out = StreamOut[UInt32](GPOutput1)
	val egress_spec_out = StreamOut[UInt32](GPOutput1)
	val ecn_out = StreamOut[UInt32](GPOutput1)
	val drop_out = StreamOut[UInt32](GPOutput1)
	
	/*
	setArg(ipv4_src_addr, args(0).to[UInt32])
	setArg(ipv4_dst_addr, args(1).to[UInt32])
	setArg(ttl, args(2).to[UInt32])

	setArg(mac_src_addr_1, args(3).to[UInt32])
	setArg(mac_src_addr_2, args(4).to[UInt32])
	
	setArg(mac_dst_addr_1, args(5).to[UInt32])
	setArg(mac_dst_addr_2, args(6).to[UInt32])
	
	setArg(egress_spec, args(7).to[UInt32])
	setArg(ecn, args(8).to[UInt32])
	
	setArg(queue_delay, args(9).to[UInt32])
	*/


	val self_addr = 42
	
	val project_dir = s"${sys.env("SPATIAL_HOME")}/apps/src/spatial-network-apps/ACTION_LUTs/"
	val Action1024 = project_dir + "Action1024.csv"

	Accel(*) {
	
		val action_table_1 = LUT.fromFile[UInt32](1024)(Action1024)
		val action_reg_1 = Reg[UInt32](0)
		val ipv4_src_addr_reg = Reg[UInt32](0)
	

		Pipe {

			ipv4_src_addr_reg := ipv4_src_addr_in.value
			val ipv4_dst_addr = ipv4_dst_addr_in.value
			val ttl  = ttl_in.value
			val mac_src_addr_1 = mac_src_addr_1_in.value
			val mac_src_addr_2 = mac_src_addr_2_in.value
			val mac_dst_addr_1 = mac_dst_addr_1_in.value
			val mac_dst_addr_2 = mac_dst_addr_2_in.value
			val egress_spec = egress_spec_in.value 
			val ecn = ecn_in.value
			val queue_delay = queue_delay_in.value
			val drop = drop_in.value

		}

		val idx = Reg[Index](0)
		// Check if mask is 0 or 1 for a given match 
		Pipe {
			val header = ipv4_src_addr_reg
			idx := murmur(header)

		}

		Pipe {
			action_reg_1 := action_table_1(idx)
			val action = action_reg_1.value
			val set_ipv4_action = mux(action.to[UInt32]==1.to[UInt32], self_addr.to[UInt32], 0.to[UInt32])
			val set_egress_spec_action = mux(action==1.to[UInt32], 1.to[UInt32], 2.to[UInt32])
			val set_ecn_action = mux(action==1.to[UInt32], 3.to[UInt32], 0.to[UInt32])
			
			val set_drop_action = mux(action==2.to[UInt32], 1.to[UInt32], 0.to[UInt32])


			ipv4_src_addr_out := set_ipv4_action
			egress_spec_out := set_egress_spec_action
			ecn_out := set_ecn_action
			drop_out := set_drop_action
		}

   
	// End Accel
	}

	/*
	val ipv4 = getArg(ipv4_src_addr_out)
	val spec = getArg(egress_spec_out)
	val e = getArg(ecn_out)
	val d = getArg(drop_out)

	print("IPv4: " + ipv4 + " | Egress Spec: " + spec + " | ECN: " + e + " | Drop: " + d)
	*/
  // End Main
  } 


// End Spatial App
}
