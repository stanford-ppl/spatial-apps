import spatial._
import org.virtualized._

object AES extends SpatialApp { // Regression (Dense) // Args: none
  import IR._

  /*
  TODO: Optimize/parallelize many of the memory accesses here and pipeline as much as possible
  
  MachSuite Concerns: 
  	- Mix rows math seemed wrong in their implementation
  	- Not exactly sure what was going on with their expand_key step
	*/
  type UInt8 = FixPt[FALSE,_8,_0]
  @virtualize
  def main() = {
  	// Setup off-chip data
	  val plaintext = Array[UInt8](0,17,34,51,68,85,102,119,136,153,170,187,204,221,238,255)
	  val key = Array.tabulate(32){i => i.to[UInt8]}
  	val sbox = Array[UInt8]( // 256 elements
	    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
	    0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
	    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
	    0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
	    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc,
	    0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
	    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a,
	    0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
	    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
	    0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
	    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
	    0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
	    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85,
	    0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
	    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
	    0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
	    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17,
	    0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
	    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88,
	    0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
	    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
	    0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
	    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9,
	    0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
	    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6,
	    0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
	    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
	    0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
	    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94,
	    0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
	    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68,
	    0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
	  )
  	
  	// Create DRAMs
  	val plaintext_dram = DRAM[UInt8](16)
  	val key_dram = DRAM[UInt8](32)
  	val sbox_dram = DRAM[UInt8](256)
  	val ciphertext_dram = DRAM[UInt8](16)

  	// Transfer data to DRAMs
  	setMem(plaintext_dram, plaintext)
  	setMem(key_dram, key)
  	setMem(sbox_dram, sbox)

  	// Debugging support
    val niter = 15
  	// val niter = ArgIn[Int]
  	// setArg(niter, args(0).to[Int])
  	// val key_debug = DRAM[UInt8](32)

  	Accel{
  		// Setup data structures
  		val plaintext_flat = SRAM[UInt8](16)
  		val plaintext_sram = SRAM[UInt8](4,4)
  		val sbox_sram = SRAM[UInt8](256)
  		val key_sram = SRAM[UInt8](32)
  		// val mix_lut = LUT[Int](4,4)(
  		// 		2, 3, 1, 1,
  		// 		1, 2, 3, 1,
  		// 		1, 1, 2, 3,
  		// 		3, 1, 1, 2
  		// 	)
  		val rcon = Reg[UInt8](1)

  		// Specify methods
		  def expand_key(): Unit = {
		    Pipe{key_sram(0) = key_sram(0) ^ sbox_sram(key_sram(29).as[UInt16].as[Int]) ^ rcon}
		    Pipe{key_sram(1) = key_sram(1) ^ sbox_sram(key_sram(30).as[UInt16].as[Int])}
		    Pipe{key_sram(2) = key_sram(2) ^ sbox_sram(key_sram(31).as[UInt16].as[Int])}
		    Pipe{key_sram(3) = key_sram(3) ^ sbox_sram(key_sram(28).as[UInt16].as[Int])}
		    rcon := (((rcon)<<1) ^ ((((rcon)>>7) & 1) * 0x1b))

		    Sequential.Foreach(4 until 16 by 4) {i =>
		    	Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
		    	Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
		    	Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
		    	Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
		    }
			
				Pipe{key_sram(16) = key_sram(16) ^ sbox_sram(key_sram(12).as[UInt16].as[Int])}
				Pipe{key_sram(17) = key_sram(17) ^ sbox_sram(key_sram(13).as[UInt16].as[Int])}
				Pipe{key_sram(18) = key_sram(18) ^ sbox_sram(key_sram(14).as[UInt16].as[Int])}
				Pipe{key_sram(19) = key_sram(19) ^ sbox_sram(key_sram(15).as[UInt16].as[Int])}

				Sequential.Foreach(20 until 32 by 4) {i => 
					Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
					Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
					Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
					Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
				}
		  }

		  def shift_rows(): Unit = {
	  		Sequential.Foreach(4 by 1){ i => 
	  			val row = RegFile[UInt8](4)
	  			Foreach(4 by 1){ j => 
		  			val col_addr = (j - i) % 4
		  			row(col_addr) = plaintext_sram(i,j)
		  		}
		  		Foreach(4 by 1){ j => 
		  			plaintext_sram(i,j) = row(j)
		  		}
	  		}
		  }

		  def substitute_bytes(): Unit = {
	  		Sequential.Foreach(4 by 1, 4 by 1){(i,j) => 
	  			val addr = plaintext_sram(i,j).as[UInt16].as[Int] // Upcast without sign-extend
	  			val subst = sbox_sram(addr)
	  			plaintext_sram(i,j) = subst
	  		}
		  }

			def rj_xtime(x: UInt8): UInt8 = {
				mux(((x & 0x80.to[UInt8]) > 0.to[UInt8]), ((x << 1) ^ 0x1b.to[UInt8]), x << 1)
			}

		  def mix_columns(): Unit = {
	  		Sequential.Foreach(4 by 1){j => 
		  		val col = RegFile[UInt8](4)
		  		Sequential.Foreach(4 by 1) { i => col(i) = plaintext_sram(i,j) }
		  		val e = Reduce(Reg[UInt8](0))(4 by 1 par 4) { i => col(i) }{_^_}
		  		// val e = col(0) ^ col(1) ^ col(2) ^ col(3)
		  		Pipe{plaintext_sram(0,j) = col(0) ^ e ^ rj_xtime(col(0) ^ col(1))}
		  		Pipe{plaintext_sram(1,j) = col(1) ^ e ^ rj_xtime(col(1) ^ col(2))}
		  		Pipe{plaintext_sram(2,j) = col(2) ^ e ^ rj_xtime(col(2) ^ col(3))}
		  		Pipe{plaintext_sram(3,j) = col(3) ^ e ^ rj_xtime(col(3) ^ col(0))}
	  		}
		  }

		  def add_round_key(round: Index): Unit = {
	  		Foreach(4 by 1, 4 by 1) { (i,j) => 
	  			val key = mux(round % 2 == 1, key_sram(i+j*4+16), key_sram(i+j*4))
	  			plaintext_sram(i,j) = plaintext_sram(i,j) ^ key
	  		}
		  }

  		// Load structures
  		Parallel {
	  		plaintext_flat load plaintext_dram // TODO: Allow dram loads to reshape (gh issue #83)
	  		sbox_sram load sbox_dram
	  		key_sram load key_dram
	  	}

	  	// gh issue #83
	  	Foreach(4 by 1, 4 by 1){(i,j) => 
	  		plaintext_sram(i,j) = plaintext_flat(j*4+i) // MachSuite flattens columnwise... Why????
	  	} 

	  	// Do AES
	  	Sequential.Foreach(niter by 1) { round => 
	  		// SubBytes
	  		if (round > 0) {
	  			Pipe{substitute_bytes()}
	  		}

	  		// ShiftRows
	  		if (round > 0) {
	  			Pipe{shift_rows()}
	  		}

	  		// MixColumns
	  		if (round > 0 && round < 14 ) {
	  			Pipe{mix_columns()}
	  		}

	  		// Expand key
	  		if (round > 0 && ((round % 2) == 0)) {
	  			Pipe{expand_key()}
	  		}

	  		// AddRoundKey
	  		add_round_key(round)

	  	}

	  	// Reshape plaintext_sram (gh issue # 83)
	  	Foreach(4 by 1, 4 by 1) {(i,j) => 
	  		plaintext_flat(j*4+i) = plaintext_sram(i,j)
	  	}

	  	ciphertext_dram store plaintext_flat

	  	// // Debugging
	  	// key_debug store key_sram

  	}

  	val ciphertext = getMem(ciphertext_dram)
  	val ciphertext_gold = Array[UInt8](142,162,183,202,81,103,69,191,234,252,73,144,75,73,96,137)

  	printArray(ciphertext_gold, "Expected: ")
  	printArray(ciphertext, "Got: ")

  	// // Debugging
  	// val key_dbg = getMem(key_debug)
  	// printArray(key_dbg, "Key: ")

  	val cksum = ciphertext_gold.zip(ciphertext){_ == _}.reduce{_&&_}
  	println("PASS: " + cksum + " (AES) * For retiming, need to fix ^ reduction if not parallelized")

  }
}


object Viterbi extends SpatialApp { // Regression (Dense) // Args: none
  import IR._

  /*

                    ←       N_OBS            →

          State 63 ----- State 63 ----- State 63                
        /  .        \ /            \ /                     P(obs | state) = emission_probs   
       /   .         X              X                                         (N_STATES x N_TOKENS)
      /    .        / \            / \                        
     O----State k  ----- State k  ----- State k  ...            
      \    .        \ /            \ /                          
       \   .         X              X                          shortest path to (state, obs) = llike
        \  .        / \            / \                                                          (N_OBS x N_STATES)
          State 0  ----- State 0  ----- State 0                  
      ↑               ↑                                                           
    init_states     transition_probs                        
     (N_STATES)       (N_STATES x N_STATES)

						obs_vec
						 (N_OBS, max value = N_TOKENS)


	TODO: Eliminate backprop step and do everything feed-forward
	MachSuite Concerns:
		- Constructing path step by step seems to give the wrong result because they do extra math in the backprop step. 
		       Why do you need to do math when going backwards? I thought you just read off the result

  */

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() = {
  	// Setup dimensions of problem
  	val N_STATES = 64
  	val N_TOKENS = 64
  	val N_OBS = 140

  	// debugging
  	val steps_to_take = N_OBS //ArgIn[Int] //
  	// setArg(steps_to_take, args(0).to[Int])

  	// Setup data
  	val init_states = Array[T](4.6977033615112305.to[T],3.6915655136108398.to[T],4.8652229309082031.to[T],4.7658410072326660.to[T],
  		4.0006790161132812.to[T],3.9517300128936768.to[T],3.4640796184539795.to[T],3.4600069522857666.to[T],4.2856273651123047.to[T],
  		3.6522088050842285.to[T],4.8189344406127930.to[T],3.8075556755065918.to[T],3.8743767738342285.to[T],5.4135279655456543.to[T],
  		4.9173111915588379.to[T],3.6458325386047363.to[T],5.8528852462768555.to[T],11.3210048675537109.to[T],4.9971127510070801.to[T],
  		5.1006979942321777.to[T],3.5980830192565918.to[T],5.3161897659301758.to[T],3.4544019699096680.to[T],3.7314746379852295.to[T],
  		4.9998908042907715.to[T],3.4898567199707031.to[T],4.2091164588928223.to[T],3.5122559070587158.to[T],3.9326364994049072.to[T],
  		7.2767667770385742.to[T],3.6539671421051025.to[T],4.0916681289672852.to[T],3.5044839382171631.to[T],4.5234117507934570.to[T],
  		3.7673256397247314.to[T],4.0265331268310547.to[T],3.7147023677825928.to[T],6.7589721679687500.to[T],3.5749390125274658.to[T],
  		3.7701597213745117.to[T],3.5728175640106201.to[T],5.0258340835571289.to[T],4.9390106201171875.to[T],5.7208223342895508.to[T],
  		6.3652114868164062.to[T],3.5838112831115723.to[T],5.0102572441101074.to[T],4.0017414093017578.to[T],4.2373661994934082.to[T],
  		3.8841004371643066.to[T],5.3679313659667969.to[T],3.9980680942535400.to[T],3.5181968212127686.to[T],4.7306714057922363.to[T],
  		5.5075111389160156.to[T],5.1880970001220703.to[T],4.8259010314941406.to[T],4.2589011192321777.to[T],5.6381106376647949.to[T],
  		3.4522385597229004.to[T],3.5920252799987793.to[T],4.2071061134338379.to[T],5.0856294631958008.to[T],6.0637059211730957.to[T])

  	val obs_vec = Array[Int](0,27,49,52,20,31,63,63,29,0,47,4,38,38,38,38,4,43,7,28,31,
  											 7,7,7,57,2,2,43,52,52,43,3,43,13,54,44,51,32,9,9,15,45,21,
  											 33,61,45,62,0,55,15,55,30,13,13,53,13,13,50,57,57,34,26,21,
  											 43,7,12,41,41,41,17,17,30,41,8,58,58,58,31,52,54,54,54,54,
  											 54,54,15,54,54,54,54,52,56,52,21,21,21,28,18,18,15,40,1,62,
  											 40,6,46,24,47,2,2,53,41,0,55,38,5,57,57,57,57,14,57,34,37,
  											 57,30,30,5,1,5,62,25,59,5,2,43,30,26,38,38)

  	val raw_transitions = loadCSV1D[T]("/remote/regression/data/machsuite/viterbi_transition.csv", "\n")
  	val raw_emissions = loadCSV1D[T]("/remote/regression/data/machsuite/viterbi_emission.csv", "\n")
  	val transitions = raw_transitions.reshape(N_STATES, N_STATES)
  	val emissions = raw_emissions.reshape(N_STATES, N_TOKENS)

  	val correct_path = Array[Int](27,27,27,27,27,31,63,63,63,63,47,4,38,38,38,38,7,7,7,
  																7,7,7,7,7,2,2,2,43,52,52,43,43,43,43,43,44,44,32,9,9,
  																15,45,45,45,45,45,45,0,55,55,55,30,13,13,13,13,13,13,
  																57,57,21,21,21,21,7,41,41,41,41,17,17,30,41,41,58,58,
  																58,31,54,54,54,54,54,54,54,54,54,54,54,54,52,52,52,21,
  																21,21,28,18,18,40,40,40,40,40,40,46,46,2,2,2,53,53,53,
  																55,38,57,57,57,57,57,57,57,57,57,57,30,30,5,5,5,5,5,5,
  																5,5,30,30,26,38,38)
  	// Handle DRAMs
  	val init_dram = DRAM[T](N_STATES)
  	val obs_dram = DRAM[Int](N_OBS)
  	val transitions_dram = DRAM[T](N_STATES,N_STATES)
  	val emissions_dram = DRAM[T](N_STATES,N_TOKENS)
  	// val llike_dram = DRAM[T](N_OBS,N_STATES)
  	val path_dram = DRAM[Int](N_OBS)
  	setMem(init_dram,init_states)
  	setMem(obs_dram, obs_vec)
  	setMem(transitions_dram,transitions)
  	setMem(emissions_dram,emissions)


  	Accel{
  		// Load data structures
  		val obs_sram = SRAM[Int](N_OBS)
  		val init_sram = SRAM[T](N_STATES)
  		val transitions_sram = SRAM[T](N_STATES,N_STATES)
  		val emissions_sram = SRAM[T](N_STATES,N_TOKENS)
  		val llike_sram = SRAM[T](N_OBS, N_STATES)
  		val path_sram = SRAM[Int](N_OBS)

  		Parallel{
  			obs_sram load obs_dram
  			init_sram load init_dram
  			transitions_sram load transitions_dram
  			emissions_sram load emissions_dram
  		}

  		// from --> to
  		Sequential.Foreach(0 until steps_to_take) { step => 
  			val obs = obs_sram(step)
  			Sequential.Foreach(0 until N_STATES) { to => 
	  			val emission = emissions_sram(to, obs)
  				val best_hop = Reg[T](0x4000)
  				best_hop.reset
  				Reduce(best_hop)(0 until N_STATES) { from => 
  					val base = llike_sram((step-1) % N_OBS, from) + transitions_sram(from,to)
  					base + emission
  				} { (a,b) => mux(a < b, a, b)}
  				llike_sram(step,to) = mux(step == 0, emission + init_sram(to), best_hop)
  			}
  		}

  		// to <-- from
  		Sequential.Foreach(steps_to_take-1 until -1 by -1) { step => 
  			val from = path_sram(step+1)
  			val min_pack = Reg[Tup2[Int, T]](pack(-1.to[Int], (0x4000).to[T]))
  			min_pack.reset
  			Reduce(min_pack)(0 until N_STATES){ to => 
  				val jump_cost = mux(step == steps_to_take-1, 0.to[T], transitions_sram(to, from))
  				val p = llike_sram(step,to) + jump_cost
  				pack(to,p)
  			}{(a,b) => mux(a._2 < b._2, a, b)}
  			path_sram(step) = min_pack._1
  		}

  		// Store results
  		// llike_dram store llike_sram
  		path_dram store path_sram
  	}

  	// Get data structures
  	// val llike = getMatrix(llike_dram)
  	val path = getMem(path_dram)

  	// Print data structures
  	// printMatrix(llike, "log-likelihood")
  	printArray(path, "path taken")
  	printArray(correct_path, "correct path")

  	// Check results
  	val cksum = correct_path.zip(path){_ == _}.reduce{_&&_}
  	println("PASS: " + cksum + " (Viterbi)")

  }
}


object Stencil2D extends SpatialApp { // Regression (Dense) // Args: none
  import IR._

  /*
           ←    COLS     →   
         ___________________             ___________________                         
        |                   |           |X  X  X  X  X  X 00|          
    ↑   |    ←3→            |           |                 00|          
        |    ___            |           |    VALID DATA   00|          
        |  ↑|   |           |           |X  X  X  X  X  X 00|          
        |  3|   | ----->    |    ->     |                 00|            
 ROWS   |  ↓|___|           |           |X  X  X  X  X  X 00|          
        |                   |           |                 00|          
        |                   |           |X  X  X  X  X  X 00|          
        |                   |           |                 00|          
    ↓   |                   |           |0000000000000000000|          
        |                   |           |0000000000000000000|          
         ```````````````````             ```````````````````      
                                               
                                               
  */


  @virtualize
  def main() = {

  	// Problem properties
  	val ROWS = 128
  	val COLS = 64
  	val filter_size = 9

  	// Setup data
  	val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil2d_data.csv", "\n")
  	val data = raw_data.reshape(ROWS, COLS)

  	// Setup DRAMs
  	val data_dram = DRAM[Int](ROWS,COLS)
  	val result_dram = DRAM[Int](ROWS,COLS)

  	setMem(data_dram, data)

  	Accel {

	  	val filter = LUT[Int](3,3)(379,909,468, // Reverse columns because we shift in from left side
	  														 771,886,165,
	  														 553,963,159)
	  	val lb = LineBuffer[Int](3,COLS)
	  	val sr = RegFile[Int](3,3)
	  	val result_sram = SRAM[Int](ROWS,COLS)
	  	Foreach(ROWS by 1){ i => 
				val wr_row = (i-2)%ROWS
	  		lb load data_dram(i, 0::COLS)
				Foreach(COLS by 1) {j => 
					Foreach(3 by 1 par 3) {k => sr(k,*) <<= lb(k,j)}
					val temp = Reduce(Reg[Int](0))(3 by 1, 3 by 1){(r,c) => sr(r,c) * filter(r,c)}{_+_}
					val wr_col = (j-2)%COLS
					if (i >= 2 && j >= 2) {result_sram(wr_row,wr_col) = temp}
					else {result_sram(wr_row,wr_col) = 0}
				}	  		
	  	}

	  	result_dram store result_sram
  	}

  	// Get results
  	val result_data = getMatrix(result_dram)
  	val raw_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil2d_gold.csv", "\n")
  	val gold = raw_gold.reshape(ROWS,COLS)

  	// Printers
  	printMatrix(gold, "gold")
  	printMatrix(result_data, "result")

  	val cksum = (0::ROWS, 0::COLS){(i,j) => if (i < ROWS-2 && j < COLS-2) gold(i,j) == result_data(i,j) else true }.reduce{_&&_}
  	println("PASS: " + cksum + " (Stencil2D) * Fix modulo addresses in scalagen?")

  }
}


object Stencil3D extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
                                                                                                                             
 H   ↗        ___________________                  ___________________                                                                  
  E         /                   /|               /000000000000000000 /|                                                                
   I       / ←    ROW      →   / |              / x  x  x  x  x  00 /0|                        
 ↙  G     /__________________ /  |             /__________________ / 0|                                                                 
     H   |                   |   |            |X  X  X  X  X  X 00| x0|      
      T  |     ___           |   |            |                 00|  0|      
         |    /__/|          |   |            |    VALID DATA   00|  0|      
   ↑     |  ↑|   ||          |   |            |X  X  X  X  X  X 00| x0|      
         |  3|   || ----->   |   |   --->     |                 00|  0|        
  COL    |  ↓|___|/          |   |            |X  X  X  X  X  X 00| x0|      
         |                   |   |            |                 00|  0|      
         |                   |   |            |X  X  X  X  X  X 00| x0|      
         |                   |  /             |                 00| 0/      
   ↓     |                   | /              |0000000000000000000|0/ 
         |                   |/               |0000000000000000000|/        
          ```````````````````                  ```````````````````      
                                                
                                                
 */


  @virtualize
  def main() = {

   	// Problem properties
   	val ROWS = 16 // Leading dim
   	val COLS = 32
    val HEIGHT = 32
    // val num_slices = ArgIn[Int]
    // setArg(num_slices, args(0).to[Int])
    val num_slices = HEIGHT
   	val filter_size = 3*3*3

   	// Setup data
   	val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil3d_data.csv", "\n")
   	val data = raw_data.reshape(HEIGHT, COLS, ROWS)

   	// Setup DRAMs
   	val data_dram = DRAM[Int](HEIGHT, COLS, ROWS)
   	val result_dram = DRAM[Int](HEIGHT, COLS, ROWS)

   	setMem(data_dram, data)

   	Accel {
      val filter = LUT[Int](3,3,3)(   0,  0,  0,
                                      0, -1,  0,
                                      0,  0,  0,

                                      0, -1,  0,
                                     -1,  6, -1,
                                      0, -1,  0,

                                      0,  0,  0,
                                      0, -1,  0,
                                      0,  0,  0)

      val result_sram = SRAM[Int](HEIGHT,COLS,ROWS)
      val temp_slice = SRAM[Int](COLS,ROWS)

      Foreach(num_slices by 1) { p => 
        MemReduce(temp_slice)(-1 until 2 by 1) { slice => 
          val local_slice = SRAM[Int](COLS,ROWS)
          val lb = LineBuffer[Int](3,ROWS)
          val sr = RegFile[Int](3,3)
          Foreach(COLS+1 by 1){ i => 
            lb load data_dram((p+slice)%HEIGHT, i, 0::ROWS)
            Foreach(ROWS+1 by 1) {j => 
              Foreach(3 by 1 par 3) {k => sr(k,*) <<= lb(k,j%ROWS)}
              val temp = Reduce(Reg[Int](0))(3 by 1, 3 by 1){(r,c) => sr(r,c) * filter(slice+1,r,c)}{_+_}
              // For final version, make wr_value a Mux1H instead of a unique writer per val
              if (i == 0 || j == 0) {Pipe{}/*do nothing*/}
              else if (i == 1 || i == COLS || j == 1 || j == ROWS) {
                Pipe{
                  if (slice == 0) {local_slice(i-1, j-1) = sr(1,1)} // If on boundary of page, use meat only
                  else {local_slice(i-1, j-1) = 0} // If on boundary of page, ignore bread
                }
              }
              else if (slice == 0 && (p == 0 || p == HEIGHT-1)) {local_slice(i-1,j-1) = sr(1,1)} // First and last page, use meat only
              else if ((p == 0 || p == HEIGHT-1)) {local_slice(i-1,j-1) = 0} // First and last page, ignore bread
              else {local_slice(i-1, j-1) = temp} // Otherwise write convolution result
            }       
          }
          local_slice
        }{_+_}

        Foreach(COLS by 1, ROWS by 1){(i,j) => result_sram(p, i, j) = temp_slice(i,j)}

      }

      result_dram store result_sram


   	}

   	// Get results
   	val result_data = getTensor3(result_dram)
   	val raw_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil3d_gold.csv", "\n")
   	val gold = raw_gold.reshape(HEIGHT,COLS,ROWS)

   	// Printers
   	printTensor3(gold, "gold") // Least significant dimension is horizontal, second-least is vertical, third least is ---- separated blocks
   	printTensor3(result_data, "results")

   	val cksum = gold.zip(result_data){_==_}.reduce{_&&_}
   	println("PASS: " + cksum + " (Stencil3D)")

 }
}


object NW extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
  
  Needleman-Wunsch Genetic Alignment algorithm                                                  
  
    LETTER KEY:         Scores                   Ptrs                                                                                                  
      a = 0                   T  T  C  G                T  T  C  G                                                                                                                          
      c = 1                0 -1 -2 -3 -4 ...         0  ←  ←  ←  ← ...                                                                                                        
      g = 2             T -1  1  0 -1 -2          T  ↑  ↖  ←  ←  ←                                                                                                                          
      t = 3             C -2  0 -1  1  0          C  ↑  ↑  ↑  ↖  ←                                                                                                                         
      - = 4             G -3 -2 -2  0  2          G  ↑  ↑  ↑  ↑  ↖                                                                                                                                  
      _ = 5             A -4 -3 -3 -1  1          A  ↑  ↑  ↑  ↑  ↖                                                                                                                                 
                           .                         .                                                                                                                        
                           .                         .                       
                           .                         .                       
                                                                                                           
    PTR KEY:                                                                                                                                                                                                      
      ← = 0 = skipB
      ↑ = 1 = skipA
      ↖ = 2 = align                                                                                      
                                                                                                           
                                                                                                           

                                                                                                           
 */

  @struct case class nw_tuple(score: Int16, ptr: Int16)

  @virtualize
  def main() = {

    // FSM setup
    val traverseState = 0
    val padBothState = 1
    val doneState = 2

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 1
    val MISMATCH_SCORE = -1
    val GAP_SCORE = -1 
    val seqa_string = "tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc".toText
    val seqb_string = "ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat".toText
    val length = 128

    val seqa_bin = Array.tabulate[Int](seqa_string.length){i => 
      val char = seqa_string(i)
      if (char == "a") {0.to[Int]}
      else if (char == "c") {1.to[Int]}
      else if (char == "g") {2.to[Int]}
      else if (char == "t") {3.to[Int]}
      else {6.to[Int]}
    } // TODO: Support c++ types with 2 bits in dram
    val seqb_bin = Array.tabulate[Int](seqb_string.length){i => 
      val char = seqb_string(i)
      if (char == "a") {0.to[Int]}
      else if (char == "c") {1.to[Int]}
      else if (char == "g") {2.to[Int]}
      else if (char == "t") {3.to[Int]}
      else {6.to[Int]}
    } // TODO: Support c++ types with 2 bits in dram

    val seqa_dram_raw = DRAM[Int](length)
    val seqb_dram_raw = DRAM[Int](length)
    val seqa_dram_aligned = DRAM[Int](length*2)
    val seqb_dram_aligned = DRAM[Int](length*2)
    setMem(seqa_dram_raw, seqa_bin)
    setMem(seqb_dram_raw, seqb_bin)

    Accel{
      val seqa_sram_raw = SRAM[Int](length)
      val seqb_sram_raw = SRAM[Int](length)
      val seqa_fifo_aligned = FIFO[Int](length*2)
      val seqb_fifo_aligned = FIFO[Int](length*2)

      seqa_sram_raw load seqa_dram_raw
      seqb_sram_raw load seqb_dram_raw

      val score_matrix = SRAM[nw_tuple](length+1,length+1)

      // Build score matrix
      Foreach(length+1 by 1){ r =>
        Foreach(length+1 by 1) { c => 
          val update = if (r == 0) (nw_tuple(-c.as[Int16], 0)) else if (c == 0) (nw_tuple(-r.as[Int16], 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = score_matrix(r, c-1).score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, nw_tuple(from_left, SKIPB), mux(from_top >= from_diag, nw_tuple(from_top,SKIPA), nw_tuple(from_diag, ALIGN)))
          }
          score_matrix(r,c) = update
        }
      }

      // Read score matrix
      val b_addr = Reg[Int](length)
      val a_addr = Reg[Int](length)
      val done_backtrack = Reg[Bool](false)
      FSM[Int](state => state != doneState) { state =>
        if (state == traverseState) {
          if (score_matrix(b_addr,a_addr).ptr == ALIGN.to[Int16]) {
            done_backtrack := b_addr == 1.to[Int] || a_addr == 1.to[Int]
            b_addr :-= 1
            a_addr :-= 1
            seqa_fifo_aligned.enq(seqa_sram_raw(a_addr-1), !done_backtrack)
            seqb_fifo_aligned.enq(seqb_sram_raw(b_addr-1), !done_backtrack)
          } else if (score_matrix(b_addr,a_addr).ptr == SKIPA.to[Int16]) {
            done_backtrack := b_addr == 1.to[Int]
            b_addr :-= 1
            seqb_fifo_aligned.enq(seqb_sram_raw(b_addr-1), !done_backtrack)  
            seqa_fifo_aligned.enq(4, !done_backtrack)          
          } else {
            done_backtrack := a_addr == 1.to[Int]
            a_addr :-= 1
            seqa_fifo_aligned.enq(seqa_sram_raw(a_addr-1), !done_backtrack)
            seqb_fifo_aligned.enq(4, !done_backtrack)          
          }
        } else if (state == padBothState) {
          seqa_fifo_aligned.enq(5, !seqa_fifo_aligned.full) // I think this FSM body either needs to be wrapped in a body or last enq needs to be masked or else we are full before FSM sees full
          seqb_fifo_aligned.enq(5, !seqb_fifo_aligned.full)
        } else {}
      } { state => 
        mux(state == traverseState && ((b_addr == 0.to[Int]) || (a_addr == 0.to[Int])), padBothState, 
          mux(seqa_fifo_aligned.full || seqb_fifo_aligned.full, doneState, state))// Safe to assume they fill at same time?
      }

      Parallel{
        seqa_dram_aligned store seqa_fifo_aligned
        seqb_dram_aligned store seqb_fifo_aligned
      }

    }

    val seqa_aligned_result = getMem(seqa_dram_aligned)
    val seqb_aligned_result = getMem(seqb_dram_aligned)

    val seqa_gold_string = "cggccgcttag-tgggtgcggtgctaagggggctagagggcttg-tc-gcggggcacgggacatgcg--gcg-t--cgtaaaccaaacat-g-gcgccgggag-attatgctcttgcacg-acag-ta----g-gat-aaagc---agc-t_________________________________________________________________________________________________________".toText
    val seqb_gold_string = "--------tagct-ggtaccgt-ctaa-gtggc--ccggg-ttgagcggctgggca--gg-c-tg-gaag-gttagcgt-aaggagatatagtccg-cgggtgcagggtg-gctggcccgtacagctacctggcgctgtgcgcgggagctt_________________________________________________________________________________________________________".toText

    val seqa_gold_bin = Array.tabulate[Int](seqa_gold_string.length){i => 
      val char = seqa_gold_string(i)
      if (char == "a") {0.to[Int]}
      else if (char == "c") {1.to[Int]}
      else if (char == "g") {2.to[Int]}
      else if (char == "t") {3.to[Int]}
      else if (char == "-") {4.to[Int]}
      else if (char == "_") {5.to[Int]}
      else {6.to[Int]}
    }
    val seqb_gold_bin = Array.tabulate[Int](seqb_gold_string.length){i => 
      val char = seqb_gold_string(i)
      if (char == "a") {0.to[Int]}
      else if (char == "c") {1.to[Int]}
      else if (char == "g") {2.to[Int]}
      else if (char == "t") {3.to[Int]}
      else if (char == "-") {4.to[Int]}
      else if (char == "_") {5.to[Int]}
      else {6.to[Int]}
    }

    printArray(seqa_aligned_result, "Aligned result A: ")
    printArray(seqa_gold_bin, "Gold A: ")
    printArray(seqb_aligned_result, "Aligned result B: ")
    printArray(seqb_gold_bin, "Gold B: ")

    val cksumA = seqa_aligned_result.zip(seqa_gold_bin){_==_}.reduce{_&&_}
    val cksumB = seqb_aligned_result.zip(seqb_gold_bin){_==_}.reduce{_&&_}
    val cksum = cksumA && cksumB
    println("PASS: " + cksum + " (NW) * Implement nodes for text operations in Scala once refactoring is done")



  }
}      


object MD_KNN extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
  
  Molecular Dynamics via K-nearest neighbors                                                                

                  ← N_NEIGHBORS →   
                 ___________________   
                |                   |  
            ↑   |                   |  
                |                   |  
                |                   |  
                |                   |  
      N_ATOMS   |                   |  
                |                   |  
                |                   |  
                |                   |  
            ↓   |                   |  
                |                   |  
                 ```````````````````   

                 For each atom (row), get id of its interactions (col), index into idx/y/z, compute potential energy, and sum them all up

                                                                                                           
 */

  // Max pos seems to be about 19
  type T = FixPt[TRUE, _12, _52]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val N_NEIGHBORS = 16 
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]
    val raw_xpos = loadCSV1D[T]("/remote/regression/data/machsuite/knn_x.csv", "\n")
    val raw_ypos = loadCSV1D[T]("/remote/regression/data/machsuite/knn_y.csv", "\n")
    val raw_zpos = loadCSV1D[T]("/remote/regression/data/machsuite/knn_z.csv", "\n")
    val raw_interactions_data = loadCSV1D[Int]("/remote/regression/data/machsuite/knn_interactions.csv", "\n")
    val raw_interactions = raw_interactions_data.reshape(N_ATOMS, N_NEIGHBORS)

    val xpos_dram = DRAM[T](N_ATOMS)
    val ypos_dram = DRAM[T](N_ATOMS)
    val zpos_dram = DRAM[T](N_ATOMS)
    val xforce_dram = DRAM[T](N_ATOMS)
    val yforce_dram = DRAM[T](N_ATOMS)
    val zforce_dram = DRAM[T](N_ATOMS)
    val interactions_dram = DRAM[Int](N_ATOMS, N_NEIGHBORS)

    setMem(xpos_dram, raw_xpos)
    setMem(ypos_dram, raw_ypos)
    setMem(zpos_dram, raw_zpos)
    setMem(interactions_dram, raw_interactions)

    Accel{
      val xpos_sram = SRAM[T](N_ATOMS)
      val ypos_sram = SRAM[T](N_ATOMS)
      val zpos_sram = SRAM[T](N_ATOMS)
      val xforce_sram = SRAM[T](N_ATOMS)
      val yforce_sram = SRAM[T](N_ATOMS)
      val zforce_sram = SRAM[T](N_ATOMS)
      val interactions_sram = SRAM[Int](N_ATOMS, N_NEIGHBORS) // Can also shrink sram and work row by row

      xpos_sram load xpos_dram
      ypos_sram load ypos_dram
      zpos_sram load zpos_dram
      interactions_sram load interactions_dram

      Foreach(N_ATOMS by 1) { atom =>
        val this_pos = XYZ(xpos_sram(atom), ypos_sram(atom), zpos_sram(atom))
        val total_force = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
        // total_force.reset // Probably unnecessary
        Reduce(total_force)(N_NEIGHBORS by 1) { neighbor => 
          val that_id = interactions_sram(atom, neighbor)
          val that_pos = XYZ(xpos_sram(that_id), ypos_sram(that_id), zpos_sram(that_id))
          val delta = XYZ(this_pos.x - that_pos.x, this_pos.y - that_pos.y, this_pos.z - that_pos.z)
          val r2inv = 1.0.to[T]/( delta.x*delta.x + delta.y*delta.y + delta.z*delta.z );
          // Assume no cutoff and aways account for all nodes in area
          val r6inv = r2inv * r2inv * r2inv;
          val potential = r6inv*(lj1*r6inv - lj2);
          val force = r2inv*potential;
          XYZ(delta.x*force, delta.y*force, delta.z*force)
        }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
        xforce_sram(atom) = total_force.x
        yforce_sram(atom) = total_force.y
        zforce_sram(atom) = total_force.z
      }
      xforce_dram store xforce_sram
      yforce_dram store yforce_sram
      zforce_dram store zforce_sram
    }

    val xforce_received = getMem(xforce_dram)
    val yforce_received = getMem(yforce_dram)
    val zforce_received = getMem(zforce_dram)
    val xforce_gold = loadCSV1D[T]("/remote/regression/data/machsuite/knn_x_gold.csv", "\n")
    val yforce_gold = loadCSV1D[T]("/remote/regression/data/machsuite/knn_y_gold.csv", "\n")
    val zforce_gold = loadCSV1D[T]("/remote/regression/data/machsuite/knn_z_gold.csv", "\n")

    printArray(xforce_gold, "Gold x:")
    printArray(xforce_received, "Received x:")
    printArray(yforce_gold, "Gold y:")
    printArray(yforce_received, "Received y:")
    printArray(zforce_gold, "Gold z:")
    printArray(zforce_received, "Received z:")

    val margin = 0.001.to[T]
    val cksumx = xforce_gold.zip(xforce_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumy = yforce_gold.zip(yforce_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumz = zforce_gold.zip(zforce_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_KNN)")
  }
}      

object MD_Grid extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
  
  Moleckaler Dynamics via the grid, a digital frontier
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
                            ←      BLOCK_SIDE     →                        
                ↗                                                                                                
                          __________________________________   
       BLOCK_SIDE        /                                  /|  
                        /                                  / |  
        ↙              /                                  /  |  
                      /_________________________________ /   |  
                     |           b1                     |    |  
           ↑         |        ..  ..  ..                |    |  
                     |       - - - - - -                |    |  
                     |      :``::``::``:                |    |  
           B         |    b1:..::__::..:b1              |    |  
           L         |      - - /_/| - -                |    |  
           O         |     :``:|b0||:``:                |    |  
           C         |   b1:..:|__|/:..:b1              |    |  
           K         |      - - - -  - -                |    |  
           |         |     :``::``: :``:                |    |  
           S         |   b1:..::..: :..:b1              |    |  
           I         |          b1                      |    |  
           D         |                                  |   /   
           E         |                                  |  /   
                     |                                  | /    
           ↓         |                                  |/     
                      ``````````````````````````````````       * Each b0 contains up to "density" number of atoms
                                                               * For each b0, and then for each atom in b0, compute this atom's
                                                                    interactions with all atoms in the adjacent (27) b1's
                                                               * One of the b1's will actually be b0, so skip this contribution                      
                                                                                                           
 */

  // Max pos seems to be about 19
  type T = FixPt[TRUE, _12, _52]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val DOMAIN_EDGE = 20
    val BLOCK_SIDE = 4
    val density = 10
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]

    val raw_npoints = Array[Int](4,4,3,4,5,5,2,1,1,8,4,8,3,3,7,5,4,5,6,2,2,4,4,3,3,4,7,2,3,2,
                                 2,1,7,1,3,7,6,3,3,4,3,4,5,5,6,4,2,5,7,6,5,4,3,3,5,4,4,4,3,2,3,2,7,5)
    val npoints_data = raw_npoints.reshape(BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    val raw_dvec = loadCSV1D[T]("/remote/regression/data/machsuite/grid_dvec.csv", "\n")
    // Strip x,y,z vectors from raw_dvec
    val dvec_x_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val dvec_y_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val dvec_z_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}

    val dvec_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val npoints_dram = DRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    setMem(dvec_x_dram, dvec_x_data)
    setMem(dvec_y_dram, dvec_y_data)
    setMem(dvec_z_dram, dvec_z_data)
    setMem(npoints_dram, npoints_data)

    Accel{
      val dvec_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val npoints_sram = SRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)
      val force_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val force_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val force_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)

      dvec_x_sram load dvec_x_dram
      dvec_y_sram load dvec_y_dram
      dvec_z_sram load dvec_z_dram
      npoints_sram load npoints_dram

      // Iterate over each block
      Foreach(BLOCK_SIDE by 1, BLOCK_SIDE by 1, BLOCK_SIDE by 1) { (b0x, b0y, b0z) => 
        // Iterate over each point in this block, considering boundaries
        val b0_cube_forces = SRAM[XYZ](density)
        val b1x_start = max(0.to[Int],b0x-1.to[Int])
        val b1x_end = min(BLOCK_SIDE.to[Int], b0x+2.to[Int])
        val b1y_start = max(0.to[Int],b0y-1.to[Int])
        val b1y_end = min(BLOCK_SIDE.to[Int], b0y+2.to[Int])
        val b1z_start = max(0.to[Int],b0z-1.to[Int])
        val b1z_end = min(BLOCK_SIDE.to[Int], b0z+2.to[Int])
        MemReduce(b0_cube_forces)(b1x_start until b1x_end by 1, b1y_start until b1y_end by 1, b1z_start until b1z_end by 1) { (b1x, b1y, b1z) => 
          val b1_cube_contributions = SRAM[XYZ](density)
          // Iterate over points in b0
          val p_range = npoints_sram(b0x, b0y, b0z)
          val q_range = npoints_sram(b1x, b1y, b1z)
          Foreach(0 until p_range) { p_idx =>
            val px = dvec_x_sram(b0x, b0y, b0z, p_idx)
            val py = dvec_y_sram(b0x, b0y, b0z, p_idx)
            val pz = dvec_z_sram(b0x, b0y, b0z, p_idx)
            val q_sum = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
            Reduce(q_sum)(0 until q_range) { q_idx => 
              val qx = dvec_x_sram(b1x, b1y, b1z, q_idx)
              val qy = dvec_y_sram(b1x, b1y, b1z, q_idx)
              val qz = dvec_z_sram(b1x, b1y, b1z, q_idx)
              if ( !(b0x == b1x && b0y == b1y && b0z == b1z && p_idx == q_idx) ) { // Skip self
                val delta = XYZ(px - qx, py - qy, pz - qz)
                val r2inv = 1.0.to[T]/( delta.x*delta.x + delta.y*delta.y + delta.z*delta.z );
                // Assume no cutoff and aways account for all nodes in area
                val r6inv = r2inv * r2inv * r2inv;
                val potential = r6inv*(lj1*r6inv - lj2);
                val force = r2inv*potential;
                XYZ(delta.x*force, delta.y*force, delta.z*force)
              } else {
                XYZ(0.to[T], 0.to[T], 0.to[T])
              }
            }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
            b1_cube_contributions(p_idx) = q_sum
          }
          Foreach(p_range until density) { i => b1_cube_contributions(i) = XYZ(0.to[T], 0.to[T], 0.to[T]) } // Zero out untouched interactions          
          b1_cube_contributions
        }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}

        Foreach(0 until density) { i => 
          force_x_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).x
          force_y_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).y
          force_z_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).z
        }
      }
      force_x_dram store force_x_sram
      force_y_dram store force_y_sram
      force_z_dram store force_z_sram

    }

    val force_x_received = getTensor4(force_x_dram)
    val force_y_received = getTensor4(force_y_dram)
    val force_z_received = getTensor4(force_z_dram)
    val raw_force_gold = loadCSV1D[T]("/remote/regression/data/machsuite/grid_gold.csv", "\n")
    val force_x_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val force_y_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val force_z_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}


    printTensor4(force_x_gold, "Gold x:")
    printTensor4(force_x_received, "Received x:")
    printTensor4(force_y_gold, "Gold y:")
    printTensor4(force_y_received, "Received y:")
    printTensor4(force_z_gold, "Gold z:")
    printTensor4(force_z_received, "Received z:")

    val margin = 0.001.to[T]
    val cksumx = force_x_gold.zip(force_x_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumy = force_y_gold.zip(force_y_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumz = force_z_gold.zip(force_z_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_Grid)")
  }
}      

object KMP extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
  
  Knuth-Morris-Pratt

  Used https://www.browserling.com/tools/text-to-hex to convert string to hex, and then converted hex to dec                                                               
                                                                                             
                                                                                                           
 */

  @virtualize
  def main() = {

    val STRING_SIZE = 32411
    val PATTERN_SIZE = 4

    val raw_string = loadCSV1D[Int]("/remote/regression/data/machsuite/kmp_data.csv", "\n")
    val raw_pattern = Array[Int](98,117,108,108)
    val string_dram = DRAM[Int](STRING_SIZE)
    val pattern_dram = DRAM[Int](PATTERN_SIZE)
    val nmatches = ArgOut[Int]

    setMem(string_dram, raw_string)
    setMem(pattern_dram, raw_pattern)

    Accel{
      val string_sram = SRAM[Int](STRING_SIZE)
      val pattern_sram = SRAM[Int](PATTERN_SIZE)
      val kmp_next = SRAM[Int](PATTERN_SIZE)
      val num_matches = Reg[Int](0)

      string_sram load string_dram
      pattern_sram load pattern_dram

      // Init kmp_next
      val k = Reg[Int](0)
      kmp_next(0) = 0
      Sequential.Foreach(1 until PATTERN_SIZE by 1) { q => 
        // val whileCond = Reg[Bool](false)
        FSM[Int](state => state != 1) { state => 
          // whileCond := (k > 0) && (pattern_sram(k) != pattern_sram(q))
          if ((k > 0) && (pattern_sram(k) != pattern_sram(q))) k := 0 // TODO: Will it always bump back to 0 in this step or should it really be kmp_next(q)?
        }{state => mux((k > 0) && (pattern_sram(k) != pattern_sram(q)), 0, 1)}
        if (pattern_sram(k) == pattern_sram(q)) {k :+= 1.to[Int]}
        kmp_next(q) = k
      }

      // Scan string
      val q = Reg[Int](0)
      Sequential.Foreach(0 until STRING_SIZE) { i => 
        // val whileCond = Reg[Bool](false) 
        FSM[Int](state => state != 1) { state => 
          // whileCond := (q > 0) && (pattern_sram(i) != pattern_sram(q))
          if ((q > 0) && (string_sram(i) != pattern_sram(q))) q := kmp_next(q)
        }{state => mux((q > 0) && (string_sram(i) != pattern_sram(q)), 0, 1)}
        if (pattern_sram(q) == string_sram(i)) { q :+= 1 }
        if (q >= PATTERN_SIZE) {
          Pipe{
            num_matches :+= 1
            val bump = kmp_next(q - 1)
            q := bump
          }
        }
      }

      nmatches := num_matches
    }

    val gold_nmatches = 12
    val computed_nmatches = getArg(nmatches)

    println("Expected " + gold_nmatches + " matches")
    println("Found " + computed_nmatches)

    val cksum = gold_nmatches == computed_nmatches
    println("PASS: " + cksum + " (KMP) * Implement string find, string file parser, and string <-> hex <-> dec features once argon refactor is done so we can test any strings")
  }
}      


object GEMM_NCubed extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
                                                             
                                                                                                           
 */
  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() = {

    val dim = 64

    val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)

    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
  }
}      

object GEMM_Blocked extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*
                                                             
    CONCERNS: We need to figure out how HLS is actually managing the srams, or make our management better  
              We cannot do unaligned stores yet, so tilesize of 8 won't work unless we keep ts 16 of c_sram onchip                                                                                          
 */
  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() = {

    val dim = 64
    val tileSize = 8

    val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)
    val c_init = (0::dim, 0::dim){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{
      val a_sram = SRAM[T](tileSize)
      val b_sram = SRAM[T](tileSize,tileSize)
      val c_sram = SRAM[T](dim,dim) // No tiling along rows dim in machsuite??
      c_sram load c_dram

      Foreach(dim by tileSize) { jj => 
        Foreach(dim by tileSize) { kk =>
          b_sram load b_dram(kk::kk+tileSize, jj::jj+tileSize)
          Foreach(dim by 1) { i => 
            a_sram load a_dram(i, kk::kk+tileSize)
            Foreach(tileSize by 1) { k => 
              val temp_a = a_sram(k)
              Foreach(tileSize by 1) { j => 
                c_sram(i,j+jj) = c_sram(i,j+jj) + b_sram(k, j) * temp_a
              }
            }
          } 
        }
      }
      c_dram store c_sram
    }

    val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}

object Sort_Merge extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*                                                                                                  
                              |     |                                                                                                                                                                                        
                     |        |     |                                      |     |                                                                                                                                                                                     
                     |  |     |     |  |                             |     |     |                                                                                                                                                                           
                     |  |  |  |     |  |                          |  |     |     |     |                                                                                                                                                                     
                     |  |  |  |  |  |  |                          |  |  |  |     |     |                                                                                                                                                                     
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |     |                                                                                                                                                                      
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |  |  |                                                                                                                                                                        
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |  |  |                                                                                                                                                                      
                                                                  |  |  |  |  |  |  |  |                      
   Outer FSM iter 1:  ↖↗    ↖↗    ↖↗    ↖↗      Outer FSM iter 2:  ↖.....↖     ↖.....↖                                                                                                                      
                     fifos numel = 1                               fifos numel = 2                                                            
                                                                  
                                                                                                     
                            |           |                                           |  |                                                                                                   
                         |  |           |                                        |  |  |                                                                                                   
                      |  |  |        |  |                                  |  |  |  |  |                                                                                                  
                   |  |  |  |        |  |                               |  |  |  |  |  |                                                                                                  
                   |  |  |  |     |  |  |                            |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
 Outer FSM iter 3:  ↖...........↖               Outer FSM iter 4:                                                                                                     
                   fifos numel = 4                                 Done
                                                                                                                                                                                                                   
                                                                                                                                                                           
 */

  @virtualize
  def main() = {

    val numel = 2048
    val START = 0
    val STOP = numel
    val levels = STOP-START //ArgIn[Int]
    // setArg(levels, args(0).to[Int])

    val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_data.csv", "\n")

    val data_dram = DRAM[Int](numel)
    // val sorted_dram = DRAM[Int](numel)

    setMem(data_dram, raw_data)

    Accel{
      val data_sram = SRAM[Int](numel)
      val lower_fifo = FIFO[Int](numel/2)
      val upper_fifo = FIFO[Int](numel/2)

      data_sram load data_dram


      FSM[Int,Int](1){m => m < levels} { m =>
        FSM[Int,Int](START)(i => i < STOP) { i =>
          val from = i
          val mid = i+m-1
          val to = min(i+m+m-1, STOP.to[Int])
          val lower_tmp = Reg[Int](0)
          val upper_tmp = Reg[Int](0)
          Foreach(from until mid+1 by 1){ i => if (i == from) {lower_tmp := data_sram(i)} else {lower_fifo.enq(data_sram(i))} }
          Foreach(mid+1 until to+1 by 1){ j => if (j == mid+1) {upper_tmp := data_sram(j)} else {upper_fifo.enq(data_sram(j))} }
          Sequential.Foreach(from until to+1 by 1) { k => 
            if (lower_tmp < upper_tmp) {
              data_sram(k) = lower_tmp
              val next_lower = if (lower_fifo.empty) {0x7FFFFFFF.to[Int]} else {lower_fifo.deq()}
              lower_tmp := next_lower
            } else {
              data_sram(k) = upper_tmp
              val next_upper = if (upper_fifo.empty) {0x7FFFFFFF.to[Int]} else {upper_fifo.deq()}
              upper_tmp := next_upper
            }
          }
        }{ i => i + m + m }
      }{ m => m + m}

      // sorted_dram store data_sram
      data_dram store data_sram
    }

    val sorted_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_gold.csv", "\n")
    val sorted_result = getMem(data_dram)

    printArray(sorted_gold, "Sorted Gold: ")
    printArray(sorted_result, "Sorted Result: ")

    val cksum = sorted_gold.zip(sorted_result){_==_}.reduce{_&&_}
    // // Use the real way to check if list is sorted instead of using machsuite gold
    // // This way says I've done goofed, issue #
    // val cksum = Array.tabulate(STOP-1){ i => pack(sorted_result(i), sorted_result(i+1)) }.map{a => a._1 <= a._2}.reduce{_&&_}
    println("PASS: " + cksum + " (Sort_Merge)")
  }
}

object Sort_Radix extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*                                                                                                  
    TODO: Cartoon of what this is doing                                                         
                                                                                                                                                                                                                       
                                                                                                                                                                           
 */

  @virtualize
  def main() = {

    val numel = 2048
    val NUM_BLOCKS = 512
    val EL_PER_BLOCK = 4
    val RADIX = 4
    val BUCKET_SIZE = NUM_BLOCKS*RADIX
    val SCAN_BLOCK = 16
    val SCAN_RADIX = BUCKET_SIZE/SCAN_BLOCK
    val a = false.to[Bool]
    val b = true.to[Bool]

    val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_data.csv", "\n")

    val data_dram = DRAM[Int](numel)

    setMem(data_dram, raw_data)

    Accel{
      val a_sram = SRAM[Int](numel)
      val b_sram = SRAM[Int](numel)
      val bucket_sram = SRAM[Int](BUCKET_SIZE)
      val sum_sram = SRAM[Int](SCAN_RADIX)
      val valid_buffer = Reg[Bool](false)
      
      a_sram load data_dram

      def hist(exp: Index, s: SRAM1[Int]): Unit = {
        Foreach(NUM_BLOCKS by 1) { blockID => 
          Sequential.Foreach(4 by 1) {i => 
            val a_indx = blockID * EL_PER_BLOCK + i
            val shifted = Reg[Int](0)
            shifted := s(a_indx) // TODO: Allow just s(a_indx) >> exp syntax
            // Reduce(shifted)(exp by 1) { k => shifted >> 1}{(a,b) => b}
            Foreach(exp by 1) { k => shifted := shifted.value >> 1}
            val bucket_indx = (shifted.value & 0x3.to[Int])*NUM_BLOCKS.to[Int] + blockID + 1.to[Int]
            bucket_sram(bucket_indx) = bucket_sram(bucket_indx) + 1
          }
        }
      }

      def local_scan(): Unit = {
        Foreach(SCAN_RADIX by 1) { radixID => 
          Sequential.Foreach(1 until SCAN_BLOCK by 1) { i => // Loop carry dependency
            val bucket_indx = radixID*SCAN_BLOCK.to[Int] + i
            val prev_val = Reg[Int](0)
            Pipe{ prev_val := bucket_sram(bucket_indx - 1) }
            Pipe{ bucket_sram(bucket_indx) = bucket_sram(bucket_indx) + prev_val }
          }
        }
      }

      def sum_scan(): Unit = {
        sum_sram(0) = 0
        Foreach(1 until SCAN_RADIX by 1) { radixID => 
          val bucket_indx = radixID*SCAN_BLOCK - 1
          sum_sram(radixID) = sum_sram(radixID-1) + bucket_sram(bucket_indx)
        }
      }

      def last_step_scan(): Unit = {
        Foreach(SCAN_RADIX by 1) { radixID => 
          Foreach(SCAN_BLOCK by 1) { i => 
            val bucket_indx = radixID * SCAN_BLOCK + i
            bucket_sram(bucket_indx) = bucket_sram(bucket_indx) + sum_sram(radixID)
          }
        }
      }

      def update(exp: Index, s1: SRAM1[Int], s2: SRAM1[Int]): Unit = {

        Foreach(NUM_BLOCKS by 1) { blockID => 
          Sequential.Foreach(4 by 1) { i => 
            val shifted = Reg[Int](0)
            shifted := s1(blockID*EL_PER_BLOCK + i) // TODO: Allow just s(a_indx) >> exp syntax
            // Reduce(shifted)(exp by 1) { k => shifted >> 1}{(a,b) => b}
            Foreach(exp by 1) { k => shifted := shifted >> 1}
            val bucket_indx = (shifted & 0x3.to[Int])*NUM_BLOCKS + blockID
            val a_indx = blockID * EL_PER_BLOCK + i
            s2(bucket_sram(bucket_indx)) = s1(a_indx)
            bucket_sram(bucket_indx) = bucket_sram(bucket_indx) + 1
          }
        }
      }


      Sequential.Foreach(32 by 2) { exp => 
        Foreach(BUCKET_SIZE by 1) { i => bucket_sram(i) = 0 }
  
        if (valid_buffer == a) {
          Pipe{hist(exp, a_sram)}
        } else {
          Pipe{hist(exp, b_sram)}
        }

        local_scan()
        sum_scan()
        last_step_scan()

        if (valid_buffer == a) {
          Pipe{
            update(exp, a_sram, b_sram)
            valid_buffer := b
          }
        } else {
          Pipe{
            update(exp, b_sram, a_sram)
            valid_buffer := a
          }
        }

      }

      if (valid_buffer == a) {
        data_dram store a_sram
      } else {
        data_dram store b_sram
      }

    }

    val sorted_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_gold.csv", "\n")
    val sorted_result = getMem(data_dram)

    printArray(sorted_gold, "Sorted Gold: ")
    printArray(sorted_result, "Sorted Result: ")

    val cksum = sorted_gold.zip(sorted_result){_==_}.reduce{_&&_}
    // // Use the real way to check if list is sorted instead of using machsuite gold
    // // This way says I've done goofed, issue #
    // val cksum = Array.tabulate(STOP-1){ i => pack(sorted_result(i), sorted_result(i+1)) }.map{a => a._1 <= a._2}.reduce{_&&_}
    println("PASS: " + cksum + " (Sort_Radix)")
  }
}


object SPMV_CRS extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*                                                                                                  
   Sparse Matrix is the IEEE 494 bus interconnect matrix from UF Sparse Datasets   

    Datastructures in CRS:
              0__1__2__3__4__5_
      rowid: |__|__|__|__|__|__|
               |  |____   |_____________                       
              _↓_______`↓_______________`↓_______
      cols:  |___________________________________|
              _↓________↓________________↓_______
     values: |___________________________________|    

        Use cols to gather from vector:
              ____________________________________________
     vector: |____________________________________________|


   Concerns:
      Machsuite assumes everything fits on chip?  So there are no gathers...  Setting tilesize to problem size for now
 */

  type T = FixPt[TRUE,_16,_16]
  @virtualize
  def main() = {

    val NNZ = 1666
    val N = 494
    val tileSize = 494

    val raw_values = loadCSV1D[T]("/remote/regression/data/machsuite/crs_values.csv", "\n")
    val raw_cols = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_cols.csv", "\n")
    val raw_rowid = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_rowid.csv", "\n")
    val raw_vec = loadCSV1D[T]("/remote/regression/data/machsuite/crs_vec.csv", "\n")

    val values_dram = DRAM[T](NNZ) 
    val cols_dram = DRAM[Int](NNZ) 
    val rowid_dram = DRAM[Int](N+1) 
    val vec_dram = DRAM[T](N) 
    val result_dram = DRAM[T](N)

    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(rowid_dram, raw_rowid)
    setMem(vec_dram, raw_vec)

    Accel {
      val rowid_sram = SRAM[Int](tileSize+1)
      val cols_sram = SRAM[Int](tileSize)
      val values_sram = SRAM[T](tileSize)
      val vec_sram = SRAM[T](tileSize)
      val result_sram = SRAM[T](tileSize)

      Foreach(N/tileSize by 1) { tile =>
        rowid_sram load rowid_dram(tile*(tileSize+1) :: (tile+1)*(tileSize+1))
        Foreach(tileSize by 1) { i => 
          val start_id = rowid_sram(i)
          val stop_id = rowid_sram(i+1)
          Parallel{
            cols_sram load cols_dram(start_id :: stop_id)
            values_sram load values_dram(start_id :: stop_id)
          }
          vec_sram gather vec_dram(cols_sram, stop_id - start_id)
          val element = Reduce(Reg[T](0))(stop_id - start_id by 1) { j => 
            values_sram(j) * vec_sram(j)
          }{_+_}
          result_sram(i) = element
        }
        result_dram(tile*tileSize :: (tile+1)*tileSize) store result_sram
      }
    }

    val data_gold = loadCSV1D[T]("/remote/regression/data/machsuite/crs_gold.csv", "\n")
    val data_result = getMem(result_dram)

    printArray(data_gold, "Gold: ")
    printArray(data_result, "Result: ")

    val margin = 0.2.to[T] // Scala does not stay in bounds as tightly as chisel
    val cksum = data_gold.zip(data_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}

    println("PASS: " + cksum + " (SPMV_CRS) * Fix gather on arbitrary width elements (64 for better prec here), issue #126")


  }
}

object SPMV_ELL extends SpatialApp { // Regression (Dense) // Args: none
 import IR._

 /*                                                                                                  
   Sparse Matrix is the IEEE 494 bus interconnect matrix from UF Sparse Datasets   

    Datastructures in Ellpack:

                                                                                                                                                 
                                             ←   L    →
        _____________________                __________         _________                                                                              
       | 9  0  2  0  0  0  1 |          ↑   | 0  2  6  |       | 9  2  1 |                                                                             
       |                     |              |          |       |         |                                                                             
       | 3  0  0  0  0  2  0 | ===>     N   | 0  5  *  |       | 3  2  * |                                                                             
       |                     |              |          |       |         |                                                                             
       | 0  0  0  0  5  0  0 |          ↓   | 4  *  *  |       | 5  *  * |                                                                             
        `````````````````````                ``````````         `````````                                                                              
         uncompressed                        cols               data                                                                                             
                                                                                       
 */

  type T = FixPt[TRUE,_16,_16]
  @virtualize
  def main() = {

    val NNZ = 1666
    val N = 494
    val L = 10    
    val tileSize = N

    val raw_values = loadCSV1D[T]("/remote/regression/data/machsuite/ell_values.csv", "\n").reshape(N,L)
    val raw_cols = loadCSV1D[Int]("/remote/regression/data/machsuite/ell_cols.csv", "\n").reshape(N,L)
    val raw_vec = loadCSV1D[T]("/remote/regression/data/machsuite/ell_vec.csv", "\n")

    val values_dram = DRAM[T](N,L) 
    val cols_dram = DRAM[Int](N,L) 
    val vec_dram = DRAM[T](N) 
    val result_dram = DRAM[T](N)

    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(vec_dram, raw_vec)

    Accel {
      Foreach(N/tileSize by 1){ tile => 
        val cols_sram = SRAM[Int](tileSize, L)
        val values_sram = SRAM[T](tileSize, L)
        val result_sram = SRAM[T](tileSize)

        cols_sram load cols_dram(tile::tile+tileSize, 0::L)
        values_sram load values_dram(tile::tile+tileSize, 0::L)

        Foreach(tileSize by 1) { i => 
          val vec_sram = SRAM[T](L)
          val gather_addrs = SRAM[Int](L)
          Foreach(L by 1) { j => gather_addrs(j) = cols_sram(i, j) }
          vec_sram gather vec_dram(gather_addrs, L)
          val element = Reduce(Reg[T](0))(L by 1) { k => values_sram(i,k) * vec_sram(k) }{_+_}
          result_sram(i) = element
        }

        result_dram(tile::tile+tileSize) store result_sram

      }
    }

    val data_gold = loadCSV1D[T]("/remote/regression/data/machsuite/ell_gold.csv", "\n")
    val data_result = getMem(result_dram)

    printArray(data_gold, "Gold: ")
    printArray(data_result, "Result: ")
    printArray(data_result.zip(data_gold){_-_}, "delta")

    val margin = 0.2.to[T] // Scala does not stay in bounds as tightly as chisel
    val cksum = data_gold.zip(data_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}

    println("PASS: " + cksum + " (SPMV_ELL)")


  }
}

object Backprop extends SpatialApp { // DISABLED Regression (Dense) // Args: none
 import IR._

 /*                                                                                                  

    Concerns: 
        It seems like they tried to make their implementation suck.......  What they do in 15 lines and 2 function calls and 3 loops, I can do in 3 lines and 2 loops                                                                   
 */

  type T = FixPt[TRUE,_32,_32]
  @virtualize
  def main() = {

    val input_dimension =  13
    val possible_outputs =  3
    val training_sets =   163
    val nodes_per_layer =  64
    val layers =            2
    val learning_rate =  0.01
    val epochs =            1
    val test_sets =        15
    val norm_param =    0.005

    val weights1_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_weights1.csv").reshape(nodes_per_layer, input_dimension)
    val weights2_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_weights2.csv").reshape(nodes_per_layer, nodes_per_layer)
    val weights3_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_weights3.csv").reshape(possible_outputs, nodes_per_layer)
    val biases1_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_biases1.csv")
    val biases2_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_biases2.csv")
    val biases3_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_biases3.csv")
    val training_data_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_training_data.csv").reshape(training_sets, input_dimension)
    val training_targets_data = loadCSV1D[T]("/remote/regression/data/machsuite/backprop_training_targets.csv").reshape(training_sets, possible_outputs)

    val weights1_dram = DRAM[T](nodes_per_layer,input_dimension)
    val weights2_dram = DRAM[T](nodes_per_layer,nodes_per_layer)
    val weights3_dram = DRAM[T](possible_outputs,nodes_per_layer)
    val biases1_dram = DRAM[T](nodes_per_layer)
    val biases2_dram = DRAM[T](nodes_per_layer)
    val biases3_dram = DRAM[T](possible_outputs)
    val training_data_dram = DRAM[T](training_sets,input_dimension)
    val training_targets_dram = DRAM[T](training_sets,possible_outputs)

    setMem(weights1_dram, weights1_data)
    setMem(weights2_dram, weights2_data)
    setMem(weights3_dram, weights3_data)
    setMem(biases1_dram, biases1_data)
    setMem(biases2_dram, biases2_data)
    setMem(biases3_dram, biases3_data)
    setMem(training_data_dram, training_data_data)
    setMem(training_targets_dram, training_targets_data)

    Accel{

      def RELU(x: T): T = {
        // Basic linear:
        mux(x < 0, 0, x)

        // Machsuite implementation:
        // 1.0/(1.0+exp(-x))
      }

      val biases1_sram = SRAM[T](nodes_per_layer)
      val biases2_sram = SRAM[T](nodes_per_layer)
      val biases3_sram = SRAM[T](nodes_per_layer)
      val weights1_sram = SRAM[T](nodes_per_layer, input_dimension)
      val weights2_sram = SRAM[T](nodes_per_layer, nodes_per_layer)
      val weights3_sram = SRAM[T](possible_outputs, nodes_per_layer)

      biases1_sram load biases1_dram
      biases2_sram load biases2_dram
      biases3_sram load biases3_dram
      weights1_sram load weights1_dram
      weights2_sram load weights2_dram
      weights3_sram load weights3_dram

      Foreach(training_sets by 1) { i => 
        val activations1 = SRAM[T](nodes_per_layer)
        val activations2 = SRAM[T](nodes_per_layer)
        val activations3 = SRAM[T](possible_outputs)
        val dactivations1 = SRAM[T](nodes_per_layer)
        val dactivations2 = SRAM[T](nodes_per_layer)
        val dactivations3 = SRAM[T](possible_outputs)
        val training_sram = SRAM[T](input_dimension)
        val training_targets = SRAM[T](possible_outputs)

        training_sram load training_data_dram(i, 0::input_dimension)
        training_targets load training_targets_dram(i, 0::possible_outputs)

        // Input Layer 
        Foreach(nodes_per_layer by 1){ j => 
          activations1(j) = Reduce(Reg[T](0))(input_dimension by 1) { i => weights1_sram(j, i) * training_sram(i)}{_+_} + biases1_sram(j)
        }
        // Relu
        Foreach(nodes_per_layer by 1) { i => 
          dactivations1(i) = activations1(i)*(1.0.to[T]-activations1(i))
          activations1(i) = RELU(activations1(i))
        }
        // Middle layer
        Foreach(nodes_per_layer by 1){ j => 
          activations2(j) = Reduce(Reg[T](0))(nodes_per_layer by 1) { i => weights2_sram(j, i) * training_sram(i)}{_+_} + biases2_sram(j)
        }
        // Relu
        Foreach(nodes_per_layer by 1) { i => 
          dactivations2(i) = activations2(i)*(1.0.to[T]-activations2(i))
          activations2(i) = RELU(activations2(i))
        }
        // Last layer
        Foreach(possible_outputs by 1){ j => 
          activations3(j) = Reduce(Reg[T](0))(nodes_per_layer by 1) { i => weights3_sram(j, i) * training_sram(i)}{_+_} + biases3_sram(j)
        }
        // Relu
        Foreach(possible_outputs by 1) { i => 
          dactivations3(i) = activations3(i)*(1.0.to[T]-activations3(i))
          activations3(i) = RELU(activations3(i))
        }


      }

    }

  }
}
