import spatial.dsl._
import virtualized._

object Indigo extends SpatialApp {
	override val target = targets.Plasticine
	import target._



	@virtualize
	def predict(X: Float) = {

		val project_dir = s"${sys.env("SPATIAL_HOME")}/apps/src/indigo/INDIGO_LUTs/"
		
		val input_file = project_dir + "INPUT_LUT.csv"
		
		val L1_weight_file = project_dir + "L1_W_LUT.csv"
		val L1_bias_file = project_dir + "L1_B_LUT.csv"

		val L2_weight_file = project_dir + "L2_W_LUT.csv"
		val L2_bias_file = project_dir + "L2_B_LUT.csv"

		/* 
		 * First Layer parameters
		 */

		// First layer paramters (LSTM units)
		val input_size = 1
		val timesteps = 1
		val num_l1_units = 2
		val num_lstm_weights = 4
		val num_lstm_biases = 4


		/*
		 * Second Layer Parameters
		 */

		// Second Layer neuron paramters
		val num_l2_neurons = 5


		//val stream_in  = StreamIn[Float](GPInput1); countOf(stream_in) = 1024l
		//val stream_out = StreamOut[Float](GPOutput1)
	    

		
  		val x = ArgIn[Float]
    		val out = ArgOut[Float]
		setArg(x, X)
		

		Accel {


		   // Activation Function approximations courtesy of Tian
		   def sigmoid_synth(p: Float) = {
			      (tanh_synth(p / 2.to[Float]) + 1.to[Float]) / 2.to[Float]
		  }

		   def tanh_synth(p: Float) = {
			   type tanhT = FixPt[TRUE, _4, _8]

			   val pt = p.to[tanhT]
			   val absp = abs(pt)
			   val absre = if (absp > (2.5).to[tanhT]) {
				   1.to[tanhT]
			   } else if (((0.5).to[tanhT] < absp) && (absp <= (2.5).to[tanhT])) {
				   // bug: if replace div4 with the shifted result directly, spatial would infer the type of absp >> 2 as FixPt[TRUE, 0, 3]
				   val div4 = absp >> 2
				   val div4Offset = div4 + (0.375).to[tanhT]
				   div4Offset
			   } else {
				   absp
			   }

			   val mabre = 0.to[tanhT] - absre
			   val re = mux((pt < 0.to[tanhT]), mabre, absre)
			   re.to[Float]
		   }



			val s_reg = Reg[Float](0)
			val L1_h = RegFile[Float](num_l1_units)
			val L1_tmp = RegFile[Float](num_l1_units * num_lstm_weights)

			
			Foreach(0 until num_l1_units){ i =>
				L1_h(i) = 0
			}

			val L1_C = RegFile[Float](num_l1_units)
			
			Foreach(0 until num_l1_units){ i =>
				L1_C(i) = 0
			}

			val L2_tmp = RegFile[Float](num_l2_neurons)

			val input = LUT.fromFile[Float](input_size, timesteps)(input_file)

			val L1_W_LUT = LUT.fromFile[Float](input_size + num_l1_units, num_lstm_weights * num_l1_units)(L1_weight_file)
      			val L1_B_LUT = LUT.fromFile[Float](num_l1_units * num_lstm_weights)(L1_bias_file)
      			
			
			val L2_W_LUT = LUT.fromFile[Float](num_l1_units, num_l2_neurons)(L2_weight_file)
      			val L2_B_LUT = LUT.fromFile[Float](num_l2_neurons)(L2_bias_file)
		
			Pipe {
				s_reg := x
			}
			
			
			// First LSTM MatMul
			Pipe {
				val input_step = RegFile[Float](input_size + num_l1_units)

				Foreach(0 until input_size){ i =>
					input_step(i) = input(0, i)
				}
				
				Foreach(0 until num_l1_units){ i =>
					input_step(i + input_size) = L1_h(i)
				}

				Foreach(0 until (num_lstm_weights*num_l1_units)){ j => 

					val w = Reg[Float]
					
					w := Reduce(Reg[Float](0.to[Float]))( (num_l1_units + input_size) by 1){ i =>
						L1_W_LUT(i,j).to[Float] * input_step(i)
					}{_ + _}

					L1_tmp(j) = w + L1_B_LUT(j) + s_reg
				}
			

			}

			// First layer activation functions
			Pipe {

				Foreach(0 until num_l1_units) { i =>
					L1_C(i) = L1_C(i) * sigmoid_synth(L1_tmp(i + 2*(num_l1_units)) + 1.0f) + sigmoid_synth(L1_tmp(i)) * tanh_synth(L1_tmp(i + 1*(num_l1_units)))
					L1_h(i) = tanh_synth(L1_C(i)) * sigmoid_synth(L1_tmp(i + 3*(num_l1_units))) 
				}


			}

			// Layer 2 no activation function
			Pipe {
				Foreach(0 until num_l2_neurons){ i =>
				
					val w = Reg[Float]
					
					w := Reduce(Reg[Float](0.to[Float]))(num_l1_units by 1){ j =>
						L1_h(j) * L2_W_LUT(j,i)
					}{_+_}
					
					L2_tmp(i) = w + L2_B_LUT(i)

				}


			}

			// Hard max
			Pipe {

				val w = Reg[Float]

				w := Reduce(Reg[Float](0.to[Float]))(num_l2_neurons by 1){ i=>
					L2_tmp(i)	
				}{ _ + _ }

				out := w
			}
			
			
		}
		getArg(out)
	}


	@virtualize
	def main() = {

		predict(args(0).to[Float])
		//print("\nResult: " + result + "\n\n")
	}

}
