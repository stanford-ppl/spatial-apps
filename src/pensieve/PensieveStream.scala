import spatial.dsl._
import virtualized._

object PensieveStream extends SpatialApp {
	override val target = targets.Plasticine
	import target._



	@virtualize
	def predict() = {

		val project_dir = s"${sys.env("SPATIAL_HOME")}/apps/src/pensieve/PENSIEVE_LUTs/"
		
		val input_pad_file = project_dir + "INPUT_LUT.csv"
		
		val filter_weight_file = project_dir + "L1_CONV1D_W_LUT.csv"
		val filter_bias_file = project_dir + "L1_CONV1D_B_LUT.csv"
		val L1_neuron_weight_file = project_dir + "L1_NEURON_W_LUT.csv"
		val L1_neuron_bias_file = project_dir + "L1_NEURON_B_LUT.csv"

		val L2_weight_file = project_dir + "L2_NEURON_W_LUT.csv"
		val L2_bias_file = project_dir + "L2_NEURON_B_LUT.csv"

		val L3_softmax_weight_file = project_dir + "L3_SOFTMAX_W_LUT.csv"
		val L3_softmax_bias_file = project_dir + "L3_SOFTMAX_B_LUT.csv"
		
		/* 
		 * First Layer parameters
		 */

		// 1D Convolution layer parameters
		val num_1d_convs = 2
		val num_filters = 2
		val filter_size = 2
		val window_size = 8
		val padding = filter_size - 1

		// First layer neuron paramters
		val num_l1_neurons = 2
		val l1_neuron_size = 2

		
		
		val l1_conv1d_size = num_filters * window_size
		
		val total_l1_conv1d_res_size = num_1d_convs * l1_conv1d_size
		val total_l1_neuron_res_size = num_l1_neurons * l1_neuron_size

		val L1_res_size = total_l1_conv1d_res_size + total_l1_neuron_res_size


		/*
		 * Second Layer Parameters
		 */

		// Second Layer neuron paramters
		val l2_neuron_size = L1_res_size
		val num_l2_neurons = 2



		/*
		 * Third Layer Parameters
		 */

		val num_actions = 2


		val stream_in  = StreamIn[Float](GPInput1)
	    
		val stream_out = StreamOut[Float](GPOutput1)
	    

  		//val x = ArgIn[Float]
    		//val out = ArgOut[Float]
		//setArg(x, X)


		Accel(*) {

			val s_reg = Reg[Float](0)
			
			val L1_res = RegFile[Float](L1_res_size)
			val L2_res = RegFile[Float](num_l2_neurons)
			val L3_res = RegFile[Float](num_actions)

			val input_pad = LUT.fromFile[Float](window_size + padding)(input_pad_file)

      			val Conv_Filter_LUT = LUT.fromFile[Float](num_1d_convs, num_filters * filter_size)(filter_weight_file)
      			val Conv_B_LUT = LUT.fromFile[Float](num_1d_convs, num_filters)(filter_bias_file)
			
			val L1_Neuron_W_LUT = LUT.fromFile[Float](num_l1_neurons, l1_neuron_size)(L1_neuron_weight_file)
      			val L1_Neuron_B_LUT = LUT.fromFile[Float](num_l1_neurons, l1_neuron_size)(L1_neuron_bias_file)
      			
			
			val L2_W_LUT = LUT.fromFile[Float](num_l2_neurons, l2_neuron_size)(L2_weight_file)
      			val L2_B_LUT = LUT.fromFile[Float](num_l2_neurons)(L2_bias_file)
		
			
			val L3_SM_W_LUT = LUT.fromFile[Float](num_actions, num_l2_neurons)(L3_softmax_weight_file)
			val L3_SM_B_LUT = LUT.fromFile[Float](num_actions)(L3_softmax_bias_file)


			// First layer - combination of 1d convolutions and neurons
			Pipe {

        Pipe {
				  s_reg := stream_in.value
        }

					// 1d Convolutions
					Foreach(0 until num_1d_convs){ c =>
						Foreach(0 until window_size){ i => 

							Foreach(0 until num_filters){ k =>
							
								val w = Reg[Float](0)
								w := Reduce(Reg[Float](0.to[Float]))(filter_size by 1){ j =>
									Conv_Filter_LUT(c, k * num_filters + j) * input_pad(i + j)
								
								}{_ + _}

								L1_res(c * l1_conv1d_size + i * num_filters + k) = max(w + Conv_B_LUT(c, k) + s_reg, 0)
							
							}
						}
					}

					// Regular neurons
					Foreach(0 until num_l1_neurons){ i =>
						Foreach(0 until l1_neuron_size){ j => 
							
							val w = Reg[Float]

							w := L1_Neuron_W_LUT(i,j).to[Float] * input_pad(i+1)

							L1_res(total_l1_conv1d_res_size + i * l1_neuron_size + j) = max(w + L1_Neuron_B_LUT(i,j), 0)
						}
					}

			}

			/*
			
			Pipe {
				Foreach(0 until L1_res_size){ i=> 
					print(L1_res(i))
					print("\n")
				}
			}
			*/

			
		        // Layer 2 - just regular neurons
			Pipe {

				Foreach(0 until num_l2_neurons){ i => 

					val w = Reg[Float]

					w := Reduce(Reg[Float](0.to[Float]))(l2_neuron_size by 1){ j =>
						L2_W_LUT(i,j).to[Float] * L1_res(j)
					}{_ + _}

					L2_res(i) = max(w + L2_B_LUT(i), 0)
				}
			}



			// Softmax layer
			Pipe {

				val numerators = RegFile[Float](num_actions)
				val denominator = Reg[Float](0)

				Foreach(0 until num_actions){ i =>
				
					val wx = Reg[Float]

					wx := Reduce(Reg[Float](0.to[Float]))(num_l2_neurons by 1){ j =>
						L3_SM_W_LUT(i, j) * L2_res(j)
					}{_ + _}

					numerators(i) = exp_taylor(wx + L3_SM_B_LUT(i))
				
				}


				Pipe {
					denominator := Reduce(Reg[Float](0.to[Float]))(num_actions by 1){ i =>
						numerators(i)
					}{_ + _}
				}

				Pipe {
					
					Foreach(0 until num_actions){ i =>
						L3_res(i) = numerators(i) / denominator
						print(L3_res(i))
						print("\n")
					}

					stream_out := L3_res(0)
				}


			}
			
			
			
			
			
		}
		//getArg(out)
	}


	@virtualize
	def main() = {

		predict()
	}

}

