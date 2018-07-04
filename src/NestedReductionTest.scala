import spatial.dsl._
import virtualized._

object NestRed extends SpatialApp {
	override val target = targets.Plasticine
	import target._



	@virtualize
	def main() = {

		val num_features = 2
		val num_sv = 6

  	
		val stream_in1  = StreamIn[Float](GPInput1)
		val stream_in2  = StreamIn[Float](GPInput2)
	    
		val stream_out = StreamOut[Float](GPOutput1)
	    

		Accel(*) {
	
			val test_pt = RegFile[Float](num_features)
			val w = Reg[Float] 
			val sq_diff = Reg[Float]
			
		

			Pipe {


				Pipe { 
					test_pt(0) = stream_in1.value
				}

				Pipe {
			
					test_pt(1) = stream_in2.value

				}



				// Toy example
				w := Reduce(Reg[Float](0.to[Float]))(num_sv by 1 par num_sv){i =>
					// RBF Kernel
					sq_diff := Reduce(Reg[Float](0.to[Float]))(num_features by 1 par num_features){j => 
						(23.5f - test_pt(j)) * (23.5f - test_pt(j))
					}{_+_}
					
					32.5f * exp_taylor(-1.0f * 0.5f * sq_diff)
				}{_+_}
			}

			Pipe {
				stream_out := w - 3.0f
			}
		}
	}


}

