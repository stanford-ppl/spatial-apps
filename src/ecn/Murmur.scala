import spatial.dsl._
import virtualized._

trait Hashes extends SpatialApp {


	type UInt = FixPt[FALSE,_32,_0]

	@virtualize
	def murmur(x: UInt) = {

			val c1 = Reg[UInt]
			val c2 = Reg[UInt]
			val n = Reg[UInt]
			val m = Reg[UInt]
			val seed = Reg[UInt]
			val k = Reg[UInt]
			val res = Reg[Index]
			c1 := 0xcc9e2d51
			c2 := 0x1b873593
			n := 0xe6546b64
			m := 5
			seed := 1234
			
			 
			val step1 = x * c1
			val step2 = (step1 << 15) | (step1 >> 17)
			val step3 = step2 * c2 
			val step4 = seed ^ step3
			val step5 = (step4 << 13) | (step4 >> 19) 
			val step6 = (step5 * m) + n 
			val step7 = step6 ^ 4 
			val step8 = step7 ^ (step7 >> 16) 
			val step9 = step8 * 0x85ebca6b 
			val step10 = step9 ^ (step9 >> 13) 
			val step11 = step10 * 0xc2b2ae35 
			val step12 = step11 ^ (step11 >> 16)  
			val step13 = step12 % 1024 
			
			res := step13.to[Index]

			res

	}

	
}

