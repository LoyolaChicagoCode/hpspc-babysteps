object integration extends App {

	type Fx = (Double) => Double

	def xSquared (x : Double) : Double = {
  		return x * x 
	}

	def integrate ( a : Double, b : Double, g : Int, f : Fx) : Double = {
		def FxValues(): List[Double] = {
			var interval = (b-a) / g;
    		for { n <- List.range(1 , g) } 
    		   	yield { f(a + n * interval) }
		}
		var fxSum = FxValues().foldLeft(0.0)(_ + _)
    	return (b - a) / g * (f(a) / 2.0 + f(b) / 2.0 + fxSum)
	}


	var area = integrate( 0, 10, 1000, xSquared)
	println("Area = " + area)
}


