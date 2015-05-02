package calculator

object Polynomial {
	// http://www.rapidtables.com/math/algebra/Quadratic_equation.htm
  
	import math.sqrt
  
	def computeDelta(
		a: Signal[Double], 
		b: Signal[Double],
		c: Signal[Double]
	): Signal[Double] = {
		// Δ = b² - 4ac
		Signal {
			(b() * b()) - (4 * a() * c())
		}
	}

	def computeSolutions(
		a: Signal[Double], 
		b: Signal[Double],
		c: Signal[Double], 
		delta: Signal[Double]
	): Signal[Set[Double]] = {
		// (-b ± √Δ) / (2a)
		Signal {
			delta() match {
			  	case dVal if dVal > 0 => {
			  		// (-b+√Δ)/(2a) and x2=(-b-√Δ)/(2a)
			  		val aVal = a()
			  		val bVal = b()
			  		val cVal = c()
			  		Set(
				  		(-bVal + sqrt(dVal)) / (2 * aVal),
				  		(-bVal - sqrt(dVal)) / (2 * aVal)
				  	)
			  	}
			  	case dVal if dVal == 0 => {
			  		// -b/(2a)
			  		val aVal = a()
			  		val bVal = b()
			  		Set (-bVal / (2 * aVal))
			  	}
			  	case _ => Set()
			}
		}
	}
}



