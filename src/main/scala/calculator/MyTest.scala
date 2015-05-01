package calculator

object MyTest extends Application {

  	def huh = () => 3
	val v = new Var(huh)
  	println("v: " + v)
}