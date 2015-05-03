package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr // Represents a reference to another variable in the map namedExpressions. Named variables; ex: "a", "b", "c", ...
// ---- Operations ---------------------------------//
final case class Plus(a: Expr, b: Expr) extends Expr // "a" and "b" here are not the "Named variables" mentioned above, but rather local variables
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  /*
   * Input: map from variable name (form field, right?) to expressions of their 
   * values.
   * Return another map from the *same set* of variable names (form fields, 
   * right?) to their actual values, computed from their expressions.
   * Q: What about order of Expr dependencies? Recursion?
   * TODO: Make sure to check whether the value has changed before causing it
   * to be updated (Doesn't Signal already do this?)
   * 
   * file:///Users/hieronymus/Development/Courses/Coursera/Reactive%20Programming/assignments/Discussions/Calculator%20discussions/Calculator%20-%20What%20does%20the%20parameter%20references%20imply_.html
   * references is the set of existing known pairs of cell names and their expressions. It is similar to namedExpressions.
   * computeValues will return an immutable Map. How you construct that is up to you but the easiest way is probably to map a function over the namedExpressions -- a function that accepts a key/value pair as input and produces a key/value as output. Hint: that function will somehow eval the expression that is the value of that key/value pair.
   */
	def computeValues(
		namedExpressions: Map[String, Signal[Expr]] // key is named variable, ex: "a", "b", "c", ...
	): Map[String, Signal[Double]] = { // potentially linked evals of expressions
		println("====>>In compute values")
	  
		/* file:///Users/hieronymus/Development/Courses/Coursera/Reactive%20Programming/assignments/Discussions/Calculator%20discussions/Cannot%20get%20Calculator%20to%20see%20changes.html
		 * Finish reading this thread 
		 */
		for{
			(k, v) <- namedExpressions
		} yield (
		    k -> Signal{ 
    	  			eval(v(), namedExpressions) 
		    }
	    ) 
	} // end comptueVAlues

	// Helper
	// Q: What does this do?
	// Recursive? Yes, I'm sure
	/* What are the references for? Just something to do with anti-cyclic
	 * checking? 
	 * 
	 * I'M NOT HANDLING THE REFERENCES CORRECTLY YET
	 */
	/*
	 * https://class.coursera.org/reactive-002/forum/thread?thread_id=338
	 * It took me sometime to figure how to solve it, think about how far you can simplify an expression once you encounter a reference (which can subsequently have nested references deep down) to its base condition (kind of denormalization for the lack of better word) and introduce interim checks, anything more i'd say i'd be pretty much end up giving out the solution.
	 *
	 * https://class.coursera.org/reactive-002/forum/thread?thread_id=211
	 * think about the Map that you're passing around. What happens when something is missing from the Map?
	 *
	 * https://class.coursera.org/reactive-002/forum/thread?thread_id=211
	 * Look carefully at getReferenceExpr. It gets passed a name for which you want to get the expr.
	 * Why does it need to be passed a List of references? What does it do with the list. Why does each method get passed its own List of references/namedExpressions. 
	 * 
	 * https://class.coursera.org/reactive-002/forum/thread?thread_id=329  
	 * It was indeed '2+3' instead of '2 + 3' the error
	 */
	def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
		expr match {
	      	case Literal(v) => {
	      		println("In eval Literal: " + v)
	      		v
	      	}
	      	case Ref(name) => {
	      		println("In eval Ref: " + name)
	      		eval(getReferenceExpr(name, references), references)
	      	}
	      	case Plus(a, b) => {
	      		println("In eval Plus: " + a + " + " + b)
	      		/* 
	      		 * a or b could be any expression type, so recurse both sides
	      		 * of operation.
	      		 */
	      		eval(a, references) + eval(b, references)
	      	}
	      	case Minus(a, b) => {
	      		println("In eval Minus: " + a + " - " + b)
	      		eval(a, references) - eval(b, references)
	      	}
	      	case Times(a, b) => {
	      		println("In eval Times: " + a + " * " + b)
	      		eval(a, references) * eval(b, references)
	      	}
	      	case Divide(a, b) => {
	      		println("In eval Divide: " + a + " / " + b)
	      		eval(a, references) / eval(b, references)
	      	}
		}
		//???
	}

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  // This is pretty straight forward.
  private def getReferenceExpr( // returns a single Expr object
    name: String, // Named variable
    references: Map[String, Signal[Expr]]
  ) = {
    println("====>>In getReferenceExpr")
    references.get(name).fold[Expr] { // Option[Signal[Expr]]
    		Literal(Double.NaN) // ifEmpty
    } {
    		/* Signal[Expr] => Signal.apply  i.e. myValue
    		 * Also does everything that Signal.apply does, i.e. register observers.
    		 */
    		exprSignal => exprSignal() 
    	}
  }
  
}
