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
   * Return another map from the *same set* of variable names (form fields) 
   * to their actual values, computed from their expressions.
   */
	def computeValues(
		namedExpressions: Map[String, Signal[Expr]] // key is named variable, ex: "a", "b", "c", ...
	): Map[String, Signal[Double]] = { // potentially linked evals of expressions
		for{(k, v) <- namedExpressions} yield (
		    k -> Signal{ eval(v(), namedExpressions) }
	    ) 
	}

	// Helper
	def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
		expr match {
	      	case Literal(v) => v
	      	case Ref(name) => {
	      		val filteredRefs = for{
					(k, v) <- references
					if k != name
				} yield (k -> v)
	      		eval(getReferenceExpr(name, references), filteredRefs)
	      	}
	      	case Plus(a, b) => eval(a, references) + eval(b, references)
	      	case Minus(a, b) => eval(a, references) - eval(b, references)
	      	case Times(a, b) => eval(a, references) * eval(b, references)
	      	case Divide(a, b) => eval(a, references) / eval(b, references)
		}
	}

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr( // returns a single Expr object
    name: String, // Named variable
    references: Map[String, Signal[Expr]]
  ) = {
    references.get(name).fold[Expr] { // Option[Signal[Expr]]
    		Literal(Double.NaN) // ifEmpty
    } {
    		/* Signal[Expr] => Signal.apply  i.e. myValue
    		 * register observers.
    		 */
    		exprSignal => exprSignal() 
    	}
  }
  
}
