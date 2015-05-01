package calculator

import scala.util.DynamicVariable

class Signal[T](expr: => T) {
  import Signal._ // references object Signal
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  private var observed: List[Signal[_]] = Nil
  update(expr)

  protected def computeValue(): Unit = {
    
    for (sig <- observed) {// List[Signal]
      sig.observers -= this // Set[Signal[_]]
    }
    
    observed = Nil
    
    /* 
     * 'this', may I remind you, is a Signal.
     * caller is a DynamicVariable that is doing localThread magic. 
     */
    val newValue = caller.withValue(this)(myExpr()) // T
    /* 
     * Odersky: Disable the following "optimization" for the assignment, because we
     * want to be able to track the actual dependency graph in the tests.
     */
    //if (myValue != newValue) {
      myValue = newValue // transformation
      val obs = observers 
      /* 
       * observers is var of Set() of Signal[_] , but how are the observers 
       * being re-populated? I believe it is by the apply method, i.e.: 
       * observers += caller.value
       * So then where/how is apply being invoked?
       */
      observers = Set()
      obs.foreach(_.computeValue()) // generalized propagation
    //}
  } // end computeValue

  protected def update(expr: => T): Unit = {
    /* 
     * The expression is stored for further (consistent, uniform, repeatable) 
     * application (i.e. value production). 
     * This is a def (function) that seems to have a body, but no (empty) 
     * input parameters. The return type is Unit. So it is just an executable
     * body with no input nor output. I have commented elsewhere in this file
     * about how I think this works (i.e. it is just providing behavior that
     * that interacts with fields external to itself (within the enclosing 
     * Signal encapsulation). More specifically, it is used by computeValue 
     * to do what that name implies.
     * See: val newValue = caller.withValue(this)(myExpr()) // T
     */
    myExpr = () => expr 
    /* 
     * computeValue carries out the important task of binding the newValue
     * to all of the observers. 
     * */
    computeValue()
  } // end update

  /*
   * How/where/when is this being invoked? To answer that I need to 
   * trace/establish a more complete and integrated picture of program
   * flow.
   */
  def apply() = {
    println("~~In Signal apply")
    observers += caller.value // callers are observers
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    caller.value.observed ::= this // this is observed
    myValue
  }
} // end class Signal[T](expr: => T)

// Var is an extention of Signal
// Odersky: Singal is assumed to be a Var Signal
class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

/*
 * This is pretty interesting. Var is serving its characteristic
 * of representing mutable state. But as an object (class instantiation)
 * it actually performs functionally in that change of value is represented
 * by a new object version which in this case contains the expression which
 * causes, or drives the change.
 * 
 * Hmmm... I'm starting to build a broader picture of this apperatus. I think
 * the sequence goes like this: Var.apply constructs a new instance of class
 * Var, which in turn calls super.update(expr). In true functional practice
 * The expresion remains integrally linked with the resulting (consequential)
 * value it produces. super.update then calls computeValue, which involves
 * caller.withValue (DynamicVariable) using the passed expression to compute 
 * the value--but there is some necessary thread magic, which I don't quite 
 * understand the purpose of yet (but I'd like to! And I will!) to compute 
 * the value, which is then further applied in a general propagation on all 
 * registered observers (and their observers) (cascading broadcasted recursion).
 */
object Var {
  def apply[T](expr: => T) = new Var(expr) // new Var is of class Var
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

object Signal {
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr) // references class Signal
}
