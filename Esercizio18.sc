abstract class Stack[+A] {
  def push[B >: A](elem: B): Stack[B]
  def top(): A
  def pop() : Stack[A]
  def isEmpty: Boolean
}

case class NonEmptyStack[+A](start: A, tail: Stack[A]) extends Stack[A] {
  def push[B >: A](elem: B): Stack[B] = new NonEmptyStack[B](elem, this)
  def top(): A = start
  def pop(): Stack[A] = tail
  def isEmpty(): Boolean = false
}

case class EmptyStack[+T]() extends Stack[T] {
  def push[B >: T](elem: B): Stack[B] = new NonEmptyStack[B](elem, this)
  def top() = throw new IllegalArgumentException("empty stack")
  def pop() = throw new IllegalArgumentException("empty stack, pop impossible")
  def isEmpty() = true
}

class F1Car(val carModel: String) extends Object{
  var model = carModel
  override def toString: String = "Modello: " + model
}

class F1CarConstructor(carModel: String, costruttore: String) extends F1Car(carModel) {
  var constructor = costruttore
  override def toString: String = "Modello: " + model + " / Casa Produttrice: " + constructor
}

val stack = EmptyStack()
stack.isEmpty()
//stack.pop()  --> java.lang.IllegalArgumentException: empty stack, pop impossible
//stack.top()  --> java.lang.IllegalArgumentException: empty stack
var stack1 = stack.push(new F1CarConstructor("Model-1", "Ferrari"))
val stack2 = stack1.push(new F1CarConstructor("Model-2", "Mercedes"))
val stack3 = stack2.push(new F1CarConstructor("Model-3", "McLaren"))
val stack4 = stack3.push(new F1Car("Model-4"))
stack4.top()
val stack5 = stack4.pop()
stack5.top()
stack5.isEmpty