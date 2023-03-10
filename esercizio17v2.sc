abstract class Expr {

}

case class Var(name: String) extends Expr {
  override def toString: String = name
}

case class Number(num: Double) extends Expr {
  override def toString: String = num.toString
}

case class UnOp(operator: String, arg: Expr) extends Expr {
  override def toString: String = "(" + operator + " " + arg + ")"
}

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr {
  override def toString: String = "(" + left.toString + " " + operator + " " + right.toString + ")"
}

val semplifica: Expr => Expr = espressione => espressione match {
  case Var(name) => Var(name)
  case Number(num) => Number(num)
  case UnOp(operator, arg) =>
    UnOp(operator, semplifica(arg)) match {
      case UnOp("-", UnOp("-", expr)) => semplifica(expr) //- ( - x)) = x
      case UnOp(op, expr) => UnOp(op, expr) //caso generale
    }
  case BinOp(operator, left, right)=>
    BinOp(operator,semplifica(left),semplifica(right)) match {
      case BinOp ("+", left, Number (0) ) => left //x + 0 = x
      case BinOp ("*", left, Number (1) ) => left //x * 1 = x
      case BinOp ("*", _, Number (0) ) => Number (0) //x * 0 = 0
      case BinOp ("+", Number (0), right) => right //0 + x = x
      case BinOp ("*", Number (1), right) => right //1 * x = x
      case BinOp ("*", Number (0), _) => Number (0) //0 * x = x
      case BinOp (op, left, right) => BinOp (op, left, right) //caso generale
    }
}

semplifica(UnOp("-",UnOp("-",Number(1715))))
semplifica(BinOp("+",BinOp("+",Var("x"),Var("y")),Number(0)))
semplifica(BinOp("*",BinOp("*",Number(1),Var("x")),Var("x")))
semplifica(BinOp("-",BinOp("",Var("x"),UnOp("-",UnOp("-",Var("y")))),Number(0)))
semplifica(BinOp("*",Number(0),BinOp("+",Var("x"),Var("y"))))
semplifica(BinOp("+",Number(1),BinOp("+",BinOp("+",Var("x"),Var("y")),Number(0))))
semplifica(BinOp("+",Number(0),Var("x")))
semplifica(BinOp("+",BinOp("-",Var("x"),UnOp("-",UnOp("-",Var("y")))),Number(1)))
semplifica(BinOp("*", Var("x"), BinOp("*", Var("x"), Number(0))))