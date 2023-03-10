abstract class Expr {
  def semplifica(): Expr
}

case class Var(name: String) extends Expr {
  override def semplifica(): Expr = this

  override def toString: String = name
}

case class Number(num: Double) extends Expr {
  override def semplifica(): Expr = this

  override def toString: String = num.toString
}

case class UnOp(operator: String, arg: Expr) extends Expr {
  override def semplifica(): Expr = {
    UnOp(operator,arg.semplifica()) match {
      case UnOp("-",UnOp("-",expr))=>expr       //- ( - x)) = x
      case UnOp(op,expr)=>UnOp(op,expr)         //caso generale
    }
  }

  override def toString: String = "("+operator+" "+arg+")"
}

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr {
  override def semplifica():Expr = {
    BinOp(operator, left.semplifica, right.semplifica) match {
      case BinOp("+", left, Number(0)) => left            //x + 0 = x
      case BinOp("*", left, Number(1)) => left            //x * 1 = x
      case BinOp("*", _, Number(0)) => Number(0)          //x * 0 = 0
      case BinOp("+", Number(0), right) => right          //0 + x = x
      case BinOp("*", Number(1), right) => right          //1 * x = x
      case BinOp("*", Number(0), _) => Number(0)          //0 * x = x
      case BinOp(op,left,right) => BinOp(op,left,right)   //caso generale
    }
  }
  override def toString: String= "("+left.toString + " " + operator +" "+ right.toString+")"
}


var unop:Expr=UnOp("-",UnOp("-",Number(10)))
var bin1= BinOp("+",BinOp("+",Var("x"),Var("y")),Number(0))
var bin2= BinOp("*",BinOp("*",Number(1),Var("x")),Var("x"))
var bin3= BinOp("+",BinOp("+",Var("x"),UnOp("-",UnOp("-",Var("y")))),Number(0))
var bin4= BinOp("*",Number(0),BinOp("+",Var("x"),Var("y")))
var bin5= BinOp("+",Number(1),BinOp("+",BinOp("+",Var("x"),Var("y")),Number(0)))
var bin6= BinOp("+",Number(0),Var("x"))
var bin7= BinOp("+",BinOp("-",Var("x"),UnOp("-",UnOp("-",Var("y")))),Number(1))


unop.semplifica()
bin1.semplifica()
bin2.semplifica()
bin3.semplifica()
bin4.semplifica()
bin5.semplifica()
bin6.semplifica()
bin7.semplifica()