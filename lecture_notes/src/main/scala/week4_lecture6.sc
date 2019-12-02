trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + "+" + show(e2)
  case Prod(e1, e2) => show(e1) + "*" + show(e2)
}

def exprToString(e: Expr): String = {
  def exprPrevIsProd(e: Expr, prevIsProd:Boolean):String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => {
      val thisSum = exprPrevIsProd(e1, false) + "+" + exprPrevIsProd(e2, false)
      if (prevIsProd) "(" + thisSum + ")" else thisSum
    }
    case Prod(e1, e2) => exprPrevIsProd(e1, true) + "*" + exprPrevIsProd(e2, true)
  }

  exprPrevIsProd(e, false)
}


println(show(Prod(Sum(Number(2), Number(3)), Number(4))))
println(show(Prod(Sum(Number(2), Number(3)), Number(4))) == "2+3*4")

println(exprToString(Prod(Sum(Number(2), Number(3)), Number(4))))
println(exprToString(Prod(Sum(Number(2), Number(3)), Prod(Number(4), Sum(Number(2), Number(3))))))

