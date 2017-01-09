package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(x => x._1 -> Signal(eval(x._2(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def innerEval(expr: Expr, nameList: List[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => {
          val next = getReferenceExpr(name, references)
          if(nameList.exists(x => name.equals(x)))
            Double.NaN
          else
            innerEval(next, name :: nameList)
        }
        case Plus(a, b) => innerEval(a, nameList) + innerEval(b, nameList)
        case Minus(a, b) => innerEval(a, nameList) - innerEval(b, nameList)
        case Times(a, b) => innerEval(a, nameList) * innerEval(b, nameList)
        case Divide(a, b) => innerEval(a, nameList) / innerEval(b, nameList)
      }
    }

    innerEval(expr, Nil)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
