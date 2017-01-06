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
    def isCyclic(expr: Expr, prev: List[String]): Boolean = {
      expr match {
        case Literal(v) => false
        case Ref(name) => {
          if(prev.exists(x => name.equals(x)))
            true
          else
            isCyclic(getReferenceExpr(name, references), name :: prev)
        }
        case Plus(a, b) => isCyclic(a, prev) || isCyclic(b, prev)
        case Minus(a, b) => isCyclic(a, prev) || isCyclic(b, prev)
        case Times(a, b) => isCyclic(a, prev) || isCyclic(b, prev)
        case Divide(a, b) => isCyclic(a, prev) || isCyclic(b, prev)
      }
    }

    expr match {
      case Literal(v) => v
      case Ref(name) => {
        val next = getReferenceExpr(name, references)
        if(isCyclic(next, List(name)))
          Double.NaN
        else
          eval(next, references)
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
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
