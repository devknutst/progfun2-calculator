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
      namedExpressions.map( a => a._1 -> Signal(eval(a._2(), namedExpressions)))
  }

  def eval(expr: Expr, ref: Map[String, Signal[Expr]]): Double = expr match {
    case e: Literal => e.v
    case e: Ref => {
      getReferenceExpr(e.name, ref) match {
        case (t: Literal) => t.v
        case (t) => {
          if (!isCyclic(e.name, ref, Set(), ref.get(e.name).get())) eval(t, ref)
          else Double.NaN
        }
      }
    }
    case e: Plus => eval(e.a, ref) + eval(e.b, ref)
    case e: Minus => eval(e.a, ref) - eval(e.b, ref)
    case e: Times => eval(e.a, ref) * eval(e.b, ref)
    case e: Divide => eval(e.a, ref) / eval(e.b, ref)
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  private def isCyclic(name: String, ref: Map[String, Signal[Expr]], comp: Set[String], e: Expr): Boolean = {

    if (comp.contains(name)) true
    else {
      e match {
        case t: Literal => false
        case t: Ref =>  isCyclic(name, ref, comp + t.name, ref.get(t.name).get())
        case t: Plus => isCyclic(name, ref, comp, t.a) ||  isCyclic(name, ref, comp, t.b)
        case t: Minus => isCyclic(name, ref, comp, t.a) ||  isCyclic(name, ref, comp, t.b)
        case t: Times => isCyclic(name, ref, comp, t.a) ||  isCyclic(name, ref, comp, t.b)
        case t: Divide => isCyclic(name, ref, comp, t.a) ||  isCyclic(name, ref, comp, t.b)
      }
    }
  }

}
