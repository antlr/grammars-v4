sealed trait Expr
case class Num(value: Double) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Neg(expr: Expr) extends Expr
case class Var(name: String) extends Expr

object Evaluator {
  def eval(expr: Expr, env: Map[String, Double] = Map.empty): Double = expr match {
    case Num(v)    => v
    case Var(name) => env.getOrElse(name, 0.0)
    case Add(l, r) => eval(l, env) + eval(r, env)
    case Mul(l, r) => eval(l, env) * eval(r, env)
    case Neg(e)    => -eval(e, env)
  }

  def stringify(expr: Expr): String = expr match {
    case Num(v)    => v.toString
    case Var(name) => name
    case Add(l, r) => s"(${stringify(l)} + ${stringify(r)})"
    case Mul(l, r) => s"(${stringify(l)} * ${stringify(r)})"
    case Neg(e)    => s"(-${stringify(e)})"
  }
}
