// Covers: wildCardSelector in importSpec (without braces), singleton simpleLiteral,
//         typTypeParamClause ARROW expr (poly lambda), infixExpr id ASSIGN expr,
//         defSig (COLON type_)? abstract, usingParamClause in defParamClause,
//         INFIX modifier, quoted, BacktickId, givenType, extension_ in templateStat/blockStat,
//         constrApps templateBody in givenImpl

// wildCardSelector as direct importSpec (no braces): import foo.given
import scala.math.given

// BacktickId
val `my val` = 42
def `my method`(): Int = `my val`

// singleton simpleLiteral: type based on literal singleton e.g. 1.type
type OneType = 1.type
type HelloType = "hello".type

// typTypeParamClause ARROW expr: polymorphic function value
val polyId: [T] => T => T = [T] => (x: T) => x
val polyConst: [A] => A => Int = [A] => (x: A) => 0

// infixExpr id ASSIGN expr: x + = 5 (op-assign with space)
object InfixAssign {
  def demo(): Unit = {
    var x = 0
    x + = 5
  }
}

// INFIX modifier in class/method definition
infix class Pair[A, B](val a: A, val b: B)

// defSig (COLON type_)? abstract in trait (no body, no =)
trait HasAbstract {
  def abstractMethod(n: Int): String
  val abstractVal: Int
}

// usingParamClause in defParamClause (def with using clause at top level)
def myUsingDef(x: Int)(using scale: Int): Int = x * scale

// quoted: QUOTE LBRACE block RBRACE
object MacroDemo {
  import scala.quoted.*
  def q1(using Quotes): Expr[Int] = '{ 1 + 2 }
  def q2(using Quotes): Type[Int] = '[Int]
}

// givenType in givenDef: annotType (id annotType)* — using complex givenType
// (givenImpl: givenType ASSIGN expr where givenType has id between annotTypes)
// Note: givenType = annotType (id annotType)* — hard to trigger the (id annotType)* part
// since `with` is WITH keyword not id. Using an infix type via Op:
given mySpecialOrd: (Ordering[Int]) = Ordering.Int

// constrApps templateBody in givenImpl
trait MyShow[A] { def show(x: A): String }
given intShow: MyShow[Int] with MyShow[Int] {
  def show(x: Int): String = x.toString
}

// extension_ in templateStat
object ExtInTemplate {
  class Wrapper(val n: Int)
  extension (w: Wrapper) {
    def doubled: Int = w.n * 2
  }
}

// extension_ in blockStat (local extension)
def withLocalExtension(x: Int): String = {
  extension (n: Int) { def asStr: String = n.toString }
  x.asStr
}
