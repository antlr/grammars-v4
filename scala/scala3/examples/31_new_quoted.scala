// Covers: NEW constrApp (WITH constrApp)* templateBody?, NEW templateBody,
//         blockExpr LBRACE caseClauses RBRACE, argumentExprs blockExpr,
//         namedExprInParens, LPAREN USING exprsInParens RPAREN,
//         LPAREN (exprsInParens COMMA)? postfixExpr Op RPAREN (varargs)

trait Flyable { def fly(): String }
trait Swimmable { def swim(): String }

object NewDemo {
  // NEW constrApp (WITH constrApp)* (no templateBody)
  class Bird extends Flyable with Swimmable {
    def fly() = "flying"
    def swim() = "swimming"
  }

  // NEW constrApp WITH constrApp templateBody
  val creature = new Flyable with Swimmable {
    def fly() = "creature fly"
    def swim() = "creature swim"
  }

  // NEW templateBody (anonymous class, no extends)
  val anon = new { def hello(): String = "hi" }

  // blockExpr as argumentExprs: method { case ... }
  val mapped: List[String] = List(1, 2, 3).map { case x => x.toString }

  // blockExpr LBRACE caseClauses RBRACE
  val pf: PartialFunction[Int, String] = { case 1 => "one"; case 2 => "two" }

  // namedExprInParens: LPAREN id ASSIGN exprInParens RPAREN
  def namedArgs(x: Int = 0, y: Int = 0): Int = x + y
  val r1 = namedArgs(x = 1, y = 2)
  val r2 = namedArgs(y = 10)

  // LPAREN USING exprsInParens RPAREN
  def usingArgs(n: Int)(using scale: Int): Int = n * scale
  val r3 = usingArgs(5)(using 3)

  // varargs: LPAREN (exprsInParens COMMA)? postfixExpr Op RPAREN
  def sumAll(xs: Int*): Int = xs.sum
  val args = Seq(1, 2, 3)
  val r4 = sumAll(args*)
}
