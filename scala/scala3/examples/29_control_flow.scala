// Covers: WHILE (both forms), TRY with catches, TRY with FINALLY, TRY with catches+finally,
//         THROW, RETURN, catches, exprCaseClause, ascription (COLON infixType, COLON annotation+),
//         INLINE? IF expr THEN expr ELSE expr (then form), WHILE expr DO expr

object ControlFlowDemo {

  // while (cond) body
  def whileParen(n: Int): Int = {
    var i = 0
    var sum = 0
    while (i < n) { sum = sum + i; i = i + 1 }
    sum
  }

  // while cond do body
  def whileDo(n: Int): Int = {
    var i = 0
    while i < n do { i = i + 1 }
    i
  }

  // if cond then expr else expr (then form)
  def ifThen(x: Int): String = if x > 0 then "pos" else "neg"

  // try/catch with block catches
  def tryCatch(x: Int): Int = {
    try { 100 / x } catch { case _: ArithmeticException => -1 }
  }

  // try/finally
  def tryFinally(x: Int): Int = {
    try { x + 1 } finally { println("cleanup") }
  }

  // try/catch/finally
  def tryCatchFinally(x: Int): Int = {
    try { 100 / x } catch { case _: ArithmeticException => 0 } finally { println("done") }
  }

  // try with single exprCaseClause
  def tryExprCase(x: Int): Int = {
    try { x } catch case e: Exception => -1
  }

  // throw
  def throwEx(msg: String): Nothing = throw new RuntimeException(msg)

  // return
  def earlyReturn(x: Int): Int = {
    if (x < 0) return 0
    return x
  }

  // ascription: postfixExpr COLON infixType
  def ascType(x: Any): String = (x: String)

  // ascription: postfixExpr COLON annotation+
  def ascAnnot(xs: List[Any]): List[String] = (xs: @unchecked).asInstanceOf[List[String]]
}
