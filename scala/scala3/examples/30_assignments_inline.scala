// Covers: simpleExpr DOT id ASSIGN expr, simpleExpr argumentExprs ASSIGN expr,
//         infixExpr id ASSIGN expr (op-assign), INLINE infixExpr matchClause,
//         simpleExpr DOT matchClause, colonArgument

class MutableBox(var value: Int) {
  def apply(i: Int): Int = value + i
  def update(i: Int, v: Int): Unit = { value = v }
}

object AssignmentDemo {
  val box = new MutableBox(0)

  def fieldAssign(): Unit = {
    box.value = 42
  }

  def indexAssign(): Unit = {
    val arr = Array(1, 2, 3)
    arr(0) = 99
  }

  def opAssign(): Unit = {
    var x = 10
    x += 5
    x -= 2
    x *= 3
  }

  // inline match expression
  inline def classifyInline(x: Int): String = inline x match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "many"
  }

  // simpleExpr DOT matchClause
  def dotMatch(x: Int): String = x.match {
    case 0 => "zero"
    case _ => "nonzero"
  }

  // colonArgument: method call with colon-block argument
  def colonArg(): Unit = {
    List(1, 2, 3).foreach: {
      x => println(x)
    }
  }

  // colonArgument with caseClauses
  def colonCase(): Unit = {
    List(1, 2, 3).collect: {
      case x if x > 1 => x * 2
    }
  }
}
