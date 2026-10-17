// Covers: FOR LPAREN enumerators RPAREN (paren form), FOR enumerators0 DO|YIELD expr (bare form),
//         enumerator pattern1 ASSIGN expr, generator CASE? (CASE generator),
//         pattern2 id AT infixPattern, pattern1 patVar COLON refinedType,
//         pattern1 simpleLiteral COLON refinedType, simplePattern1 DOT id,
//         namedPattern (in patterns), simplePattern LPAREN RPAREN,
//         simplePattern GIVEN refinedType, argumentPatterns varargs

object ForPatternDemo {

  // FOR LPAREN enumerators RPAREN YIELD expr
  val r1 = for (x <- List(1, 2, 3); y = x * 2; if y > 2) yield y

  // FOR LPAREN enumerators RPAREN DO expr (no yield)
  def printAll(): Unit = for (x <- List(1, 2, 3)) { println(x) }

  // FOR enumerators DO expr (bare form, no parens/braces)
  val pairs = List((1, "a"), (2, "b"))
  def bareFor(): Unit = for x <- pairs do println(x)

  // enumerator: pattern1 ASSIGN expr (value definition in for)
  val r2 = for { x <- List(1, 2); y = x + 1 } yield y

  // generator with CASE (CASE pattern1 LARROW expr)
  val r3 = for { case (a, b) <- pairs } yield a + b.length

  // pattern2: id AT infixPattern (binding with @)
  val r4 = List(1, 2, 3).collect {
    case all @ (1 | 2) => all
  }

  // pattern1: patVar COLON refinedType (typed pattern)
  def typedPat(x: Any): String = x match {
    case s: String => s
    case n: Int => n.toString
    case _ => "other"
  }

  // pattern1: simpleLiteral COLON refinedType
  def litTyped(x: Any): String = x match {
    case (1: Int) => "one"
    case _ => "other"
  }

  // simplePattern1 DOT id (qualified constructor pattern)
  object Wrap { case class Box(n: Int) }
  val r5 = Wrap.Box(5) match {
    case Wrap.Box(n) => n
  }

  // simplePattern LPAREN RPAREN (unit pattern)
  def unitPat(x: Unit): String = x match {
    case () => "unit"
  }

  // simplePattern GIVEN refinedType (given pattern)
  def givenPat(x: Any): Unit = x match {
    case given String => println("got string given")
    case _ => ()
  }

  // argumentPatterns varargs: LPAREN (patterns COMMA)? patVar Op RPAREN
  case class Many(xs: Int*)
  val r6 = Many(1, 2, 3) match {
    case Many(head, rest*) => head
  }

  // namedPattern in patterns
  case class Point(x: Int, y: Int)
  val r7 = Point(1, 2) match {
    case Point(x = a, y = b) => a + b
  }
}
