// Covers: simpleType_ LPAREN RPAREN (unit type), refinement as structural type in simpleType_,
//         INLINE? IF LPAREN (inline if paren form), constrExpr LBRACE selfInvocation blockStats RBRACE,
//         blockStat annotation* localModifier* def_, defDef abstract decl,
//         singleton as simpleLiteral, exprInParens postfixExpr COLON type_,
//         pattern USCORE in funParams, USCORE in patVar

// simpleType_ : LPAREN RPAREN (unit type in type position)
type UnitAlias = ()
def returnsUnitType(): () = ()

// simpleType_ : refinement (structural type directly, no annotType prefix)
def acceptStructural(x: { def foo(): Int }): Int = x.foo()

// singleton as simpleLiteral (literal type used as singleton)
// e.g. a val with literal type
val zeroLit: 0 = 0
val trueLit: true = true

// INLINE? IF LPAREN ... RPAREN form (inline if)
inline def inlineIf(x: Int): String = inline if (x > 0) "pos" else "neg"

// constrExpr: LBRACE selfInvocation (SEMI? blockStat)* RBRACE
class MultiCons(val x: Int, val y: Int) {
  def this(n: Int) = {
    this(n, n * 2)
    println("two-arg constructor via block")
  }
}

// defDef abstract (defSig COLON type_? with no body - abstract method in abstract class)
abstract class Abstracted {
  def computeValue(n: Int): Int           // abstract, no body
  def describeValue: String               // abstract, no body
  def complexAbstract(x: Int, y: String): Boolean  // abstract
}

// exprInParens: postfixExpr COLON type_ (type ascription inside parens)
def ascriptionInParens(x: Any): Int = (x: Int)

// blockStat: annotation* localModifier* def_ (with annotation in block)
def blockWithAnnotation(): Unit = {
  @deprecated("old", "1.0") def oldHelper(): Int = 42
  oldHelper()
}

// INLINE modifier in local def (blockStat)
def blockWithInlineLocal(): Int = {
  inline def helper(x: Int): Int = x * 2
  helper(21)
}

// patVar USCORE in various positions
def matchUscore(x: Any): String = x match {
  case _: Int => "int"
  case _ => "other"
}

// CASE? in generator (without CASE — already tested in 32; now also CASE form)
val pairs2 = List(Some(1), None, Some(2))
val r = for { case Some(n) <- pairs2 } yield n

// importExpr: LBRACE importSelectors RBRACE with multiple selectors
import scala.collection.mutable.{ArrayBuffer, LinkedList as LL}

// wildCardSelector Op (the '*' form in an importSelectors context)
// Note: namedSelector via id catches '*' first if listed first;
// but in importSelectors second alternative: wildCardSelector (COMMA wildCardSelector)*
// Try to reach the wildCardSelector via importSelectors second alternative
// by using: import foo.{given, given SomeType}
import scala.math.{given}
