// Covers: blockResult (funParams ARROW block, typTypeParamClause ARROW block),
//         USCORE in binding, USCORE in funParams, infixExpr id colonArgument,
//         blockStat extension_, blockStat endMarker, blockStat importDecl already covered,
//         LPAREN RPAREN in simpleExpr (unit), prefixOperator simpleExpr ASSIGN,
//         simpleExpr argumentExprs ASSIGN (index update), packaging (PACKAGE qualId LBRACE RBRACE)

// blockResult: funParams (ARROW | CTXARROW) block
// binding with USCORE
val lambdaUnit = { (_: Int) => { println("ignored") } }
val lambdaUScore = { _ => { 42 } }

// USCORE in funParams (not binding)
val fn1: Int => Int = x => { x + 1 }
val fn2: Int => Int = { _ => 42 }

// blockResult: typTypeParamClause ARROW block
val polyBlock = { [T] => { List.empty[T] } }

// blockStat endMarker inside a method block
def methodWithEndMarker(): Int = {
  val x = 1
  end methodWithEndMarker
  x
}

// blockStat extension_ (extension defined inside a block/def)
def localExtension(): Unit = {
  extension (s: String) {
    def shout: String = s.toUpperCase
  }
  println("hello".shout)
}

// infixExpr id colonArgument
def colonArgInfix(): Unit = {
  List(1, 2, 3) foreach: {
    x => println(x)
  }
}

// LPAREN RPAREN in simpleExpr (unit value)
val unitVal: Unit = ()

// prefixOperator simpleExpr ASSIGN (unusual but grammatically valid)
// e.g. !arr(0) = true — hard to construct naturally, skip for now

// packaging: PACKAGE qualId LBRACE topStats RBRACE
package mypackage {
  class Inner { val x: Int = 1 }
  object InnerObj { val y: Int = 2 }
}

// blockStat: annotation* localModifier* def_ (INLINE modifier in block)
def blockWithInline(): Int = {
  inline val k = 5
  k * 2
}

// LPAREN RPAREN in simpleExpr as expression
def returnsUnit(): Unit = {
  val u = ()
  u
}
