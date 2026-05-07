// Covers: quoted (QUOTE LBRACE block RBRACE, QUOTE LBRACKET type_ RBRACKET),
//         blockResult funParams CTXARROW block (?=>),
//         funTypeArgs LPAREN funArgTypes? RPAREN (empty paren => type)

// quoted: '{ block }
object QuotedDemo {
  // quoted expression (macro splice syntax)
  def makeQuote(using scala.quoted.Quotes): scala.quoted.Expr[Int] = '{ 1 + 2 }

  // quoted type: '[ Type ]
  def makeTypeQuote(using scala.quoted.Quotes): scala.quoted.Type[Int] = '[Int]

  // blockResult with CTXARROW (?=>): context function
  val ctxFun: Int ?=> String = { x ?=> x.toString }

  // Another context function via ?=>
  type CtxFn = Int ?=> Boolean
  val ctxBool: CtxFn = { n ?=> n > 0 }

  // funTypeArgs: LPAREN RPAREN ARROW type_ (unit -> type, LPAREN funArgTypes? RPAREN)
  // This forces LPAREN funArgTypes? RPAREN path (empty funArgTypes)
  type NullaryFn = () => Int

  // CTXARROW in type (context function type)
  type CtxType = Int ?=> String
}
