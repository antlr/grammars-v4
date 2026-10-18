// Covers: blockStat importDecl, constrParamClause usingParamClause,
//         extMethods extMethod (single, no braces), inheritClauses (enum derives),
//         funParamClause/typedFunParam, ascription COLON infixType,
//         givenConditional givenType

// blockStat: importDecl — import statement inside a function body
def usesLocalImport(): String = {
  import scala.collection.mutable
  mutable.ArrayBuffer(1, 2, 3).mkString(", ")
}

// constrParamClause: usingParamClause — secondary constructor with using clause
// (constrParamClauses is used in: def this(...)(using ...) = this(...))
class MultiUsing(val x: Int, val y: Int) {
  def this(n: Int)(using scale: Int) = this(n, n * scale)
}

// extMethods: extMethod (single method without braces — alt 1 of extMethods)
extension (x: Int) def tripled: Int = x * 3

// inheritClauses: (EXTENDS constrApps)? (DERIVES qualId (COMMA qualId)*)? in enumDef
// Enum with derives covers the qualId reference inside inheritClauses
enum Weekday derives CanEqual {
  case Mon, Tue, Wed, Thu, Fri, Sat, Sun
}

// funParamClause / typedFunParam: (x: Int) => String
// funTypeArgs: funParamClause (third alternative, not LPAREN funArgTypes? RPAREN)
type DepFn = (x: Int) => String
val mkStr: (n: Int) => String = (n: Int) => n.toString

// ascription: COLON infixType — type ascription in expression position
// expr1: postfixExpr ascription, ascription: COLON infixType
def ascribeExample(x: Any): Any = x: String

// givenConditional: givenType — unparenthesized type as given conditional
// givenSig: givenConditional ARROW givenSig, where givenConditional = givenType = Int
given intToStr: Int => String = n => n.toString

// infixExpr id ASSIGN expr — op-assign with space between operator and =
// Requires explicit SEMI so ANTLR doesn't greedily consume the next identifier
// as a postfix of the preceding expression (newlines are skipped by lexer).
object OpAssign {
  def demo(): Unit = { var x = 0; x + = 5 }
}

// prefixOperator simpleExpr ASSIGN expr — prefix assignment (L304)
// e.g. -arr(0) = 5 parses as: prefixOp(-) simpleExpr(arr(0)) ASSIGN(=) expr(5)
object PrefixAssign {
  var arr: Array[Int] = Array(1, 2, 3)
  def demo(): Unit = { -arr(0) = 5 }
}
