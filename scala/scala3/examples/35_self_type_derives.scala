// Covers: selfType (id COLON infixType ARROW, THIS COLON infixType ARROW),
//         DERIVES qualId in template, constrExpr, selfInvocation, constrParamClauses,
//         defDef THIS form (secondary constructor), defDef LBRACE block RBRACE form,
//         defDef abstract (no body), templateStat exportDecl, templateStat expr1,
//         templateStat endMarker, extMethod exportDecl

// selfType: id COLON infixType ARROW
trait Logging {
  def log(msg: String): Unit = println(msg)
}

trait Service {
  self: Logging =>
  def run(): Unit = log("running")
}

// selfType: THIS COLON infixType ARROW
trait Auditable {
  this: Service =>
  def audit(): Unit = run()
}

// DERIVES qualId in template
case class Point(x: Int, y: Int) derives CanEqual

// DERIVES multiple
case class Color(r: Int, g: Int, b: Int) derives CanEqual, Ordering

// defDef with LBRACE block RBRACE (old procedure-ish style with explicit type)
class ProcStyle {
  def doWork(x: Int): Unit { println(x) }
}

// Secondary constructor: defDef THIS constrParamClauses ASSIGN constrExpr
// selfInvocation: THIS argumentExprs+
class MyData(val x: Int, val y: String) {
  def this(n: Int) = this(n, n.toString)
  def this() = this(0, "default")
}

// constrExpr with block: LBRACE selfInvocation (SEMI? blockStat)* RBRACE
class MyDataBlock(val x: Int) {
  def this(s: String) = {
    this(s.length)
    println("created from string")
  }
}

// abstract defDef (no body)
abstract class AbstractBase {
  def abstractMethod(x: Int): Int
  def abstractTyped: String
}

// templateStat exportDecl
object ExportingObj {
  val value: Int = 1
  export ExportingObj.value
}

// templateStat expr1 (expression statement in template)
object WithExprStat {
  println("initializing")
  val x = 1
  println("done")
}

// templateStat endMarker
object WithEndMarker {
  def foo(): Int = 1
  end foo
}
end WithEndMarker

// extMethod exportDecl
extension (x: Int) {
  export x.toString
}
