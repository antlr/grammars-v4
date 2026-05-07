// Covers: localModifier (ABSTRACT, FINAL, OPEN, IMPLICIT, LAZY, TRANSPARENT, INFIX),
//         accessModifier PROTECTED, accessQualifier, OPAQUE (already tested? testing again),
//         defImplicitClause (LPAREN IMPLICIT defTermParams RPAREN),
//         LPAREN IMPLICIT clsParams RPAREN (clsParamClauses),
//         LPAREN USING (clsParams | funArgTypes) RPAREN (clsParamClause),
//         hkTypeParamClause in clsTypeParam

// ABSTRACT class
abstract class Shape {
  def area(): Double
}

// FINAL class
final class Circle(val radius: Double) extends Shape {
  def area(): Double = 3.14159 * radius * radius
}

// OPEN class (Scala 3)
open class Vehicle(val speed: Int)

// SEALED trait/class
sealed trait Color
case object Red extends Color
case object Blue extends Color

// IMPLICIT class (Scala 2 compat)
implicit class StringOps(val s: String) {
  def shout: String = s.toUpperCase + "!"
}

// LAZY modifier
object LazyDemo {
  lazy val expensive: Int = { println("computed"); 42 }
}

// TRANSPARENT inline
transparent inline def tInline(x: Int): Int = x + 1

// INFIX def
infix def combine(x: Int, y: Int): Int = x + y

// PROTECTED accessModifier
class ProtectedDemo {
  protected val secret: Int = 42
  protected def inner(): Int = secret
}

// accessQualifier: private[pkg]
class QualifiedAccess {
  private[QualifiedAccess] val x: Int = 1
  protected[QualifiedAccess] val y: Int = 2
}

// defImplicitClause: (implicit params) at end of defDef
def withImplicit(x: Int)(implicit n: Int): Int = x + n

// hkTypeParamClause in clsTypeParam: class F[G[_]]
class HigherKinded[F[_]](val fa: F[Int])
class HK2[M[_, _]](val m: M[Int, String])

// LPAREN USING (clsParams | funArgTypes) RPAREN in clsParamClause
class WithUsing(val x: Int)(using val scale: Int) {
  def scaled: Int = x * scale
}

// LPAREN USING funArgTypes RPAREN
class WithUsingTypes(val x: Int)(using Int, String)

// LPAREN IMPLICIT clsParams RPAREN (old-style implicit constructor param)
class OldImplicit(val x: Int)(implicit val ev: Ordering[Int])
