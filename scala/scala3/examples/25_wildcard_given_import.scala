// Covers: wildCardSelector (GIVEN form), classQualifier, namedSelector AS USCORE

import scala.math.{given}
import scala.collection.mutable.{HashMap as MHM, ArrayList as _}

trait Base {
  def foo(): Int = 1
}

class Derived extends Base {
  override def foo(): Int = super[Base].foo() + 10
}

class MultiBase extends Base {
  def callSuper(): Int = super[Base].foo()
}

object WildcardGivenDemo {
  val d = new Derived
  val r = d.foo()
}
