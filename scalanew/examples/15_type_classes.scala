trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  def apply[A](using m: Monoid[A]): Monoid[A] = m

  given Monoid[Int] with {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  given Monoid[String] with {
    def empty: String = ""
    def combine(x: String, y: String): String = x + y
  }
}

def fold[A](list: List[A])(using m: Monoid[A]): A = {
  list.foldLeft(m.empty)(m.combine)
}

object TypeClassDemo {
  def main(args: Array[String]): Unit = {
    println(fold(List(1, 2, 3, 4, 5)))
    println(fold(List("hello", " ", "world")))
  }
}
