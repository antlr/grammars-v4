object HOF {
  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def applyTwice[A](f: A => A)(x: A): A = f(f(x))

  def foldr[A, B](f: (A, B) => B)(z: B)(list: List[A]): B = list match {
    case Nil    => z
    case h :: t => f(h, foldr(f)(z)(t))
  }

  def main(args: Array[String]): Unit = {
    val double: Int => Int = _ * 2
    val inc: Int => Int = _ + 1
    val doubleInc = compose(double, inc)
    println(doubleInc(3))

    val add = curry((a: Int, b: Int) => a + b)
    println(add(5)(3))

    println(applyTwice(double)(3))

    val sum = foldr((a: Int, b: Int) => a + b)(0)(List(1, 2, 3, 4, 5))
    println(sum)
  }
}
