class Counter private (val count: Int) {
  def increment: Counter = new Counter(count + 1)
  def decrement: Counter = new Counter(if (count > 0) count - 1 else 0)
  def reset: Counter = Counter.zero
  override def toString: String = s"Counter($count)"
}

object Counter {
  val zero: Counter = new Counter(0)
  def apply(start: Int): Counter = new Counter(math.max(0, start))
  def unapply(c: Counter): Option[Int] = Some(c.count)
}

sealed trait Status
case object Active   extends Status
case object Inactive extends Status
case object Pending  extends Status

object MathUtils {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)
  def isPrime(n: Int): Boolean = n > 1 && (2 to math.sqrt(n).toInt).forall(n % _ != 0)
}

object CompanionDemo {
  def main(args: Array[String]): Unit = {
    val c = Counter(5)
    val c2 = c.increment.increment
    println(c2)
    val Counter(n) = c2
    println(n)
    println(MathUtils.gcd(12, 8))
    println(MathUtils.lcm(4, 6))
  }
}
