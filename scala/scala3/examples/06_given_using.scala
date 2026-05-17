trait Ordering[A] {
  def compare(x: A, y: A): Int
}

given Ordering[Int] with {
  def compare(x: Int, y: Int): Int = x - y
}

given Ordering[String] with {
  def compare(x: String, y: String): Int = x.compareTo(y)
}

def max[A](x: A, y: A)(using ord: Ordering[A]): A = {
  if (ord.compare(x, y) >= 0) x else y
}

object GivenDemo {
  def main(args: Array[String]): Unit = {
    println(max(3, 7))
    println(max("apple", "banana"))
  }
}
