extension (s: String) {
  def isPalindrome: Boolean = s == s.reverse
  def wordCount: Int = s.split("\\s+").length
}

extension (n: Int) {
  def isEven: Boolean = n % 2 == 0
  def isOdd: Boolean = !n.isEven
}

extension [A](list: List[A]) {
  def second: Option[A] = list.drop(1).headOption
}

object ExtensionDemo {
  def main(args: Array[String]): Unit = {
    println("racecar".isPalindrome)
    println("hello world".wordCount)
    println(List(1, 2, 3).second)
  }
}
