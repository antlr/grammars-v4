object CollectionsDemo {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(nums.filter(_ % 2 == 0))
    println(nums.map(_ * 2))
    println(nums.foldLeft(0)(_ + _))
    println(nums.takeWhile(_ < 5))
    println(nums.partition(_ % 2 == 0))

    val scores: Map[String, Int] = Map(
      "Alice" -> 95,
      "Bob"   -> 82,
      "Carol" -> 91
    )
    val passing = scores.filter(_._2 >= 90)
    println(passing)

    val s1 = Set(1, 2, 3, 4, 5)
    val s2 = Set(4, 5, 6, 7, 8)
    println(s1 union s2)
    println(s1 intersect s2)

    val nested = List(List(1, 2), List(3, 4), List(5))
    println(nested.flatten)
    println(nested.flatMap(_.map(_ * 2)))
  }
}
