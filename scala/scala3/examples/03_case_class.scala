case class Person(name: String, age: Int)

object CaseClassDemo {
  def describe(p: Person): String = p match {
    case Person(name, age) if age < 18 => s"$name is a minor"
    case Person(name, age) if age >= 65 => s"$name is a senior"
    case Person(name, _) => s"$name is an adult"
  }
}
