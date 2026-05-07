trait Animal {
  def name: String
  def sound: String
  def describe: String = s"$name says $sound"
}

trait Domestic extends Animal {
  def owner: String
  override def describe: String = s"${super.describe} (owned by $owner)"
}

class Dog(val name: String, val owner: String) extends Animal with Domestic {
  val sound: String = "Woof"
}

class Wolf(val name: String) extends Animal {
  val sound: String = "Howl"
}
