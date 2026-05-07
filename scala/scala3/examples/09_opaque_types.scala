object Lengths {
  opaque type Meters = Double

  object Meters {
    def apply(d: Double): Meters = d
    extension (m: Meters) {
      def value: Double = m
      def +(other: Meters): Meters = m + other
    }
  }
}

import Lengths.*

object OpaqueDemo {
  def main(args: Array[String]): Unit = {
    val d1 = Meters(5.0)
    val d2 = Meters(3.0)
    val total = d1 + d2
    println(total.value)
  }
}
