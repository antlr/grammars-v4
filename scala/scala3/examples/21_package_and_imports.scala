package com.example.demo

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.math.*

object WildcardDemo {
  def circleArea(r: Double): Double = Pi * r * r
  def hypotenuse(a: Double, b: Double): Double = sqrt(a * a + b * b)
}

object PackageDemo {
  def main(args: Array[String]): Unit = {
    val buf = new ArrayBuffer[Int]()
    buf += 1
    buf += 2
    buf += 3
    println(buf.toList)

    println(WildcardDemo.circleArea(5.0))
    println(WildcardDemo.hypotenuse(3.0, 4.0))
  }
}
