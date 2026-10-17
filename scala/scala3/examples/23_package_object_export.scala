// Covers: packageObject, exportDecl, importExpr (simpleRef AS id), namedSelector AS USCORE

package com.example.mylib

export com.example.mylib.utils.Helper
export com.example.mylib.utils.{StringHelper, IntHelper}

import com as c

package object utils {
  val version: String = "1.0"
  def greet(name: String): String = "Hello, " + name
}

object Helper {
  def help(): String = "helping"
}

object StringHelper {
  def process(s: String): String = s.trim
}

object IntHelper {
  def process(n: Int): Int = n + 1
}
