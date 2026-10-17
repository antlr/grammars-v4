// Covers: givenDef, givenSig, givenImpl (givenType ASSIGN expr, givenType templateBody,
//         constrApps templateBody), givenConditional (defTypeParamClause, defTermParamClause,
//         LPAREN funArgTypes RPAREN, givenType), givenType, oldGivenSig

// givenDef: id COLON givenSig
// givenSig: givenImpl
// givenImpl: givenType ASSIGN expr
given myInt: Int = 42
given myStr: String = "hello"

// givenDef with templateBody (givenImpl: givenType templateBody)
trait Printable {
  def print(): Unit
}

given printableInt: Printable {
  def print(): Unit = println(myInt)
}

// givenDef: no id (anonymous), just COLON givenSig
given : Boolean = true

// givenSig: givenConditional ARROW givenSig (conditional given)
// givenConditional: defTypeParamClause ([A])
given myListGiven: [A] => List[A] = Nil

// givenConditional: LPAREN funArgTypes RPAREN (parenthesized type)
given myConv: (Int) => String = _.toString

// givenSig: LPAREN RPAREN ARROW givenImpl (unit-conditional given)
given myNullary: () => Int = 99

// oldGivenDef with oldGivenSig: id usingParamClause* COLON structuralInstance
trait Comparable[A] {
  def compare(x: A, y: A): Int
}

given listComp[A](using ord: Comparable[A]): Comparable[List[A]] with {
  def compare(x: List[A], y: List[A]): Int = x.length - y.length
}

// oldGivenDef without oldGivenSig (just structuralInstance)
given Comparable[Int] with {
  def compare(x: Int, y: Int): Int = x - y
}

// givenImpl: constrApps templateBody (given with constrApps and template body)
given myOrd2: Ordering[Int] = Ordering.Int

// givenConditional: defTermParamClause
given myDependent: (n: Int) => String = n => n.toString

// givenType: annotType id annotType (more complex givenType)
given myOrd3: Ordering[String] = Ordering.String
