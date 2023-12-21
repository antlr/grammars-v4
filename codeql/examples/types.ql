class OneTwoThree extends int {
  OneTwoThree() { // characteristic predicate
    this = 1 or this = 2 or this = 3
  }

  string getAString() { // member predicate
    result = "One, two or three: " + this.toString()
  }

  predicate isEven() { // member predicate
    this = 2
  }
}


class SmallInt extends int {
  SmallInt() { this = [1 .. 10] }
}

class DivisibleInt extends SmallInt {
  SmallInt divisor;   // declaration of the field `divisor`
  DivisibleInt() { this % divisor = 0 }

  SmallInt getADivisor() { result = divisor }
}

class OneTwo extends OneTwoThree {
  OneTwo() {
    this = 1 or this = 2
  }

  override string getAString() {
    result = "One or two: " + this.toString()
  }
}

class TwoThree extends OneTwoThree {
  TwoThree() {
    this = 2 or this = 3
  }

  override string getAString() {
    result = "Two or three: " + this.toString()
  }
}
final class FinalOneTwoThree = OneTwoThree;

class OneTwoFinalExtension extends FinalOneTwoThree {
  OneTwoFinalExtension() {
    this = 1 or this = 2
  }

  string getAString() {
    result = "One or two: " + this.toString()
  }
}


class Interface extends int {
  Interface() { this in [1 .. 10] }
  string foo() { result = "" }
}

class Foo extends int {
  Foo() { this in [1 .. 5] }
  string foo() { result = "foo" }
}

class Bar extends Interface instanceof Foo {
  override string foo() { result = "bar" }
}

newtype T =
  Type1(SmallInt a, SmallInt b) { a = b }
  or
  Type2(SmallInt c)
  or
  Type3()

  private newtype TTaintType =
  TExactValue()
  or
  TTaintedValue()

/** Describes how data is tainted. */
class TaintType extends TTaintType {
  string toString() {
    this = TExactValue() and result = "exact"
    or
    this = TTaintedValue() and result = "tainted"
  }
}

/** A taint type where the data is untainted. */
class Untainted extends TaintType, TExactValue {
}

/** A taint type where the data is tainted. */
class Tainted extends TaintType, TTaintedValue {
}

class  TypeX = Type1 or Type2;



from DivisibleInt i
select i, i.getADivisor(), 1.(OneTwoThree).getAString(), 1.(OneTwoThree).getAString().toUpperCase()