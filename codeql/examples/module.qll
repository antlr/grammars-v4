module Example {
  class OneTwoThree extends int {
    OneTwoThree() {
      this = 1 or this = 2 or this = 3
    }
  }


}

bindingset[result] bindingset[x]
int increment(int x) { result = x + 1 }

module IncrementTwice = M<increment/1, increment/1>;

bindingset[x] signature int transformer(int x);

module M<transformer/1 first, transformer/1 second> {
  bindingset[x]
  int applyBoth(int x) {
    result = second(first(x))
  }
}

bindingset[this]
signature class TSig;

module M1<TSig T> {
  newtype A = B() or C()
}

string foo(M1<int>::A a) { result  = "1" }

predicate test() {
  foo(M1<int>::B()) = ""   // valid: repeated identical instantiation of M does not duplicate A, B, C
}
