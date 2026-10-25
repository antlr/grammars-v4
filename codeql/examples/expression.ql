select 0,42,-2048,2.0,123.456,-100.5
,"hello"
,"They said, \"Please escape quotation marks!\""
,(1)
, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]


class A extends int {
    A() { this = 1 }
    int getANumber() { result = 2 }
  }

  class B extends int {
    B() { this = 1 }
    int getANumber() { result = 3 }
  }

  class C extends A, B {
    // Need to define `int getANumber()`; otherwise it would be ambiguous
    int getANumber() {
      result = B.super.getANumber()
    }

    predicate test() {
        count(File f | f.getTotalNumberOfLines() > 500 | f) = 1
        and max(File f | f.getExtension() = "js" | f.getBaseName() order by f.getTotalNumberOfLines(), f.getNumberOfLinesOfCode()) = 1
        and min(string s | s = "Tarski" or s = "Dedekind" or s = "De Morgan" | s) = ""
        and avg(int i | i = [0 .. 3] | i)  = 1
        and sum(int i, int j | i = [0 .. 2] and j = [3 .. 5] | i * j) =  1
        and concat(int i | i = [0 .. 3] | i.toString() order by i desc) = ""
        and concat(int i | i = [0 .. 3] | i.toString(), "|") = ""
        and rank[4](int i | i = [5 .. 15] | i) = 1
    }
  }
