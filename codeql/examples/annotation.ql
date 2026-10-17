private module M {
}

module M1 {
    private int foo() { result = 1 }
    predicate bar = foo/0;
}

abstract class Configuration extends string {
    /** Holds if `source` is a relevant data flow source. */
    abstract predicate isSource(Node source);
}

class ConfigA extends Configuration {
    // provides a concrete definition of `isSource`
    override predicate isSource(Node source) { any() }
  }
  class ConfigB extends ConfigA {
    // doesn't need to override `isSource`, because it inherits it from ConfigA
  }
  deprecated class DataFlowNode extends @dataflownode {
   
  }

  class Element extends int {
    string getName() { result = "" }
    final predicate hasName(string name) { name = this.getName() }
  }

select 1