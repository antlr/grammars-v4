signature int operator(int lhs, int rhs);
signature class ExtendsInt extends int;

signature class CanBePrinted {
  string toString();
}
signature module MSig {
  class T;
  predicate restriction(T t);
  default string descr(T t) { result = "default" }
}

module Module implements MSig {
  newtype T = A() or B()

  predicate restriction(T t) { t = A() }
}

signature class NodeSig;

signature module EdgeSig<NodeSig Node> {
  predicate apply(Node src, Node dst);
}

module Reachability<NodeSig Node, EdgeSig<Node> Edge> {
  Node reachableFrom(Node src) {
    Edge::apply+(src, result)
  }
}
