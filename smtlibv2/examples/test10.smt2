(theory ArraysEx
 :sorts ( (Array 2) )
 :funs ( (par (X Y) (select (Array X Y) X Y))
         (par (X Y) (store (Array X Y) X Y (Array X Y))) )
:notes
"A schematic version of the theory of functional arrays with extensionality."
 :definition
  "For every expanded signature Sigma, the instance of ArraysEx with that
signature is the theory consisting of all Sigma-models that satisfy all axioms of the form below, for all sorts s1, s2 in Sigma:
- (forall ((a (Array s1 s2)) (i s1) (e s2)) (= (select (store a i e) i) e))
- (forall ((a (Array s1 s2)) (i s1) (j s1) (e s2))
(=> (distinct i j) (= (select (store a i e) j) (select a j))))
- (forall ((a (Array s1 s2)) (b (Array s1 s2))) (=>
         (forall ((i s1)) (= (select a i) (select b i))) (= a b))) "
  :values
"For all sorts s1, s2, the values of sort (Array s1 s2) are either abstract or have the form (store a i v) where
- a is value of sort (Array s1 s2),
- i is a value of sort s1, and
- v is a value of sort s2." )