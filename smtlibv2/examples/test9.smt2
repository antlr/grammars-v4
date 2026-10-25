(theory Ints
 :sorts ( (Int 0) )
 :funs ( (NUMERAL Int)
(- Int Int) ; negation
(- Int Int Int :left-assoc) ; subtraction (+ Int Int Int :left-assoc)
(* Int Int Int :left-assoc)
(<= Int Int Bool :chainable)
(< Int Int Bool :chainable)
(>= Int Int Bool :chainable)
(> Int Int Bool :chainable) )
 :definition
 "For every expanded signature Sigma, the instance of Ints with that
signature is the theory consisting of all Sigma-models that interpret - the sort Int as the set of all integers,
- the function symbols of Ints as expected. "
:values
"The Int values are all the numerals and all the terms of the form (- n)
where n is a non-zero numeral." )