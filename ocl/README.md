# OCL Antlr Grammar

Based on the OCL 2.4 definition https://www.omg.org/spec/OCL/2.4/PDF but with a more consistent use of the arrow operators ->op: these are almost always used for operations on non-primitive types (strings, collections, functions, maps). Thus str.isMatch(patt) is written as str->isMatch(patt) for strings str, patt. 

One exception to this rule is string addition: s1 + s2

Boolean and numeric operators are written with infix or prefix notation as usual, thus: 

a xor b

a * b

m div n

etc. 


## Main contributors

* Kevin Lano, 2022

