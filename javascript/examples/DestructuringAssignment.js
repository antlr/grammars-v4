"use strict";
//------------------------------------------------------------------------------
// Array Matching
// Intuitive and flexible destructuring of Arrays into individual variables during assignment.
// http://es6-features.org/#ArrayMatching
//------------------------------------------------------------------------------

var list = [ 1, 2, 3 ]
var [ a, , b ] = list
[ b, a ] = [ a, b ]

//------------------------------------------------------------------------------
// Object Matching, Shorthand Notation
// Intuitive and flexible destructuring of Objects into individual variables during assignment.
// http://es6-features.org/#ObjectMatchingShorthandNotation
//------------------------------------------------------------------------------

var { op, lhs, rhs } = getASTNode()

//------------------------------------------------------------------------------
// Object Matching, Deep Matching
// Intuitive and flexible destructuring of Objects into individual variables during assignment.
// http://es6-features.org/#ObjectMatchingDeepMatching
//------------------------------------------------------------------------------

var { op: a, lhs: { op: b }, rhs: c } = getASTNode()

//------------------------------------------------------------------------------
// Object And Array Matching, Default Values
// Simple and intuitive default values for destructuring of Objects and Arrays.
// http://es6-features.org/#ObjectAndArrayMatchingDefaultValues
//------------------------------------------------------------------------------

var obj = { a: 1 }
var list = [ 1 ]
var { a, b = 2 } = obj
var [ x, y = 2 ] = list

//------------------------------------------------------------------------------
// Parameter Context Matching
// Intuitive and flexible destructuring of Arrays and Objects into individual
//   parameters during function calls.
// http://es6-features.org/#ParameterContextMatching
//------------------------------------------------------------------------------

function f ([ name, val ]) {
    console.log(name, val)
}
function g ({ name: n, val: v }) {
    console.log(n, v)
}
function h ({ name, val }) {
    console.log(name, val)
}
f([ "bar", 42 ])
g({ name: "foo", val:  7 })
h({ name: "bar", val: 42 })

//------------------------------------------------------------------------------
// Fail-Soft Destructuring
// Fail-soft destructuring, optionally with defaults.
// http://es6-features.org/#FailSoftDestructuring
//------------------------------------------------------------------------------

var list = [ 7, 42 ]
var [ a = 1, b = 2, c = 3, d ] = list
a === 7
b === 42
c === 3
d === undefined