"use strict";
//------------------------------------------------------------------------------
// Default Parameter Values
// Simple and intuitive default values for function parameters.
// http://es6-features.org/#DefaultParameterValues
//------------------------------------------------------------------------------

function f (x, y = 7, z = 42) {
    return x + y + z
}
f(1) === 50

//------------------------------------------------------------------------------
// Rest Parameter
// Aggregation of remaining arguments into single parameter of variadic functions.
// http://es6-features.org/#RestParameter
//------------------------------------------------------------------------------

function f (x, y, ...a) {
    return (x + y) * a.length
}
f(1, 2, "hello", true, 7) === 9

//------------------------------------------------------------------------------
// Spread Operator
// Spreading of elements of an iterable collection (like an array or even a string)
//   into both literal elements and individual function parameters.
// http://es6-features.org/#SpreadOperator
//------------------------------------------------------------------------------

var params = [ "hello", true, 7 ]
var other = [ 1, 2, ...params ] // [ 1, 2, "hello", true, 7 ]

function f (x, y, ...a) {
    return (x + y) * a.length
}
f(1, 2, ...params) === 9

var str = "foo"
var chars = [ ...str ] // [ "f", "o", "o" ]