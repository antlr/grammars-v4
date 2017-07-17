"use strict";
// CData sample

<!--//--><![CDATA[//><!--
//--><!]]>

// Arrow functions
// https://strongloop.com/strongblog/an-introduction-to-javascript-es6-arrow-functions/

var func = function (parameters) { return expression; }
var func = () => { return 1; }
var func = () => 1

var func = function (a) { return a * 2; }
var func = (a) => { return a * 2; }
var func = (a) => a * 2
var func = a => a * 2

var func = function (a, b) { return a * b; }
var func = (a, b) => { return a * b; }
var func = (a, b) => a * b

var func = function () { return arguments[0]; }
var func = (...args) => args[0]

// Template strings
// TODO: implement interpolation expressions parsing.
// https://docs.microsoft.com/en-us/scripting/javascript/advanced/template-strings-javascript
// http://es6-features.org/#StringInterpolation

var url = buildURL`http://msdn.microsoft.com/${lang}/${a}/${b}`;
var greeting = `\`Yo\` World!`;
console.log(html`<b>${username} says</b>: "${tag}"`);

// var, let, const
// http://es6-features.org/#Constants

var c = 10;
{
    const c = 2; // At this point, c = 2.
} // At this point, c = 10.

const name = "Thomas Jefferson";
const answer = 42, numpages = 10;
const myarray = new Array();

function foo () {
    typeof bar;
    let bar = 'baz';
}
foo();

if (x) {
  let foo;
}

//--------------------------
// Scoping
//--------------------------

// Block-scoped variables (and constants) without hoisting.
// http://es6-features.org/#BlockScopedVariables

for (let i = 0; i < a.length; i++) {
    let x = a[i]
}
for (let i = 0; i < b.length; i++) {
    let y = b[i]
}

let callbacks = []
for (let i = 0; i <= 2; i++) {
    callbacks[i] = function () { return i * 2 }
}
callbacks[0]() === 0
callbacks[1]() === 2
callbacks[2]() === 4

// Block-Scoped Functions
// http://es6-features.org/#BlockScopedFunctions

function foo () { return 1 }
foo() === 1
{
    function foo () { return 2 }
    foo() === 2
}

// Default Parameter Values
// http://es6-features.org/#DefaultParameterValues

function f (x, y = 7) {
    return x + y
}

// Rest Parameter
// http://es6-features.org/#RestParameter

function f (x, y, ...a) {
    return (x + y) * a.length
}
f(1, 2, "hello", true, 7) === 9

// Spread Operator
// http://es6-features.org/#SpreadOperator

var params = [ "hello", true, 7 ]
var other = [ 1, 2, ...params ] // [ 1, 2, "hello", true, 7 ]

function f (x, y, ...a) {
    return (x + y) * a.length
}
f(1, 2, ...params) === 9

var str = "foo"
var chars = [ ...str ] // [ "f", "o", "o" ]

// Raw String Access
// TODO: implement

// Binary & Octal Literal
// http://es6-features.org/#BinaryOctalLiteral

0b111110111 === 503
0o767 === 503

//--------------------------
// Enchanced Regular Expression
//--------------------------

// Unicode String & RegExp Literal
// http://es6-features.org/#UnicodeStringRegExpLiteral

"𠮷".length === 2
"𠮷".match(/./u)[0].length === 2
"𠮷" === "\uD842\uDFB7"
"𠮷" === "\u{20BB7}"
"𠮷".codePointAt(0) == 0x20BB7
// for (let codepoint of "𠮷") console.log(codepoint) TODO: fix

//--------------------------
// Enchanced Object Properties
//--------------------------

// Property Shorthand
// http://es6-features.org/#PropertyShorthand

obj = { x, y }

// Computed Property Names
// http://es6-features.org/#ComputedPropertyNames

let obj = {
    foo: "bar",
    [ "baz" + quux() ]: 42
}

var obj = {
    foo: "bar"
};
obj[ "baz" + quux() ] = 42;

// Method Properties
// http://es6-features.org/#MethodProperties

obj = {
    foo (a, b) {
    },
    *quux (x, y) {
    }
}

//--------------------------
// Destructuring Assignment
//--------------------------

// Array Matching
// http://es6-features.org/#ArrayMatching

var list = [ 1, 2, 3 ]
var [ a, , b ] = list
[ b, a ] = [ a, b ]

// Object Matching, Shorthand Notation
// http://es6-features.org/#ObjectMatchingShorthandNotation

var { op, lhs, rhs } = getASTNode()

// Object Matching, Deep Matching
// http://es6-features.org/#ObjectMatchingDeepMatching

var { op: a, lhs: { op: b }, rhs: c } = getASTNode()

// Object And Array Matching, Default Values
// http://es6-features.org/#ObjectAndArrayMatchingDefaultValues

var obj = { a: 1 }
var list = [ 1 ]
var { a, b = 2 } = obj
var [ x, y = 2 ] = list
