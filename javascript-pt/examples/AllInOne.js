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

// Parameter Context Matching
// http://es6-features.org/#ParameterContextMatching

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

// Fail-Soft Destructuring
// http://es6-features.org/#FailSoftDestructuring

var list = [ 7, 42 ]
var [ a = 1, b = 2, c = 3, d ] = list
a === 7
b === 42
c === 3
d === undefined

//--------------------------
// Modules
//--------------------------

// Value Export/Import
// http://es6-features.org/#ValueExportImport

//  lib/math.js
export function sum (x, y) { return x + y }
export var pi = 3.141593

//  someApp.js
// TODO: import * as math from "lib/math"
console.log("2π = " + math.sum(math.pi, math.pi))

//  otherApp.js
// TODO: import { sum, pi } from "lib/math"
console.log("2π = " + sum(pi, pi))

// Default & Wildcard
// http://es6-features.org/#DefaultWildcard

//  lib/mathplusplus.js
// TODO: export * from "lib/math"
export var e = 2.71828182846
// TODO: export default (x) => Math.exp(x)

//  someApp.js
// TODO: import exp, { pi, e } from "lib/mathplusplus"
console.log("e^{π} = " + exp(pi))

//--------------------------
// Classes
//--------------------------

// Class Definition
// http://es6-features.org/#ClassDefinition

class Shape {
    constructor (id, x, y) {
        this.id = id
        this.move(x, y)
    }
    move (x, y) {
        this.x = x
        this.y = y
    }
}

// Class Inheritance
// http://es6-features.org/#ClassInheritance

class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
        super(id, x, y)
        this.width  = width
        this.height = height
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
        super(id, x, y)
        this.radius = radius
    }
}

// Class Inheritance, From Expressions
// http://es6-features.org/#ClassInheritanceFromExpressions

var aggregation = (baseClass, ...mixins) => {
    let base = class _Combined extends baseClass {
        constructor (...args) {
            super(...args)
            mixins.forEach((mixin) => {
                mixin.prototype.initializer.call(this)
            })
        }
    }
    let copyProps = (target, source) => {
        Object.getOwnPropertyNames(source)
            .concat(Object.getOwnPropertySymbols(source))
            .forEach((prop) => {
            if (prop.match(/^(?:constructor|prototype|arguments|caller|name|bind|call|apply|toString|length)$/))
                return
            Object.defineProperty(target, prop, Object.getOwnPropertyDescriptor(source, prop))
        })
    }
    mixins.forEach((mixin) => {
        copyProps(base.prototype, mixin.prototype)
        copyProps(base, mixin)
    })
    return base
}

class Colored {
    initializer ()     { this._color = "white" }
    get color ()       { return this._color }
    set color (v)      { this._color = v }
}

class ZCoord {
    initializer ()     { this._z = 0 }
    get z ()           { return this._z }
    set z (v)          { this._z = v }
}

class Shape {
    constructor (x, y) { this._x = x; this._y = y }
    get x ()           { return this._x }
    set x (v)          { this._x = v }
    get y ()           { return this._y }
    set y (v)          { this._y = v }
}

class Rectangle extends aggregation(Shape, Colored, ZCoord) {}

var rect = new Rectangle(7, 42)
rect.z     = 1000
rect.color = "red"
console.log(rect.x, rect.y, rect.z, rect.color)

// Base Class Access
// http://es6-features.org/#BaseClassAccess

class Shape {
    // …
    toString () {
        return `Shape(${this.id})`
    }
}
class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
        super(id, x, y)
        // …
    }
    toString () {
        return "Rectangle > " + super.toString()
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
        super(id, x, y)
        // …
    }
    toString () {
        return "Circle > " + super.toString()
    }
}

// Static Members
// http://es6-features.org/#StaticMembers

class Rectangle extends Shape {
    // …
    static defaultRectangle () {
        return new Rectangle("default", 0, 0, 100, 100)
    }
}
class Circle extends Shape {
    // …
    static defaultCircle () {
        return new Circle("default", 0, 0, 100)
    }
}
var defRectangle = Rectangle.defaultRectangle()
var defCircle    = Circle.defaultCircle()

// Getter/Setter
// http://es6-features.org/#GetterSetter

class Rectangle {
    constructor (width, height) {
        this._width  = width
        this._height = height
    }
    set width  (width)  { this._width = width               }
    get width  ()       { return this._width                }
    set height (height) { this._height = height             }
    get height ()       { return this._height               }
    get area   ()       { return this._width * this._height }
}
var r = new Rectangle(50, 20)
r.area === 1000

//--------------------------
// Symbol Type
//--------------------------

// Symbol Type
// http://es6-features.org/#SymbolType

Symbol("foo") !== Symbol("foo")
const foo = Symbol()
const bar = Symbol()
typeof foo === "symbol"
typeof bar === "symbol"
let obj = {}
obj[foo] = "foo"
obj[bar] = "bar"
JSON.stringify(obj) // {}
Object.keys(obj) // []
Object.getOwnPropertyNames(obj) // []
Object.getOwnPropertySymbols(obj) // [ foo, bar ]

// Global Symbols
// http://es6-features.org/#GlobalSymbols

Symbol.for("app.foo") === Symbol.for("app.foo")
const foo = Symbol.for("app.foo")
const bar = Symbol.for("app.bar")
Symbol.keyFor(foo) === "app.foo"
Symbol.keyFor(bar) === "app.bar"
typeof foo === "symbol"
typeof bar === "symbol"
let obj = {}
obj[foo] = "foo"
obj[bar] = "bar"
JSON.stringify(obj) // {}
Object.keys(obj) // []
Object.getOwnPropertyNames(obj) // []
Object.getOwnPropertySymbols(obj) // [ foo, bar ]

//--------------------------
// TODO: Iterators
//--------------------------

// Iterator & For-Of Operator
// http://es6-features.org/#IteratorForOfOperator

/*let fibonacci = {
    [Symbol.iterator]() {
        let pre = 0, cur = 1
        return {
           next () {
               [ pre, cur ] = [ cur, pre + cur ]
               return { done: false, value: cur }
           }
        }
    }
}

for (let n of fibonacci) {
    if (n > 1000)
        break
    console.log(n)
}*/

//--------------------------
// TODO: Generators
//--------------------------

// Generator Function, Iterator Protocol
// http://es6-features.org/#GeneratorFunctionIteratorProtocol

/*let fibonacci = {
    *[Symbol.iterator]() {
        let pre = 0, cur = 1
        for (;;) {
            [ pre, cur ] = [ cur, pre + cur ]
            yield cur
        }
    }
}

for (let n of fibonacci) {
    if (n > 1000)
        break
    console.log(n)
}

// Generator Function, Direct Use
// http://es6-features.org/#GeneratorFunctionDirectUse

function* range (start, end, step) {
    while (start < end) {
        yield start
        start += step
    }
}

for (let i of range(0, 10, 2)) {
    console.log(i) // 0, 2, 4, 6, 8
}

// Generator Matching
// http://es6-features.org/#GeneratorMatching

let fibonacci = function* (numbers) {
    let pre = 0, cur = 1
    while (numbers-- > 0) {
        [ pre, cur ] = [ cur, pre + cur ]
        yield cur
    }
}

for (let n of fibonacci(1000))
    console.log(n)

let numbers = [ ...fibonacci(1000) ]

let [ n1, n2, n3, ...others ] = fibonacci(1000)

// Generator Control-Flow
// http://es6-features.org/#GeneratorControlFlow

//  generic asynchronous control-flow driver
function async (proc, ...params) {
    var iterator = proc(...params)
    return new Promise((resolve, reject) => {
        let loop = (value) => {
            let result
            try {
                result = iterator.next(value)
            }
            catch (err) {
                reject(err)
            }
            if (result.done)
                resolve(result.value)
            else if (   typeof result.value      === "object"
                     && typeof result.value.then === "function")
                result.value.then((value) => {
                    loop(value)
                }, (err) => {
                    reject(err)
                })
            else
                loop(result.value)
        }
        loop()
    })
}

//  application-specific asynchronous builder
function makeAsync (text, after) {
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve(text), after)
    })
}

//  application-specific asynchronous procedure
async(function* (greeting) {
    let foo = yield makeAsync("foo", 300)
    let bar = yield makeAsync("bar", 200)
    let baz = yield makeAsync("baz", 100)
    return `${greeting} ${foo} ${bar} ${baz}`
}, "Hello").then((msg) => {
    console.log("RESULT:", msg) // "Hello foo bar baz"
})

// Generator Methods
// http://es6-features.org/#GeneratorMethods

class Clz {
    * bar () {
        // …
    }
}
let Obj = {
    * foo () {
        // …
    }
}*/

//--------------------------
// Map/Set & WeakMap/WeakSet
//--------------------------

// Set Data-Structure
// http://es6-features.org/#SetDataStructure

let s = new Set()
s.add("hello").add("goodbye").add("hello")
s.size === 2
s.has("hello") === true
for (let key of s.values()) // insertion order
    console.log(key)

// Map Data-Structure
// http://es6-features.org/#MapDataStructure

let m = new Map()
let s = Symbol()
m.set("hello", 42)
m.set(s, 34)
m.get(s) === 34
m.size === 2
for (let [ key, val ] of m.entries())
    console.log(key + " = " + val)

// Weak-Link Data-Structures
// http://es6-features.org/#WeakLinkDataStructures

let isMarked     = new WeakSet()
let attachedData = new WeakMap()

export class Node {
    constructor (id)   { this.id = id                  }
    mark        ()     { isMarked.add(this)            }
    unmark      ()     { isMarked.delete(this)         }
    marked      ()     { return isMarked.has(this)     }
    set data    (data) { attachedData.set(this, data)  }
    get data    ()     { return attachedData.get(this) }
}

let foo = new Node("foo")

JSON.stringify(foo) === '{"id":"foo"}'
foo.mark()
foo.data = "bar"
foo.data === "bar"
JSON.stringify(foo) === '{"id":"foo"}'

isMarked.has(foo)     === true
attachedData.has(foo) === true
foo = null  /* remove only reference to foo */
attachedData.has(foo) === false
isMarked.has(foo)     === false

//--------------------------
// Typed Arrays
//--------------------------

// Typed Arrays
// http://es6-features.org/#TypedArrays

//  ES6 class equivalent to the following C structure:
//  struct Example { unsigned long id; char username[16]; float amountDue }
class Example {
    constructor (buffer = new ArrayBuffer(24)) {
        this.buffer = buffer
    }
    set buffer (buffer) {
        this._buffer    = buffer
        this._id        = new Uint32Array (this._buffer,  0,  1)
        this._username  = new Uint8Array  (this._buffer,  4, 16)
        this._amountDue = new Float32Array(this._buffer, 20,  1)
    }
    get buffer ()     { return this._buffer       }
    set id (v)        { this._id[0] = v           }
    get id ()         { return this._id[0]        }
    set username (v)  { this._username[0] = v     }
    get username ()   { return this._username[0]  }
    set amountDue (v) { this._amountDue[0] = v    }
    get amountDue ()  { return this._amountDue[0] }
}

let example = new Example()
example.id = 7
example.username = "John Doe"
example.amountDue = 42.0

//--------------------------
// New Built-In Methods
//--------------------------

// Object Property Assignment
// http://es6-features.org/#ObjectPropertyAssignment

var dest = { quux: 0 }
var src1 = { foo: 1, bar: 2 }
var src2 = { foo: 3, baz: 4 }
Object.assign(dest, src1, src2)
dest.quux === 0
dest.foo  === 3
dest.bar  === 2
dest.baz  === 4

// Array Element Finding
// http://es6-features.org/#ArrayElementFinding

[ 1, 3, 4, 2 ].find(x => x > 3) // 4
[ 1, 3, 4, 2 ].findIndex(x => x > 3) // 2

// String Repeating
// http://es6-features.org/#StringRepeating

" ".repeat(4 * depth)
"foo".repeat(3)

// String Searching
// http://es6-features.org/#StringSearching

"hello".startsWith("ello", 1) // true
"hello".endsWith("hell", 4)   // true
"hello".includes("ell")       // true
"hello".includes("ell", 1)    // true
"hello".includes("ell", 2)    // false

// Number Type Checking
// http://es6-features.org/#NumberTypeChecking

Number.isNaN(42) === false
Number.isNaN(NaN) === true

Number.isFinite(Infinity) === false
Number.isFinite(-Infinity) === false
Number.isFinite(NaN) === false
Number.isFinite(123) === true

// Number Safety Checking
// http://es6-features.org/#NumberSafetyChecking

Number.isSafeInteger(42) === true
Number.isSafeInteger(9007199254740992) === false

// Number Comparison
// http://es6-features.org/#NumberComparison

console.log(0.1 + 0.2 === 0.3) // false
console.log(Math.abs((0.1 + 0.2) - 0.3) < Number.EPSILON) // true

// Number Truncation
// http://es6-features.org/#NumberTruncation

console.log(Math.trunc(42.7)) // 42
console.log(Math.trunc( 0.1)) // 0
console.log(Math.trunc(-0.1)) // -0

// Number Sign Determination
// http://es6-features.org/#NumberSignDetermination

console.log(Math.sign(7))   // 1
console.log(Math.sign(0))   // 0
console.log(Math.sign(-0))  // -0
console.log(Math.sign(-7))  // -1
console.log(Math.sign(NaN)) // NaN

//--------------------------
// Promise Usage
//--------------------------

// Promise Usage
// http://es6-features.org/#PromiseUsage

function msgAfterTimeout (msg, who, timeout) {
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve(`${msg} Hello ${who}!`), timeout)
    })
}
msgAfterTimeout("", "Foo", 100).then((msg) =>
    msgAfterTimeout(msg, "Bar", 200)
).then((msg) => {
    console.log(`done after 300ms:${msg}`)
})

// Promise Combination
// http://es6-features.org/#PromiseCombination

function fetchAsync (url, timeout, onData, onError) {
    // …
}
let fetchPromised = (url, timeout) => {
    return new Promise((resolve, reject) => {
        fetchAsync(url, timeout, resolve, reject)
    })
}
Promise.all([
    fetchPromised("http://backend/foo.txt", 500),
    fetchPromised("http://backend/bar.txt", 500),
    fetchPromised("http://backend/baz.txt", 500)
]).then((data) => {
    let [ foo, bar, baz ] = data
    console.log(`success: foo=${foo} bar=${bar} baz=${baz}`)
}, (err) => {
    console.log(`error: ${err}`)
})