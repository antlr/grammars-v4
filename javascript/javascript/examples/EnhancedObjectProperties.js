"use strict";
//------------------------------------------------------------------------------
// Property Shorthand
// Shorter syntax for common object property definition idiom.
// http://es6-features.org/#PropertyShorthand
//------------------------------------------------------------------------------

obj = { x, y }

//------------------------------------------------------------------------------
// Computed Property Names
// Support for computed names in object property definitions.
// http://es6-features.org/#ComputedPropertyNames
//------------------------------------------------------------------------------

let obj = {
    foo: "bar",
    [ "baz" + quux() ]: 42
}

var obj = {
    foo: "bar"
};
obj[ "baz" + quux() ] = 42;

//------------------------------------------------------------------------------
// Method Properties
// Support for method notation in object property definitions, for both regular
//   functions and generator functions.
// http://es6-features.org/#MethodProperties
//------------------------------------------------------------------------------

obj = {
    foo (a, b) {
    },
    *quux (x, y) {
    }
}