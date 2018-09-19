"use strict";
//------------------------------------------------------------------------------
// Block-Scoped Variables
// Block-scoped variables (and constants) without hoisting.
// http://es6-features.org/#BlockScopedVariables
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// Block-Scoped Functions
// Block-scoped function definitions.
// http://es6-features.org/#BlockScopedFunctions
//------------------------------------------------------------------------------

function foo () { return 1 }
foo() === 1
{
    function foo () { return 2 }
    foo() === 2
}
