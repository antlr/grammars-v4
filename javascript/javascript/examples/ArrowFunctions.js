"use strict";
//------------------------------------------------------------------------------
// Expression Bodies
// More expressive closure syntax.
// http://es6-features.org/#ExpressionBodies
//------------------------------------------------------------------------------

odds  = evens.map(v => v + 1)
pairs = evens.map(v => ({ even: v, odd: v + 1 }))
nums  = evens.map((v, i) => v + i)

//------------------------------------------------------------------------------
// Statement Bodies
// More expressive closure syntax.
// http://es6-features.org/#StatementBodies
//------------------------------------------------------------------------------

nums.forEach(v => {
   if (v % 5 === 0)
       fives.push(v)
})

//------------------------------------------------------------------------------
// Lexical this
// More intuitive handling of current object context.
// http://es6-features.org/#Lexicalthis
//------------------------------------------------------------------------------

this.nums.forEach((v) => {
    if (v % 5 === 0)
        this.fives.push(v)
})