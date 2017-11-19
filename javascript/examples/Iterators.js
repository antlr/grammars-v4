"use strict";
// TODO: Implement
/*
//------------------------------------------------------------------------------
// Iterator & For-Of Operator
// http://es6-features.org/#IteratorForOfOperator
// http://es6-features.org/#IteratorForOfOperator
//------------------------------------------------------------------------------

let fibonacci = {
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
}
*/