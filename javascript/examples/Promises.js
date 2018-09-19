"use strict";
//------------------------------------------------------------------------------
// Promise Usage
//   First class representation of a value that may be made asynchronously and
//   be available in the future.
// http://es6-features.org/#PromiseUsage
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// Promise Combination
// Combine one or more promises into new promises without having to take care
//   of ordering of the underlying asynchronous operations yourself.
// http://es6-features.org/#PromiseCombination
//------------------------------------------------------------------------------

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