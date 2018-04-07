"use strict";
//------------------------------------------------------------------------------
// Proxying
// Hooking into runtime-level object meta-operations.
// http://es6-features.org/#Proxying
//------------------------------------------------------------------------------

let target = {
    foo: "Welcome, foo"
}
let proxy = new Proxy(target, {
    get (receiver, name) {
        return name in receiver ? receiver[name] : `Hello, ${name}`
    }
})
proxy.foo   === "Welcome, foo"
proxy.world === "Hello, world"

//------------------------------------------------------------------------------
// Reflection
// Make calls corresponding to the object meta-operations.
// http://es6-features.org/#Reflection
//------------------------------------------------------------------------------

let obj = { a: 1 }
Object.defineProperty(obj, "b", { value: 2 })
obj[Symbol("c")] = 3
Reflect.ownKeys(obj) // [ "a", "b", Symbol(c) ]