"use strict";
//------------------------------------------------------------------------------
// Map/Set & WeakMap/WeakSet
// Cleaner data-structure for common algorithms based on sets.
// http://es6-features.org/#SetDataStructure
//------------------------------------------------------------------------------

let s = new Set()
s.add("hello").add("goodbye").add("hello")
s.size === 2
s.has("hello") === true
for (let key of s.values()) // insertion order
    console.log(key)

//------------------------------------------------------------------------------
// Map Data-Structure
// Cleaner data-structure for common algorithms based on maps.
// http://es6-features.org/#MapDataStructure
//------------------------------------------------------------------------------

let m = new Map()
let s = Symbol()
m.set("hello", 42)
m.set(s, 34)
m.get(s) === 34
m.size === 2
for (let [ key, val ] of m.entries())
    console.log(key + " = " + val)

//------------------------------------------------------------------------------
// Weak-Link Data-Structures
// Memory-leak-free Object-key’d side-by-side data-structures.
// http://es6-features.org/#WeakLinkDataStructures
//------------------------------------------------------------------------------

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