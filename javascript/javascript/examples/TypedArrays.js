"use strict";
//------------------------------------------------------------------------------
// Typed Arrays
// Support for arbitrary byte-based data structures to implement network protocols,
//   cryptography algorithms, file format manipulations, etc.
// http://es6-features.org/#TypedArrays
//------------------------------------------------------------------------------

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