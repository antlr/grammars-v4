"use strict";
//------------------------------------------------------------------------------
// Support for constants (also known as "immutable variables"), i.e.,
//   variables which cannot be re-assigned new content.
//   Notice: this only makes the variable itself immutable,
//   not its assigned content (for instance, in case the content is an object,
//   this means the object itself can still be altered).
//------------------------------------------------------------------------------

const PI = 3.141593
PI > 3.0

var c = 10;
{
    const c = 2; // At this point, c = 2.
} // At this point, c = 10.

const name = "Thomas Jefferson";
const answer = 42, numpages = 10;
const myarray = new Array();