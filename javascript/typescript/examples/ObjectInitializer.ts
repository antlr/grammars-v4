"use strict";

//------------------------------------------------------------------------------
// Object Literal
// Initialization syntax for object literals
// https://262.ecma-international.org/11.0/#prod-ObjectLiteral
//------------------------------------------------------------------------------

let obj = { };
obj = { item1: "item1", item2: "item2" };
obj = { item1: "item1", item2: "item2", };

// https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-7.html#constant-named-properties
const Foo = "Foo";
const Bar = "Bar";
let x = {
  [Foo]: 100,
  [Bar]: "hello"
};
