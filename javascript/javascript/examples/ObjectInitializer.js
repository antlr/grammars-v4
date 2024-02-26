"use strict";

//------------------------------------------------------------------------------
// Object Literal
// Initialization syntax for object literals
// https://262.ecma-international.org/11.0/#prod-ObjectLiteral
//------------------------------------------------------------------------------

obj = { };
obj = { item1: "item1", item2: "item2" };
obj = { item1: "item1", item2: "item2", };
obj = { item1: "item1",
        item2: "item2",
        item3: function(arg1) { return arg1; },
        item4: function myFunction(arg1) { return arg1; },
        item5: (arg1) => { return arg1; }
      };

