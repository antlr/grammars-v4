function strict() {
   /* comment */
   // comment
   // comment
  'use strict';
  function nested()
  {
    let a = 4; // strict, "let" is not an identifier.
    return 'And so am I!';
  }
  return "Hi!  I'm a strict mode function!  " + nested();
}

function notStrict() {
  var let = 0; // not strict, no error.
  return "I'm not strict.";
}