/* */

/* Explicit concatenation: */
say "Now is the winter " || "of our discontent"
say "Now is the winter " || /*comment*/ "of our discontent"
say "Now is the winter " || ,
"of our discontent"

/* Abuttal concatenation: */
say "Made glorious "Summer()
say "Made glorious "/*comment*/Summer()
say "Made glorious ",
Summer()

/* Blank concatenation: */
say "By this sun" "of York"
say "By this sun"/*comment*/ "of York"
say "By this sun" ,
 "of York"

exit

Summer: procedure
return "summer"
