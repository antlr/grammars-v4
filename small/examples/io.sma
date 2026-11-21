// Read next line into variable a as a string.
$->a;
// Read two lines, convert to number, add them and print the result.
$+$!
// Prompt for a Yes or No response and test it.
"Answer Y or N"! $="Y"?
// A less stringent test for words like "yes", "Yes", "Yo", "Ya", "y", ...
"Answer y or n:"! $:"[yY].*"?
// Prompt for a value for n.
"n=?"! $->n
