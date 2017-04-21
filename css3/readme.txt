trihus Tue Jul 1, 2008 13:43
This CSS parser handles valid CSS syntax. It will also accept property values not defined by CSS without giving an error. It has been debugged with ANTLRWorks and tested on a number of CSS files. (You must comment out language=CSharp for it to work with ANTLRWorks). I don't think it handles the full syntax of @import or @include.

Forward ported from Antlr3 to Antlr4 by Tom Everett.  April 2017.

