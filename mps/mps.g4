/*
* FreeMPS originally written for Antlr3
*
* Ported to Antlr4 by Tom Everett
*
*/

grammar mps;

modell
   : firstrow rows columns rhs ranges? bounds? endata EOF
   ;

firstrow
   : NAMEINDICATORCARD IDENTIFIER? KEYWORDFREE?
   ;

rows
   : ROWINDICATORCARD (rowdatacard) +
   ;

columns
   : COLUMNINDICATORCARD columndatacards
   ;

rhs
   : RHSINDICATORCARD rhsdatacards
   ;

ranges
   : RANGESINDICATORCARD rangesdatacards
   ;

bounds
   : BOUNDSINDICATORCARD boundsdatacards
   ;

endata
   : ENDATAINDICATORCARD
   ;

rowdatacard
   : ROWTYPE IDENTIFIER
   ;

columndatacards
   : (columndatacard | intblock) +
   ;

rhsdatacards
   : rhsdatacard +
   ;

rangesdatacards
   : rangesdatacard +
   ;

boundsdatacards
   : boundsdatacard +
   ;

columndatacard
   : IDENTIFIER IDENTIFIER NUMERICALVALUE (IDENTIFIER NUMERICALVALUE)?
   ;

rhsdatacard
   : (IDENTIFIER | RHSINDICATORCARD) IDENTIFIER NUMERICALVALUE (IDENTIFIER NUMERICALVALUE)?
   ;

rangesdatacard
   : (IDENTIFIER | RANGESINDICATORCARD) IDENTIFIER NUMERICALVALUE (IDENTIFIER NUMERICALVALUE)?
   ;

boundsdatacard
   : BOUNDKEY (IDENTIFIER | BOUNDSINDICATORCARD) IDENTIFIER NUMERICALVALUE?
   ;

intblock
   : startmarker columndatacard + endmarker
   ;

startmarker
   : IDENTIFIER KEYWORDMARKER STARTMARKER
   ;

endmarker
   : IDENTIFIER KEYWORDMARKER ENDMARKER
   ;


NAMEINDICATORCARD
   : 'NAME'
   ;


ROWINDICATORCARD
   : 'ROWS'
   ;


COLUMNINDICATORCARD
   : 'COLUMNS'
   ;


RHSINDICATORCARD
   : 'RHS'
   ;


RANGESINDICATORCARD
   : 'RANGES'
   ;


BOUNDSINDICATORCARD
   : 'BOUNDS'
   ;


ENDATAINDICATORCARD
   : 'ENDATA'
   ;


KEYWORDMARKER
   : '\'MARKER\''
   ;


STARTMARKER
   : '\'INTORG\''
   ;


ENDMARKER
   : '\'INTEND\''
   ;


KEYWORDFREE
   : 'FREE'
   ;


BOUNDKEY
   : ('UP' | 'LO' | 'FX' | 'LI' | 'UI' | 'SC' | 'FR' | 'BV' | 'MI' | 'PL')
   ;


ROWTYPE
   : ('E' | 'L' | 'G' | 'N')
   ;


IDENTIFIER
   : LETTER CHARACTER*
   ;


NUMERICALVALUE
   : DIGIT DIGITS*
   ;


WS
   : (' ' | '\t' | '\n' | '\r' | '\f') + -> skip
   ;


LINE_COMMENT
   : ('*' | '$') ~ ('\n' | '\r')* '\r'? '\n' -> skip
   ;


fragment CHARACTER
   : (LETTER | DIGIT)
   ;


fragment LETTER
   : ('a' .. 'z' | 'A' .. 'Z' | '_' | '/' | '#' | '@' | '(' | ')')
   ;


fragment DIGIT
   : '0' .. '9' | '-' | '+' | '.' | ','
   ;


fragment DIGITS
   : DIGIT | 'D' | 'E' | 'e' | 'd'
   ;
