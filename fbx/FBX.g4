/*
 * Grammar for text-based (AKA ASCII) encoding of FBX format,
 * a format originally developed by Kaydara but now owned by Autodesk.
 *
 * This simpler parser just recognises the recursive node structure
 * and does nothing to help you know which nodes can be inside which.
 */
grammar FBX;

// PARSER RULES

start
    : node+
    ;

// Recursive node structure
node
    : name=IDENTIFIER
      attributes
      ( OPEN_BRACE
        node*
        CLOSE_BRACE
      )?
    ;

attributes
    : ( attribute
        ( COMMA
          attribute
        )*
      )?
    ;

attribute
    : value=(INTEGER | DECIMAL | STRING)
    ;

// LEXER RULES

INTEGER : '-'? DIGIT+;
DECIMAL : INTEGER ( '.' DIGIT* )?;

// Unknown behaviour: What if I want to escape a quote?
STRING : '"' ~'"'* '"';

IDENTIFIER : ( 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' )+;

fragment DIGIT : '0' .. '9';

QUOTE : '"';
COMMA : ',';
OPEN_BRACE : '{';
CLOSE_BRACE : '}';

COMMENT : ';' NON_NL* NL -> skip;

WS : ( ' ' | '\t' | NL )+ -> skip;
NL : ( '\r' '\n'? | '\n' );
NON_NL : ~[\r\n];
