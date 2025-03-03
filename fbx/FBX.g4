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
      COLON
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
    : value=ASTERISK? (INTEGER | DECIMAL | STRING | BOOLEAN)
    ;

// LEXER RULES

INTEGER : '-'? DIGIT+;
DECIMAL : INTEGER ( '.' DIGIT* )? ( ('e' | 'E') ( '+' | '-' ) DIGIT+ )?;

// Unknown behaviour: What if I want to escape a quote?
STRING : '"' ~'"'* '"';

BOOLEAN : 'Y' | 'N';

IDENTIFIER : ( 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' )+;

fragment DIGIT : '0' .. '9';

QUOTE : '"';
COLON : ':';
COMMA : ',';
OPEN_BRACE : '{';
CLOSE_BRACE : '}';
ASTERISK : '*';

COMMENT : ';' NON_NL* NL -> channel(HIDDEN);

WS : ( ' ' | '\t' | NL )+ -> channel(HIDDEN);
NL : '\r' '\n'? | '\n';
NON_NL : ~[\r\n];
