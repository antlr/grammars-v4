grammar vba_like;

options {
    caseInsensitive = false;
}

// module ----------------------------------

program
    : likePatternElement* EOF
    ;

likePatternElement
    : likePatternChar
    | likePatternCharlist
    | wildcard
    ;

likePatternChar
    : CHAR
    | '-'
    | ']'
    ;

wildcard
    : WILD_CHAR
    | WILD_SEQ
    | WILD_DIGIT
    ;

likePatternCharlist
    : '[' '!'? charList? ']'
    ;

charList
    : '-' charListElement* '-'?
    | charListElement+ '-'?
    ;
    
charListElement
    : charlistChar
    | charRange
    ;

charlistChar
    : CHAR
    | '*'
    | '#'
    | '?'
    | '['
    ;

charRange
    : charlistChar '-' charlistChar
    ;

// lexer rules --------------------------------------------------------------------------------

CHAR
    : ~[-*#?[\]]
    ;

WILD_CHAR
    : '?'
    ;

WILD_SEQ
    : '*'
    ;

WILD_DIGIT
    : '#'
    ;
    
