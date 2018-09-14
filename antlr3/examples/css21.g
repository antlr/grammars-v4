// A complete lexer and grammar for CSS 2.1 as defined by the
// W3 specification.
//
// This grammar is free to use providing you retain everyhting in this header comment
// section.
//
// Author      : Jim Idle, Temporal Wave LLC.
// Contact     : jimi@temporal-wave.com
// Website     : http://www.temporal-wave.com
// License     : ANTLR Free BSD License
//
// Please visit our Web site at http://www.temporal-wave.com and try our commercial
// parsers for SQL, C#, VB.Net and more.
//
// This grammar is free to use providing you retain everything in this header comment
// section.
//
grammar css21;

// -------------
// Main rule.   This is the main entry rule for the parser, the top level
//              grammar rule.
//
// A style sheet consists of an optional character set specification, an optional series
// of imports, and then the main body of style rules.
//
styleSheet  
    :   charSet
        imports*
        bodylist
     EOF
    ;
    
// -----------------
// Character set.   Picks up the user specified character set, should it be present.
//
charSet
    :   CHARSET_SYM STRING SEMI
    |
    ;

// ---------
// Import.  Location of an external style sheet to include in the ruleset.
//
imports
    :   IMPORT_SYM (STRING|URI) (medium (COMMA medium)*)? SEMI
    ;

// ---------
// Media.   Introduce a set of rules that are to be used if the consumer indicates
//          it belongs to the signified medium.
//
media
    : MEDIA_SYM medium (COMMA medium)*
        LBRACE
            ruleSet
        RBRACE
    ;

// ---------    
// Medium.  The name of a medim that are particulare set of rules applies to.
//
medium
    : IDENT 
    ;
    

bodylist
    : bodyset*
    ;
    
bodyset
    : ruleSet
    | media
    | page
    ;   
    
page
    : PAGE_SYM pseudoPage?
        LBRACE
            declaration SEMI (declaration SEMI)*
        RBRACE
    ;
    
pseudoPage
    : COLON IDENT
    ;
    
operator
    : SOLIDUS
    | COMMA
    |
    ;
    
combinator
    : PLUS
    | GREATER
    |
    ;
    
unaryOperator
    : MINUS
    | PLUS
    ;  
    
property
    : IDENT
    ;
    
ruleSet
    : selector (COMMA selector)*
        LBRACE
            declaration SEMI (declaration SEMI)*
        RBRACE
    ;
    
selector
    : simpleSelector (combinator simpleSelector)*
    ;

simpleSelector
    : elementName 
        ((esPred)=>elementSubsequent)*
        
    | ((esPred)=>elementSubsequent)+
    ;
    
esPred
    : HASH | DOT | LBRACKET | COLON
    ;
    
elementSubsequent
    : HASH
    | cssClass
    | attrib
    | pseudo
    ;
    
cssClass
    : DOT IDENT
    ;
    
elementName
    : IDENT
    | STAR
    ;
    
attrib
    : LBRACKET
    
        IDENT
        
            (
                (
                      OPEQ
                    | INCLUDES
                    | DASHMATCH
                )
                (
                      IDENT
                    | STRING
                )       
            )?
    
      RBRACKET
;

pseudo
    : COLON 
            IDENT
                ( // Function
                
                    LPAREN IDENT? RPAREN
                )?
    ;

declaration
    : property COLON expr prio?
    ;
    
prio
    : IMPORTANT_SYM
    ;
    
expr
    : term (operator term)*
    ;
    
term
    : unaryOperator?
        (
              NUMBER
            | PERCENTAGE
            | LENGTH
            | EMS
            | EXS
            | ANGLE
            | TIME
            | FREQ
        )
    | STRING
    | IDENT (   // Function
                LPAREN expr RPAREN
            )?
    | URI
    | hexColor
    ;
    
hexColor
    : HASH
    ;
    
// ==============================================================
// LEXER
//
// The lexer follows the normative section of WWW standard as closely
// as it can. For instance, where the ANTLR lexer returns a token that
// is unambiguous for both ANTLR and lex (the standard defines tokens
// in lex notation), then the token names are equivalent.
//
// Note however that lex has a match order defined as top to bottom 
// with longest match first. This results in a fairly inefficent, match,
// REJECT, match REJECT set of operations. ANTLR lexer grammars are actaully
// LL grammars (and hence LL recognizers), which means that we must
// specifically disambiguate longest matches and so on, when the lex
// like normative grammar results in ambiguities as far as ANTLR is concerned.
//
// This means that some tokens will either be combined compared to the
// normative spec, and the paresr will recognize them for what they are.
// In this case, the token will named as XXX_YYY where XXX and YYY are the
// token names used in the specification.
//
// Lex style macro names used in the spec may sometimes be used (in upper case
// version) as fragment rules in this grammar. However ANTLR fragment rules
// are not quite the same as lex macros, in that they generate actual 
// methods in the recognizer class, and so may not be as effecient. In
// some cases then, the macro contents are embedded. Annotation indicate when
// this is the case.
//
// See comments in the rules for specific details.
// --------------------------------------------------------------
//
// N.B. CSS 2.1 is defined as case insensitive, but because each character
//      is allowed to be written as in escaped form we basically define each
//      character as a fragment and reuse it in all other rules.
// ==============================================================


// --------------------------------------------------------------
// Define all the fragments of the lexer. These rules neither recognize
// nor create tokens, but must be called from non-fragment rules, which
// do create tokens, using these fragments to either purely define the
// token number, or by calling them to match a certain portion of
// the token string.
//

fragment    HEXCHAR     : ('a'..'f'|'A'..'F'|'0'..'9')  ;

fragment    NONASCII    : '\u0080'..'\uFFFF'            ;   // NB: Upper bound should be \u4177777

fragment    UNICODE     : '\\' HEXCHAR 
                                (HEXCHAR 
                                    (HEXCHAR 
                                        (HEXCHAR 
                                            (HEXCHAR HEXCHAR?)?
                                        )?
                                    )?
                                )? 
                                ('\r'|'\n'|'\t'|'\f'|' ')*  ;
                                
fragment    ESCAPE      : UNICODE | '\\' ~('\r'|'\n'|'\f'|HEXCHAR)  ;

fragment    NMSTART     : '_'
                        | 'a'..'z'
                        | 'A'..'Z'
                        | NONASCII
                        | ESCAPE
                        ;

fragment    NMCHAR      : '_'
                        | 'a'..'z'
                        | 'A'..'Z'
                        | '0'..'9'
                        | '-'
                        | NONASCII
                        | ESCAPE
                        ;
                        
fragment    NAME        : NMCHAR+   ;

fragment    URL         : ( 
                              '['|'!'|'#'|'$'|'%'|'&'|'*'|'-'|'~'
                            | NONASCII
                            | ESCAPE
                          )*
                        ;

                        
// Basic Alpha characters in upper, lower and escaped form. Note that
// whitespace and newlines are unimportant even within keywords. We do not
// however call a further fragment rule to consume these characters for
// reasons of performance - the rules are still eminently readable.
//
fragment    A   :   ('a'|'A') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'1'
                ;
fragment    B   :   ('b'|'B') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'2'
                ;
fragment    C   :   ('c'|'C') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'3'
                ;
fragment    D   :   ('d'|'D') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'4'
                ;
fragment    E   :   ('e'|'E') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'5'
                ;
fragment    F   :   ('f'|'F') ('\r'|'\n'|'\t'|'\f'|' ')*    
                |   '\\' ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'6'
                ;
fragment    G   :   ('g'|'G') ('\r'|'\n'|'\t'|'\f'|' ')* 
                |   '\\'
                        (
                              'g'
                            | 'G'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'7'
                        )
                ;
fragment    H   :   ('h'|'H') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'h'
                            | 'H'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'8'
                        )   
                ;
fragment    I   :   ('i'|'I') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'i'
                            | 'I'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')'9'
                        )
                ;
fragment    J   :   ('j'|'J') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'j'
                            | 'J'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('A'|'a')
                        )   
                ;
fragment    K   :   ('k'|'K') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'k'
                            | 'K'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('B'|'b')
                        )   
                ;
fragment    L   :   ('l'|'L') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'l'
                            | 'L'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('C'|'c')
                        )   
                ;
fragment    M   :   ('m'|'M') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'm'
                            | 'M'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('D'|'d')
                        )   
                ;
fragment    N   :   ('n'|'N') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'n'
                            | 'N'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('E'|'e')
                        )   
                ;
fragment    O   :   ('o'|'O') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'o'
                            | 'O'
                            | ('0' ('0' ('0' '0'?)?)?)? ('4'|'6')('F'|'f')
                        )   
                ;
fragment    P   :   ('p'|'P') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\'
                        (
                              'p'
                            | 'P'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('0')
                        )   
                ;
fragment    Q   :   ('q'|'Q') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'q'
                            | 'Q'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('1')
                        )   
                ;
fragment    R   :   ('r'|'R') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'r'
                            | 'R'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('2')
                        )   
                ;
fragment    S   :   ('s'|'S') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              's'
                            | 'S'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('3')
                        )   
                ;
fragment    T   :   ('t'|'T') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              't'
                            | 'T'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('4')
                        )   
                ;
fragment    U   :   ('u'|'U') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'u'
                            | 'U'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('5')
                        )
                ;
fragment    V   :   ('v'|'V') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (     'v'
                            | 'V'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('6')
                        )
                ;
fragment    W   :   ('w'|'W') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'w'
                            | 'W'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('7')
                        )   
                ;
fragment    X   :   ('x'|'X') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'x'
                            | 'X'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('8')
                        )
                ;
fragment    Y   :   ('y'|'Y') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'y'
                            | 'Y'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('9')
                        )
                ;
fragment    Z   :   ('z'|'Z') ('\r'|'\n'|'\t'|'\f'|' ')* 
                | '\\' 
                        (
                              'z'
                            | 'Z'
                            | ('0' ('0' ('0' '0'?)?)?)? ('5'|'7')('A'|'a')
                        )
                ;


// -------------
// Comments.    Comments may not be nested, may be multilined and are delimited
//              like C comments: /* ..... */
//              COMMENTS are hidden from the parser which simplifies the parser 
//              grammar a lot.
//
COMMENT         : '/*' ( options { greedy=false; } : .*) '*/'
    
                    {
                        $channel = 2;   // Comments on channel 2 in case we want to find them
                    }
                ;

// ---------------------
// HTML comment open.   HTML/XML comments may be placed around style sheets so that they
//                      are hidden from higher scope parsing engines such as HTML parsers.
//                      They comment open is therfore ignored by the CSS parser and we hide
//                      it from the ANLTR parser.
//
CDO             : '<!--'

                    {
                        $channel = 3;   // CDO on channel 3 in case we want it later
                    }
                ;
    
// ---------------------            
// HTML comment close.  HTML/XML comments may be placed around style sheets so that they
//                      are hidden from higher scope parsing engines such as HTML parsers.
//                      They comment close is therfore ignored by the CSS parser and we hide
//                      it from the ANLTR parser.
//
CDC             : '-->'

                    {
                        $channel = 4;   // CDC on channel 4 in case we want it later
                    }
                ;
                
INCLUDES        : '~='      ;
DASHMATCH       : '|='      ;

GREATER         : '>'       ;
LBRACE          : '{'       ;
RBRACE          : '}'       ;
LBRACKET        : '['       ;
RBRACKET        : ']'       ;
OPEQ            : '='       ;
SEMI            : ';'       ;
COLON           : ':'       ;
SOLIDUS         : '/'       ;
MINUS           : '-'       ;
PLUS            : '+'       ;
STAR            : '*'       ;
LPAREN          : '('       ;
RPAREN          : ')'       ;
COMMA           : ','       ;
DOT             : '.'       ;

// -----------------
// Literal strings. Delimited by either ' or "
//
fragment    INVALID :;
STRING          : '\'' ( ~('\n'|'\r'|'\f'|'\'') )* 
                    (
                          '\''
                        | { $type = INVALID; }
                    )
                    
                | '"' ( ~('\n'|'\r'|'\f'|'"') )*
                    (
                          '"'
                        | { $type = INVALID; }
                    )
                ;

// -------------
// Identifier.  Identifier tokens pick up properties names and values
//
IDENT           : '-'? NMSTART NMCHAR*  ;

// -------------
// Reference.   Reference to an element in the body we are styling, such as <XXXX id="reference">
//
HASH            : '#' NAME              ;

IMPORT_SYM      : '@' I M P O R T       ;
PAGE_SYM        : '@' P A G E           ;
MEDIA_SYM       : '@' M E D I A         ;
CHARSET_SYM     : '@charset '           ;

IMPORTANT_SYM   : '!' (WS|COMMENT)* I M P O R T A N T   ;

// ---------
// Numbers. Numbers can be followed by pre-known units or unknown units
//          as well as '%' it is a precentage. Whitespace cannot be between
//          the numebr and teh unit or percent. Hence we scan any numeric, then
//          if we detect one of the lexical sequences for unit tokens, we change
//          the lexical type dynamically.
//
//          Here we first define the various tokens, then we implement the
//          number parsing rule.
//
fragment    EMS         :;  // 'em'
fragment    EXS         :;  // 'ex'
fragment    LENGTH      :;  // 'px'. 'cm', 'mm', 'in'. 'pt', 'pc'
fragment    ANGLE       :;  // 'deg', 'rad', 'grad'
fragment    TIME        :;  // 'ms', 's'
fragment    FREQ        :;  // 'khz', 'hz'
fragment    DIMENSION   :;  // nnn'Somethingnotyetinvented'
fragment    PERCENTAGE  :;  // '%'

NUMBER
    :   (
              '0'..'9' ('.' '0'..'9'+)?
            | '.' '0'..'9'+
        )
        (
              (E (M|X))=>
                E
                (
                      M     { $type = EMS;          }
                    | X     { $type = EXS;          }
                )
            | (P(X|T|C))=>
                P
                (
                      X     
                    | T
                    | C
                )
                            { $type = LENGTH;       }   
            | (C M)=>
                C M         { $type = LENGTH;       }
            | (M (M|S))=> 
                M
                (
                      M     { $type = LENGTH;       }
            
                    | S     { $type = TIME;         }
                )
            | (I N)=>
                I N         { $type = LENGTH;       }
            
            | (D E G)=>
                D E G       { $type = ANGLE;        }
            | (R A D)=>
                R A D       { $type = ANGLE;        }
            
            | (S)=>S        { $type = TIME;         }
                
            | (K? H Z)=>
                K? H    Z   { $type = FREQ;         }
            
            | IDENT         { $type = DIMENSION;    }
            
            | '%'           { $type = PERCENTAGE;   }
            
            | // Just a number
        )
    ;

// ------------
// url and uri.
//
URI :   U R L
        '('
            ((WS)=>WS)? (URL|STRING) WS?
        ')'
    ;

// -------------
// Whitespace.  Though the W3 standard shows a Yacc/Lex style parser and lexer
//              that process the whitespace within the parser, ANTLR does not
//              need to deal with the whitespace directly in the parser.
//
WS      : (' '|'\t')+           { $channel = HIDDEN;    }   ;
NL      : ('\r' '\n'? | '\n')   { $channel = HIDDEN;    }   ;


// -------------
//  Illegal.    Any other character shoudl not be allowed.
//
