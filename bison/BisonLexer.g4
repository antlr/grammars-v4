// Author -- Ken Domino
// Copyright 2020-2022
// MIT License

lexer grammar BisonLexer;

options { superClass = BisonLexerBase; }

// Insert here @header for C++ lexer.

tokens {
    SC_EPILOGUE
}

// ======================= Common fragments =========================

fragment Underscore
    : '_'
    ;

fragment NameStartChar
    :   'A'..'Z'
    |   'a'..'z'
    | '_'
    |   '\u00C0'..'\u00D6'
    |   '\u00D8'..'\u00F6'
    |   '\u00F8'..'\u02FF'
    |   '\u0370'..'\u037D'
    |   '\u037F'..'\u1FFF'
    |   '\u200C'..'\u200D'
    |   '\u2070'..'\u218F'
    |   '\u2C00'..'\u2FEF'
    |   '\u3001'..'\uD7FF'
    |   '\uF900'..'\uFDCF'
    |   '\uFDF0'..'\uFFFD'
    | '$' // For PHP
    ;   // ignores | ['\u10000-'\uEFFFF] ;

fragment DQuoteLiteral
    : DQuote ( EscSeq | ~["\r\n\\] | '\\' [\n\r]* )* DQuote
    ;

fragment DQuote
    : '"'
    ;

fragment SQuote
    : '\''
    ;

fragment CharLiteral
    : SQuote ( EscSeq | ~['\r\n\\] )  SQuote
    ;

fragment SQuoteLiteral
    : SQuote ( EscSeq | ~['\r\n\\] )* SQuote
    ;

fragment Esc
    : '\\'
    ;

fragment EscSeq
    :   Esc
        ([abefnrtv?"'\\]    // The standard escaped character set such as tab, newline, etc.
        | [xuU]?[0-9]+) // C-style 
    ;

fragment EscAny
    :   Esc .
    ;

fragment Id
    : NameStartChar NameChar*
    ;

fragment Type
    : ([\t\r\n\f a-zA-Z0-9] | '[' | ']' | '{' | '}' | '.' | '_' | '(' | ')' | ',')+
    ;

fragment NameChar
    :   NameStartChar
    |   '0'..'9'
    |   Underscore
    |   '\u00B7'
    |   '\u0300'..'\u036F'
    |   '\u203F'..'\u2040'
    | '.'
    | '-'
    ;

fragment BlockComment
    : '/*' 
     (
       '/' ~'*'
       | ~'/'
     )*
     '*/'
    ;

fragment LineComment
    : '//' ~[\r\n]*
    ;

fragment LineCommentExt
    : '//' ~'\n'* ( '\n' Hws* '//' ~'\n'* )*
    ;

fragment Ws
    : Hws
    | Vws
    ;

fragment Hws
    : [ \t]
    ;

fragment Vws
    : [\r\n\f]
    ;

/* Four types of user code:
    - prologue (code between '%{' '%}' in the first section, before %%);
    - actions, printers, union, etc, (between braced in the middle section);
    - epilogue (everything after the second %%).
    - predicate (code between '%?{' and '{' in middle section); */

// -------------------------
// Actions

fragment LBrace
    : '{'
    ;

fragment RBrace
    : '}'
    ;

fragment PercentLBrace
    : '%{'
    ;

fragment PercentRBrace
    : '%}'
    ;

fragment PercentQuestion
    : '%?{'
    ;

fragment ActionCode
    : Stuff*
    ;

fragment Stuff
    : EscAny
    | DQuoteLiteral
    | SQuoteLiteral
    | BlockComment
    | LineComment
    | NestedAction
    | ~('{' | '}' | '\'' | '"')
    ;

fragment NestedPrologue
    : PercentLBrace ActionCode PercentRBrace
    ;

fragment NestedAction
    : LBrace ActionCode RBrace
    ;

fragment NestedPredicate
    : PercentQuestion ActionCode RBrace
    ;

fragment Sp
    : Ws*
    ;

fragment Eqopt
    : (Sp [=])?
    ;

PercentPercent:   '%%' { this.NextMode(); }
    ;

  /*----------------------------.
  | Scanning Bison directives.  |
  `----------------------------*/

  /* For directives that are also command line options, the regex must be
        "%..."
     after "[-_]"s are removed, and the directive must match the --long
     option name, with a single string argument.  Otherwise, add exceptions
     to ../build-aux/cross-options.pl.  */

NONASSOC
    : '%binary'
    ;

CODE
    : '%code'
    ;

PERCENT_DEBUG
    : '%debug'
    ;

DEFAULT_PREC
    : '%default-prec'
    ;

DEFINE
    : '%define'
    ;

DEFINES
    : '%defines'
    ;

DESTRUCTOR
    : '%destructor'
    ;

DPREC
    : '%dprec'
    ;

EMPTY_RULE
    : '%empty'
    ;

EXPECT
    : '%expect'
    ;

EXPECT_RR
    : '%expect-rr'
    ;

PERCENT_FILE_PREFIX
    : '%file-prefix'
    ;

INITIAL_ACTION
    : '%initial-action'
    ;

GLR_PARSER
    : '%glr-parser'
    ;

LANGUAGE
    : '%language'
    ;

PERCENT_LEFT
    : '%left'
    ;

LEX
    : '%lex-param'
    ;

LOCATIONS
    : '%locations'
    ;

MERGE
    : '%merge'
    ;

NO_DEFAULT_PREC
    : '%no-default-prec'
    ;

NO_LINES
    : '%no-lines'
    ;

PERCENT_NONASSOC
    : '%nonassoc'
    ;

NONDETERMINISTIC_PARSER
    : '%nondeterministic-parser'
    ;

NTERM
    : '%nterm'
    ;

PARAM
    : '%param'
    ;

PARSE
    : '%parse-param'
    ;

PERCENT_PREC
    : '%prec'
    ;

PRECEDENCE
    : '%precedence'
    ;

PRINTER
    : '%printer'
    ;

REQUIRE
    : '%require'
    ;

PERCENT_RIGHT
    : '%right'
    ;

SKELETON
    : '%skeleton'
    ;

PERCENT_START
    : '%start'
    ;

TOKEN
    : '%term'
    ;

PERCENT_TOKEN
    : '%token'
    ;

TOKEN_TABLE
    : '%token' [-_] 'table'
    ;

PERCENT_TYPE
    : '%type'
    ;

PERCENT_UNION
    : '%union'
    ;

VERBOSE
    : '%verbose'
    ;

PERCENT_YACC
    : '%yacc'
    ;

  /* Deprecated since Bison 2.3b (2008-05-27), but the warning is
     issued only since Bison 3.4. */
PERCENT_PURE_PARSER
    : '%pure' [-_] 'parser'
    ;

  /* Deprecated since Bison 2.6 (2012-07-19), but the warning is
     issued only since Bison 3.3. */
PERCENT_NAME_PREFIX
    : '%name' [-_] 'prefix' Eqopt? Sp
    ;

  /* Deprecated since Bison 2.7.90, 2012. */
OBS_DEFAULT_PREC
    : '%default' [-_] 'prec'
    ;

OBS_PERCENT_ERROR_VERBOSE
    : '%error' [-_] 'verbose'
    ;

OBS_EXPECT_RR
    : '%expect' [-_] 'rr'
    ;

OBS_PERCENT_FILE_PREFIX
    : '%file-prefix' Eqopt
    ;

OBS_FIXED_OUTPUT
    : '%fixed' [-_] 'output' [-_] 'files'
    ;

OBS_NO_DEFAULT_PREC
    : '%no' [-_] 'default' [-_] 'prec'
    ;

OBS_NO_LINES
    : '%no' [-_] 'lines'
    ;

OBS_OUTPUT
    : '%output' Eqopt
    ;

OBS_TOKEN_TABLE
    : '%token' [-_] 'table'
    ;


BRACED_CODE:       NestedAction;
BRACED_PREDICATE:  NestedPredicate;
BRACKETED_ID:      '[' Id ']';
CHAR:              CharLiteral;
COLON:             ':';
//EPILOGUE:          'epilogue';
EQUAL:             '=';
//ID_COLON:          Id ':';
ID:                Id;
PIPE:              '|';
SEMICOLON:         ';';
TAG:               '<' Type '>';
TAG_ANY:           '<*>';
TAG_NONE:          '<>';
STRING: DQuoteLiteral;
INT: [0-9]+;
LPAREN: '(';
RPAREN: ')';

BLOCK_COMMENT
    :   BlockComment -> channel(HIDDEN)
    ;

LINE_COMMENT
    :   LineComment -> channel(HIDDEN)
    ;

WS
    : ( Hws | Vws )+ -> channel(HIDDEN)
    ;


PROLOGUE
    : NestedPrologue
    ;

// ==============================================================
// Note, all prologue rules can be used in grammar declarations.
// ==============================================================
//mode RuleMode;

mode EpilogueMode;
// Expected: Warning AC0131 greedy block ()+ contains wildcard; the non-greedy syntax ()+? may be preferred LanguageServer
// Changing from .* to .*? to avoid the warning. It may or may not work.
    EPILOGUE: .+ ;

