/*
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
lexer grammar ANTLRv3Lexer;

options { superClass = LexerAdaptor; }

channels { OFF_CHANNEL }

tokens {
    DOC_COMMENT,
    PARSER,	
    LEXER,
    RULE,
    BLOCK,
    OPTIONAL,
    CLOSURE,
    POSITIVE_CLOSURE,
    SYNPRED,
    RANGE,
    CHAR_RANGE,
    EPSILON,
    ALT,
    EOR,
    EOB,
    EOA, // end of alt
    ID,
    ARG,
    ARGLIST,
    RET,
    LEXER_GRAMMAR,
    PARSER_GRAMMAR,
    TREE_GRAMMAR,
    COMBINED_GRAMMAR,
    INITACTION,
    LABEL, // $x used in rewrite rules
    TEMPLATE,
    SCOPE,
    SEMPRED,
    GATED_SEMPRED, // {p}? =>
    SYN_SEMPRED, // (...) =>   it's a manually-specified synpred converted to sempred
    BACKTRACK_SEMPRED, // auto backtracking mode syn pred converted to sempred
    FRAGMENT,
    TREE_BEGIN,
    ROOT,
    BANG,
    RANGE,
    REWRITE,
    ACTION_CONTENT
}

DOC_COMMENT
   : '/**' .*? ('*/' | EOF) -> channel(OFF_CHANNEL)
   ;

SL_COMMENT
   : '//' ~ [\r\n]* -> channel(OFF_CHANNEL)
   ;

ML_COMMENT
   : '/*' .*? '*/' -> channel(OFF_CHANNEL)
   ;

INT
   : '0' .. '9'+
   ;

CHAR_LITERAL
   : '\'' LITERAL_CHAR '\''
   ;

STRING_LITERAL
   : '\'' LITERAL_CHAR LITERAL_CHAR* '\''
   ;

fragment LITERAL_CHAR
   : ESC
   | ~ ('\'' | '\\')
   ;

// This seems to be available in Antlr3.

DOUBLE_QUOTE_STRING_LITERAL
   : '"' (ESC | ~ ('\\' | '"'))* '"'
   ;

// This seems to be available in Antlr3.

DOUBLE_ANGLE_STRING_LITERAL
   : '<<' .*? '>>'
   ;

fragment ESC
   : '\\' ('n' | 'r' | 't' | 'b' | 'f' | '"' | '\'' | '\\' | '>' | 'u' XDIGIT XDIGIT XDIGIT XDIGIT | .)
   ;

fragment XDIGIT
   : '0' .. '9'
   | 'a' .. 'f'
   | 'A' .. 'F'
   ;


// -------------------------
// Arguments
//
// Certain argument lists, such as those specifying call parameters
// to a rule invocation, or input parameters to a rule specification
// are contained within square brackets.

BEGIN_ARGUMENT
   : LBrack
   { this.handleBeginArgument(); }
   ;

// -------------------------
// Actions

BEGIN_ACTION
   : LBrace -> pushMode (Actionx)
   ;

// -------------------------
// Keywords
//
// Keywords may not be used as labels for rules or in any other context where
// they would be ambiguous with the keyword vs some other identifier.  OPTIONS,
// TOKENS, & CHANNELS blocks are handled idiomatically in dedicated lexical modes.

OPTIONS
   : 'options' -> pushMode (Options)
   ;

TOKENS
   : 'tokens' -> pushMode (Tokens)
   ;

CATCH : 'catch' ;
FINALLY : 'finally' ;
FRAGMENT : 'fragment' ;
GRAMMAR : 'grammar' ;
LEXER : 'lexer' ;
PARSER : 'parser' ;
PRIVATE : 'private' ;
PROTECTED : 'protected' ;
PUBLIC : 'public' ;
RETURNS : 'returns' ;
SCOPE : 'scope' ;
THROWS : 'throws' ;
TREE : 'tree' ;


fragment WS_LOOP : (WS | SL_COMMENT | ML_COMMENT)* ;

//// =================================
 
AT : At ;
BANG : '!' ;
COLON : Colon ;
COLONCOLON : DColon ;
COMMA : Comma ;
DOT : Dot ;
EQUAL : Equal ;
LBRACE : LBrace ;
LBRACK : LBrack ;
LPAREN : LParen ;
OR : Pipe ;
PLUS : Plus ;
QM : Question ;
RANGE : '..' ;
RBRACE : RBrace ;
RBRACK : RBrack ;
REWRITE : RArrow ;
ROOT : '^' ;
RPAREN : RParen ;
SEMI : Semi ;
SEMPREDOP : '=>' ;
STAR : Star ;
TREE_BEGIN : '^(' ;
DOLLAR : Dollar ;
PEQ : PlusAssign ;
NOT : Tilde ;

WS
   : (' ' | '\t' | '\r'? '\n')+ -> channel(OFF_CHANNEL)
   ;

TOKEN_REF
   : 'A' .. 'Z' ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')*
   ;

RULE_REF
   : 'a' .. 'z' ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')*
   ;

// ======================================================
// Lexer fragments
//
// -----------------------------------
// Whitespace & Comments

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

fragment BlockComment
   : '/*' .*? ('*/' | EOF)
   ;

fragment DocComment
   : '/**' .*? ('*/' | EOF)
   ;

fragment LineComment
   : '//' ~ [\r\n]*
   ;
   // -----------------------------------
   // Escapes
   // Any kind of escaped character that we can embed within ANTLR literal strings.

fragment EscSeq
   : Esc ([btnfr"'\\] | UnicodeEsc | . | EOF)
   ;

fragment EscAny
   : Esc .
   ;

fragment UnicodeEsc
   : 'u' (HexDigit (HexDigit (HexDigit HexDigit?)?)?)?
   ;
   // -----------------------------------
   // Numerals

fragment DecimalNumeral
   : '0'
   | [1-9] DecDigit*
   ;
   // -----------------------------------
   // Digits

fragment HexDigit
   : [0-9a-fA-F]
   ;

fragment DecDigit
   : [0-9]
   ;
   // -----------------------------------
   // Literals

fragment BoolLiteral
   : 'true'
   | 'false'
   ;

fragment CharLiteral
   : SQuote (EscSeq | ~ ['\r\n\\]) SQuote
   ;

fragment SQuoteLiteral
   : SQuote (EscSeq | ~ ['\r\n\\])* SQuote
   ;

fragment DQuoteLiteral
   : DQuote (EscSeq | ~ ["\r\n\\])* DQuote
   ;

fragment USQuoteLiteral
   : SQuote (EscSeq | ~ ['\r\n\\])*
   ;
   // -----------------------------------
   // Character ranges

fragment NameChar
   : NameStartChar
   | '0' .. '9'
   | Underscore
   | '\u00B7'
   | '\u0300' .. '\u036F'
   | '\u203F' .. '\u2040'
   ;

fragment NameStartChar
   : 'A' .. 'Z'
   | 'a' .. 'z'
   | '\u00C0' .. '\u00D6'
   | '\u00D8' .. '\u00F6'
   | '\u00F8' .. '\u02FF'
   | '\u0370' .. '\u037D'
   | '\u037F' .. '\u1FFF'
   | '\u200C' .. '\u200D'
   | '\u2070' .. '\u218F'
   | '\u2C00' .. '\u2FEF'
   | '\u3001' .. '\uD7FF'
   | '\uF900' .. '\uFDCF'
   | '\uFDF0' .. '\uFFFD'
   ;

// ignores | ['\u10000-'\uEFFFF] ;

// -----------------------------------
// Types

fragment Int : 'int' ;

// -----------------------------------
// Symbols

fragment Esc : '\\' ;
fragment Colon : ':' ;
fragment DColon : '::' ;

fragment SQuote
   : '\''
   ;

fragment DQuote
   : '"'
   ;

fragment LParen : '(' ;
fragment RParen : ')' ;
fragment LBrace : '{' ;
fragment RBrace : '}' ;
fragment LBrack : '[' ;
fragment RBrack : ']' ;
fragment RArrow : '->' ;

fragment Lt
   : '<'
   ;

fragment Gt
   : '>'
   ;

fragment Equal : '=' ;
fragment Question : '?' ;
fragment Star : '*' ;
fragment Plus : '+' ;
fragment PlusAssign : '+=' ;
fragment Underscore : '_' ;
fragment Pipe : '|' ;
fragment Dollar : '$' ;
fragment Comma : ',' ;
fragment Semi : ';' ;
fragment Dot : '.' ;
fragment Range : '..' ;
fragment At : '@' ;

fragment Pound
   : '#'
   ;

fragment Tilde
   : '~'
   ;

// ======================================================
// Lexer modes
// -------------------------
// Arguments

mode Argument;
// E.g., [int x, List<String> a[]]
NESTED_ARGUMENT
   : LBrack -> type (ARGUMENT_CONTENT) , pushMode (Argument)
   ;

ARGUMENT_ESCAPE
   : EscAny -> type (ARGUMENT_CONTENT)
   ;

ARGUMENT_STRING_LITERAL
   : DQuoteLiteral -> type (ARGUMENT_CONTENT)
   ;

ARGUMENT_CHAR_LITERAL
   : SQuoteLiteral -> type (ARGUMENT_CONTENT)
   ;

END_ARGUMENT
   : RBrack
   { this.handleEndArgument(); }
   ;
   // added this to return non-EOF token type here. EOF does something weird

UNTERMINATED_ARGUMENT
   : EOF -> popMode
   ;

ARGUMENT_CONTENT
   : .
   ;

// -------------------------
// Actions
//
// Many language targets use {} as block delimiters and so we
// must recursively match {} delimited blocks to balance the
// braces. Additionally, we must make some assumptions about
// literal string representation in the target language. We assume
// that they are delimited by ' or " and so consume these
// in their own alts so as not to inadvertantly match {}.

mode Actionx;
NESTED_ACTION
   : LBrace -> type (ACTION_CONTENT) , pushMode (Actionx)
   ;

ACTION_ESCAPE
   : EscAny -> type (ACTION_CONTENT)
   ;

ACTION_STRING_LITERAL
   : DQuoteLiteral -> type (ACTION_CONTENT)
   ;

ACTION_CHAR_LITERAL
   : SQuoteLiteral -> type (ACTION_CONTENT)
   ;

ACTION_DOC_COMMENT
   : DocComment -> type (ACTION_CONTENT)
   ;

ACTION_BLOCK_COMMENT
   : BlockComment -> type (ACTION_CONTENT)
   ;

ACTION_LINE_COMMENT
   : LineComment -> type (ACTION_CONTENT)
   ;

END_ACTION
   : RBrace
   { this.handleEndAction(); }
   ;

UNTERMINATED_ACTION
   : EOF -> popMode
   ;

ACTION_CONTENT
   : .
   ;

// -------------------------

mode Options;
OPT_DOC_COMMENT
   : DocComment -> type (DOC_COMMENT) , channel (OFF_CHANNEL)
   ;

OPT_BLOCK_COMMENT
   : BlockComment -> type (ML_COMMENT) , channel (OFF_CHANNEL)
   ;

OPT_LINE_COMMENT
   : LineComment -> type (SL_COMMENT) , channel (OFF_CHANNEL)
   ;

OPT_LBRACE
   : LBrace
   { this.handleOptionsLBrace(); }
   ;

OPT_RBRACE
   : RBrace -> type (RBRACE) , popMode
   ;

OPT_ID
   : Id -> type (ID)
   ;

OPT_DOT
   : Dot -> type (DOT)
   ;

OPT_ASSIGN
   : Equal -> type (EQUAL)
   ;

OPT_STRING_LITERAL
   : SQuoteLiteral -> type (STRING_LITERAL)
   ;

OPT_INT
   : DecimalNumeral -> type (INT)
   ;

OPT_STAR
   : Star -> type (STAR)
   ;

OPT_SEMI
   : Semi -> type (SEMI)
   ;

OPT_WS
   : Ws+ -> type (WS) , channel (OFF_CHANNEL)
   ;

// -------------------------

mode Tokens;
TOK_DOC_COMMENT
   : DocComment -> type (DOC_COMMENT) , channel (OFF_CHANNEL)
   ;

TOK_BLOCK_COMMENT
   : BlockComment -> type (ML_COMMENT) , channel (OFF_CHANNEL)
   ;

TOK_LINE_COMMENT
   : LineComment -> type (SL_COMMENT) , channel (OFF_CHANNEL)
   ;

TOK_LBRACE
   : LBrace -> type (LBRACE)
   ;

TOK_RBRACE
   : RBrace -> type (RBRACE) , popMode
   ;

TOK_ID
   : Id -> type (TOKEN_REF)
   ;

TOK_EQ
   : Equal -> type (EQUAL)
   ;

TOK_CL
   : '\'' LITERAL_CHAR '\'' -> type(CHAR_LITERAL)
   ;

TOK_SL
   : '\'' LITERAL_CHAR LITERAL_CHAR* '\'' -> type(STRING_LITERAL)
   ;

TOK_SEMI
   : Semi -> type (SEMI)
   ;

TOK_WS
   : Ws+ -> type (WS) , channel (OFF_CHANNEL)
   ;


// -------------------------

mode LexerCharSet;
LEXER_CHAR_SET_BODY
   : (~ [\]\\] | EscAny)+ -> more
   ;

LEXER_CHAR_SET
   : RBrack -> popMode
   ;

UNTERMINATED_CHAR_SET
   : EOF -> popMode
   ;

// ------------------------------------------------------------------------------
// Grammar specific Keywords, Punctuation, etc.

fragment Id
   : NameStartChar NameChar*
   ;

