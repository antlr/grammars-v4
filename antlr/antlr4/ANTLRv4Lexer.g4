/*
 * [The "BSD license"]
 *  Copyright (c) 2012-2015 Terence Parr
 *  Copyright (c) 2012-2015 Sam Harwell
 *  Copyright (c) 2015 Gerald Rosenberg
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/**
 *	A grammar for ANTLR v4 implemented using v4 syntax
 *
 *	Modified 2015.06.16 gbr
 *	-- update for compatibility with Antlr v4.5
 */

// $antlr-format alignTrailingComments on, columnLimit 130, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments off
// $antlr-format useTab off, allowShortRulesOnASingleLine off, allowShortBlocksOnASingleLine on, alignSemicolons hanging
// $antlr-format alignColons hanging

// ======================================================
// Lexer specification
// ======================================================

lexer grammar ANTLRv4Lexer;

options {
    superClass = LexerAdaptor;

    // Using a predefined list of tokens here to ensure the same order of the tokens as they were defined
    // in the old ANTLR3 tree parsers (to avoid having to change the tree parsers code).
    // The actual values of the tokens doesn't matter, but the order does.
    tokenVocab = predefined;
}

// Insert here @header for lexer.

// Standard set of fragments
tokens {
    TOKEN_REF,
    RULE_REF,
    LEXER_CHAR_SET
}

channels {
    OFF_CHANNEL,
    COMMENT
}

// -------------------------
// Comments

DOC_COMMENT
    : '/**' .*? ('*/' | EOF) -> channel (COMMENT)
    ;

BLOCK_COMMENT
    : '/*' .*? ('*/' | EOF) -> channel (COMMENT)
    ;

LINE_COMMENT
    : '//' ~ [\r\n]* -> channel (COMMENT)
    ;

// -------------------------
// Integer

INT
    : '0'
    | [1-9] [0-9]*
    ;

// -------------------------
// Literal string
//
// ANTLR makes no distinction between a single character literal and a
// multi-character string. All literals are single quote delimited and
// may contain unicode escape sequences of the form \uxxxx, where x
// is a valid hexadecimal number (per Unicode standard).
STRING_LITERAL
    : '\'' (ESC_SEQUENCE | ~ ['\r\n\\])* '\''
    ;

UNTERMINATED_STRING_LITERAL
    : '\'' (ESC_SEQUENCE | ~ ['\r\n\\])*
    ;

// -------------------------
// Arguments
//
// Certain argument lists, such as those specifying call parameters
// to a rule invocation, or input parameters to a rule specification
// are contained within square brackets.
BEGIN_ARGUMENT
    : '[' { this.handleBeginArgument(); }
    ;

// Many language targets use {} as block delimiters and so we
// must recursively match {} delimited blocks to balance the
// braces. Additionally, we must make some assumptions about
// literal string representation in the target language. We assume
// that they are delimited by ' or " and so consume these
// in their own alts so as not to inadvertently match {}.
ACTION
    : NESTED_ACTION
    ;

fragment NESTED_ACTION
    : // Action and other blocks start with opening {
    '{' (
        NESTED_ACTION          // embedded {} block
        | STRING_LITERAL       // single quoted string
        | DoubleQuoteLiteral   // double quoted string
        | TripleQuoteLiteral   // string literal with triple quotes
        | BacktickQuoteLiteral // backtick quoted string
        | '/*' .*? '*/'        // block comment
        | '//' ~[\r\n]*        // line comment
        | '\\' .               // Escape sequence
        | ~(
            '\\'
            | '"'
            | '\''
            | '`'
            | '{'
        ) // Some other single character that is not handled above
    )*? '}'
    ;

// -------------------------
// Keywords
//
// 'options', 'tokens', and 'channels' are considered keywords
// but only when followed by '{', and considered as a single token.
// Otherwise, the symbols are tokenized as RULE_REF and allowed as
// an identifier in a labeledElement.
OPTIONS
    : 'options' WS* '{'
    ;

TOKENS
    : 'tokens' WS* '{'
    ;

CHANNELS
    : 'channels' WS* '{'
    ;

IMPORT
    : 'import'
    ;

FRAGMENT
    : 'fragment'
    ;

LEXER
    : 'lexer'
    ;

PARSER
    : 'parser'
    ;

GRAMMAR
    : 'grammar'
    ;

PROTECTED
    : 'protected'
    ;

PUBLIC
    : 'public'
    ;

PRIVATE
    : 'private'
    ;

RETURNS
    : 'returns'
    ;

LOCALS
    : 'locals'
    ;

THROWS
    : 'throws'
    ;

CATCH
    : 'catch'
    ;

FINALLY
    : 'finally'
    ;

MODE
    : 'mode'
    ;

// -------------------------
// Punctuation

COLON
    : ':'
    ;

COLONCOLON
    : '::'
    ;

COMMA
    : ','
    ;

SEMI
    : ';'
    ;

LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

RBRACE
    : '}'
    ;

RARROW
    : '->'
    ;

LT
    : '<'
    ;

GT
    : '>'
    ;

ASSIGN
    : '='
    ;

QUESTION
    : '?'
    ;

STAR
    : '*'
    ;

PLUS_ASSIGN
    : '+='
    ;

PLUS
    : '+'
    ;

OR
    : '|'
    ;

DOLLAR
    : '$'
    ;

RANGE
    : '..'
    ;

DOT
    : '.'
    ;

AT
    : '@'
    ;

POUND
    : '#'
    ;

NOT
    : '~'
    ;

// -------------------------
// Identifiers - allows unicode rule/token names

ID
    : NameStartChar NameChar*
    ;

// -------------------------
// Whitespace

WS
    : [ \t\r\n\f]+ -> channel (OFF_CHANNEL)
    ;

// ======================================================
// Lexer modes
// -------------------------
// Arguments
mode Argument;

// E.g., [int x, List<String> a[]]
NESTED_ARGUMENT
    : '[' -> type (ARGUMENT_CONTENT), pushMode (Argument)
    ;

ARGUMENT_ESCAPE
    : '\\' . -> type (ARGUMENT_CONTENT)
    ;

ARGUMENT_STRING_LITERAL
    : DoubleQuoteLiteral -> type (ARGUMENT_CONTENT)
    ;

ARGUMENT_CHAR_LITERAL
    : STRING_LITERAL -> type (ARGUMENT_CONTENT)
    ;

END_ARGUMENT
    : ']' { this.handleEndArgument(); }
    ;

// added this to return non-EOF token type here. EOF does something weird
UNTERMINATED_ARGUMENT
    : EOF -> popMode
    ;

ARGUMENT_CONTENT
    : .
    ;

// -------------------------
mode LexerCharSet;

LEXER_CHAR_SET_BODY
    : (~ [\]\\] | '\\' .)+ -> more
    ;

LEXER_CHAR_SET
    : ']' -> popMode
    ;

UNTERMINATED_CHAR_SET
    : EOF -> popMode
    ;

// ------------------------------------------------------------------------------
// Grammar specific Keywords, Punctuation, etc.

fragment ESC_SEQUENCE
    : '\\' ([btnfr"'\\] | UnicodeESC | . | EOF)
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;

fragment UnicodeESC
    : 'u' (HexDigit (HexDigit (HexDigit HexDigit?)?)?)?
    ;

fragment DoubleQuoteLiteral
    : '"' (ESC_SEQUENCE | ~["\r\n\\])*? '"'
    ;

fragment TripleQuoteLiteral
    : '"""' (ESC_SEQUENCE | .)*? '"""'
    ;

fragment BacktickQuoteLiteral
    : '`' (ESC_SEQUENCE | ~["\r\n\\])*? '`'
    ;

// -----------------------------------
// Character ranges

fragment NameChar
    : NameStartChar
    | '0' .. '9'
    | '_'
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
    // ignores | ['\u10000-'\uEFFFF]
    ;
