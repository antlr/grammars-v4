/*
 * Parser grammar for Invisible XML (iXML) 1.0
 * https://invisiblexml.org/1.0/
 *
 * Parser rule names mirror those of the iXML specification.
 * The mark/tmark characters ('@', '^', '-') are retained in the
 * parse tree as data rather than being suppressed.
 *
 * Notes on the translation:
 *   - ANTLR4 reserved words 'rule', 'from', 'class', 'string'
 *     are renamed with a trailing underscore.
 *   - 's' and 'rs' consume visible WS/COMMENT tokens, so required
 *     separation is enforced rather than discarded by the lexer.
 *   - 'cchar', 'dchar', and 'schar' are stubs; their characters
 *     are bundled into comment/string tokens by the lexer.
 *   - Nested comments are supported via the COMMENT lexer rule.
 *   - Names are assembled from adjacent segments and dots in parser
 *     context. '#' uses HEX_MODE to separate encoded characters.
 */

// $antlr-format alignColons hanging, alignSemicolons hanging, alignTrailingComments true, allowShortBlocksOnASingleLine true
// $antlr-format allowShortRulesOnASingleLine false, columnLimit 150, maxEmptyLinesToKeep 1, minEmptyLines 1, reflowComments false, useTab false

parser grammar ixmlParser;

options {
    tokenVocab = ixmlLexer;
}

// ── Parser rules ─────────────────────────────────────────────────────────────

ixml
    : s prolog? rule_ (rs rule_)* s EOF
    ;

prolog
    : version s
    ;

version
    : 'ixml' rs 'version' rs string_ s '.'
    ;

rule_
    : (mark s)? name s ASSIGN s alts '.'
    ;

mark
    : '@'
    | '^'
    | '-'
    ;

alts
    : alt (ALT_SEP s alt)*
    ;

alt
    : (term_ ( ',' s term_)*)?
    ;

term_
    : factor
    | option
    | repeat0
    | repeat1
    ;

factor
    : terminal_
    | nonterminal
    | insertion
    | '(' s alts ')' s
    ;

repeat0
    : factor '*' s
    | factor '**' s sep
    ;

repeat1
    : factor '+' s
    | factor '++' s sep
    ;

option
    : factor '?' s
    ;

sep
    : factor
    ;

nonterminal
    : (mark s)? name s
    ;

name
    : (NAME | 'ixml' | 'version' | CODE)
      (NAME | 'ixml' | 'version' | CODE | NAME_FOLLOWER | '-' | '.')*
    ;

terminal_
    : literal
    | charset
    ;

literal
    : quoted
    | encoded
    ;

quoted
    : (tmark s)? string_ s
    ;

tmark
    : '^'
    | '-'
    ;

string_
    : DQUOTE_CHAR
    | SQUOTE_CHAR
    | DQUOTE_STRING
    | SQUOTE_STRING
    ;

// dchar and schar are handled at the lexer level inside DQUOTE_STRING /
// SQUOTE_STRING; these stubs preserve the rule names from the iXML spec.
dchar
    : // ~['"'; #a; #d] | '""'  — bundled into DQUOTE_STRING
    ;

schar
    : // ~["'"; #a; #d] | "''"  — bundled into SQUOTE_STRING
    ;

encoded
    : (tmark s)? HASH hex s
    ;

hex
    : HEX_DIGITS
    ;

charset
    : inclusion
    | exclusion
    ;

inclusion
    : (tmark s)? set_
    ;

exclusion
    : (tmark s)? '~' s set_
    ;

set_
    : '[' s (member s (ALT_SEP s member s)*)? ']' s
    ;

member
    : string_
    | HASH hex
    | range_
    | class_
    ;

range_
    : from_ s '-' s to_
    ;

from_
    : character
    ;

to_
    : character
    ;

// CHAR tokens contain exactly one decoded character, including doubled quotes.
character
    : DQUOTE_CHAR
    | SQUOTE_CHAR
    | HASH hex
    ;

class_
    : code
    ;

code
    : CODE
    ;

insertion
    : '+' s (string_ | HASH hex) s
    ;

// Required separation must remain distinguishable from optional spacing.
s
    : (whitespace_ | comment)*
    ;

rs
    : (whitespace_ | comment)+
    ;

comment
    : COMMENT
    ;

cchar
    : // ~['{' '}']  — see COMMENT lexer rule
    ;

whitespace_
    : WS
    ;
