/*
 [The "BSD licence"]
 Copyright (c) 2007-2008 Terence Parr
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

/*
author: Stefan Taranu
mail: stefan.taranu@gmail.com
Built with : java org.antlr.Tool ASN.g
antlr version: 3.1.1

The grammar is by far not complete. I have no experience in ANTLR, still
it was not so difficult to write this grammar.

In broad lines it is copied  from the ASN specification files (from the Annex):
X.680, X.681, X.682, X.683 and compiled it into one file. I removed some
of the predicates since it was too much ambiguity.

If you have some comments/improvements, send me an e-mail.
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

lexer grammar ASN_3gppLexer;

options {
    superClass = ASN_3gppLexerBase;
}

// Insert here @header for lexer.

NEED_LITERAL
    : '--' (' ' | '\t')*? 'Need'
    ;

COND_LITERAL
    : '--' (' ' | '\t')*? 'Cond'
    ;

INVALID_TAG
    : '--' ~('\n' | '\r')*
    ;

A_ROND
    : '@'
    ;

STAR
    : '*'
    ;

ASSIGN_OP
    : '::='
    ;

BOOLEAN_LITERAL
    : 'BOOLEAN'
    ;

TRUE_LITERAL
    : 'TRUE'
    ;

FALSE_LITERAL
    : 'FALSE'
    ;

DOT
    : '.'
    ;

DOUBLE_DOT
    : '..'
    ;

ELLIPSIS
    : '...'
    ;

APOSTROPHE
    : '\''
    ;

AMPERSAND
    : '&'
    ;

LESS_THAN
    : '<'
    ;

GREATER_THAN
    : '>'
    ;

LESS_THAN_SLASH
    : '</'
    ;

SLASH_GREATER_THAN
    : '/>'
    ;

TRUE_SMALL_LITERAL
    : 'true'
    ;

FALSE_SMALL_LITERAL
    : 'false'
    ;

INTEGER_LITERAL
    : 'INTEGER'
    ;

L_BRACE
    : '{'
    ;

R_BRACE
    : '}'
    ;

COMMA
    : ','
    ;

L_PARAN
    : '('
    ;

R_PARAN
    : ')'
    ;

MINUS
    : '-'
    ;

ENUMERATED_LITERAL
    : 'ENUMERATED'
    ;

REAL_LITERAL
    : 'REAL'
    ;

PLUS_INFINITY_LITERAL
    : 'PLUS-INFINITY'
    ;

MINUS_INFINITY_LITERAL
    : 'MINUS-INFINITY'
    ;

BIT_LITERAL
    : 'BIT'
    ;

STRING_LITERAL
    : 'STRING'
    ;

CONTAINING_LITERAL
    : 'CONTAINING'
    ;

OCTET_LITERAL
    : 'OCTET'
    ;

NULL_LITERAL
    : 'NULL'
    ;

SEQUENCE_LITERAL
    : 'SEQUENCE'
    ;

OPTIONAL_LITERAL
    : 'OPTIONAL'
    ;

DEFAULT_LITERAL
    : 'DEFAULT'
    ;

COMPONENTS_LITERAL
    : 'COMPONENTS'
    ;

OF_LITERAL
    : 'OF'
    ;

SET_LITERAL
    : 'SET'
    ;

EXCLAM
    : '!'
    ;

ALL_LITERAL
    : 'ALL'
    ;

EXCEPT_LITERAL
    : 'EXCEPT'
    ;

POWER
    : '^'
    ;

PIPE
    : '|'
    ;

UNION_LITERAL
    : 'UNION'
    ;

INTERSECTION_LITERAL
    : 'INTERSECTION'
    ;

INCLUDES_LITERAL
    : 'INCLUDES'
    ;

MIN_LITERAL
    : 'MIN'
    ;

MAX_LITERAL
    : 'MAX'
    ;

SIZE_LITERAL
    : 'SIZE'
    ;

FROM_LITERAL
    : 'FROM'
    ;

WITH_LITERAL
    : 'WITH'
    ;

COMPONENT_LITERAL
    : 'COMPONENT'
    ;

PRESENT_LITERAL
    : 'PRESENT'
    ;

ABSENT_LITERAL
    : 'ABSENT'
    ;

PATTERN_LITERAL
    : 'PATTERN'
    ;

TYPE_IDENTIFIER_LITERAL
    : 'TYPE-Identifier'
    ;

ABSTRACT_SYNTAX_LITERAL
    : 'ABSTRACT-SYNTAX'
    ;

CLASS_LITERAL
    : 'CLASS'
    ;

UNIQUE_LITERAL
    : 'UNIQUE'
    ;

SYNTAX_LITERAL
    : 'SYNTAX'
    ;

L_BRACKET
    : '['
    ;

R_BRACKET
    : ']'
    ;

INSTANCE_LITERAL
    : 'INSTANCE'
    ;

SEMI_COLON
    : ';'
    ;

IMPORTS_LITERAL
    : 'IMPORTS'
    ;

EXPORTS_LITERAL
    : 'EXPORTS'
    ;

EXTENSIBILITY_LITERAL
    : 'EXTENSIBILITY'
    ;

IMPLIED_LITERAL
    : 'IMPLIED'
    ;

EXPLICIT_LITERAL
    : 'EXPLICIT'
    ;

TAGS_LITERAL
    : 'TAGS'
    ;

IMPLICIT_LITERAL
    : 'IMPLICIT'
    ;

AUTOMATIC_LITERAL
    : 'AUTOMATIC'
    ;

DEFINITIONS_LITERAL
    : 'DEFINITIONS'
    ;

BEGIN_LITERAL
    : 'BEGIN'
    ;

END_LITERAL
    : 'END'
    ;

DOUBLE_L_BRACKET
    : '[['
    ;

DOUBLE_R_BRACKET
    : ']]'
    ;

COLON
    : ':'
    ;

CHOICE_LITERAL
    : 'CHOICE'
    ;

UNIVERSAL_LITERAL
    : 'UNIVERSAL'
    ;

APPLICATION_LITERAL
    : 'APPLICATION'
    ;

PRIVATE_LITERAL
    : 'PRIVATE'
    ;

EMBEDDED_LITERAL
    : 'EMBEDDED'
    ;

PDV_LITERAL
    : 'PDV'
    ;

EXTERNAL_LITERAL
    : 'EXTERNAL'
    ;

OBJECT_LITERAL
    : 'OBJECT'
    ;

IDENTIFIER_LITERAL
    : 'IDENTIFIER'
    ;

RELATIVE_OID_LITERAL
    : 'RELATIVE-OID'
    ;

CHARACTER_LITERAL
    : 'CHARACTER'
    ;

CONSTRAINED_LITERAL
    : 'CONSTRAINED'
    ;

BY_LITERAL
    : 'BY'
    ;

A_ROND_DOT
    : '@.'
    ;

ENCODED_LITERAL
    : 'ENCODED'
    ;

COMMENT
    : '--'
    ;

UNRESTRICTEDCHARACTERSTRINGTYPE
    : CHARACTER_LITERAL STRING_LITERAL
    ;

EXTENSTIONENDMARKER
    : COMMA ELLIPSIS
    ;

fragment DIGIT
    : '0' ..'9'
    ;

fragment UPPER
    : 'A' ..'Z'
    ;

fragment LOWER
    : 'a' ..'z'
    ;

NUMBER
    : DIGIT+
    ;

WS
    : (' ' | '\r' | '\t' | '\u000C' | '\n') -> skip
    ;

fragment Exponent
    : ('e' | 'E') ('+' | '-')? NUMBER
    ;

LINE_COMMENT
    : {this.IsColumnZero()}? (' ' | '\t')*? '--' ~('\n' | '\r')* '\r'? '\n' -> skip
    ;

BSTRING
    : APOSTROPHE ('0' ..'1')* '\'B'
    ;

fragment HEXDIGIT
    : DIGIT
    | 'a' ..'f'
    | 'A' ..'F'
    ;

HSTRING
    : APOSTROPHE HEXDIGIT* '\'H'
    ;

//IDENTIFIER : ('a'..'z'|'A'..'Z') ('0'..'9'|'a'..'z'|'A'..'Z')* ;
CSTRING
    : '"' (EscapeSequence | ~('\\' | '"'))* '"'
    ;

fragment EscapeSequence
    : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | APOSTROPHE | '\\')
    ;

//fragment

/**I found this char range in JavaCC's grammar, but Letter and Digit overlap.
   Still works, but...
 */
fragment LETTER
    : '\u0024'
    | '\u002d'
    | '\u0041' ..'\u005a'
    | '\u005f'
    | '\u0061' ..'\u007a'
    | '\u00c0' ..'\u00d6'
    | '\u00d8' ..'\u00f6'
    | '\u00f8' ..'\u00ff'
    | '\u0100' ..'\u1fff'
    | '\u3040' ..'\u318f'
    | '\u3300' ..'\u337f'
    | '\u3400' ..'\u3d2d'
    | '\u4e00' ..'\u9fff'
    | '\uf900' ..'\ufaff'
    ;

fragment JavaIDDigit
    : '\u0030' ..'\u0039'
    | '\u0660' ..'\u0669'
    | '\u06f0' ..'\u06f9'
    | '\u0966' ..'\u096f'
    | '\u09e6' ..'\u09ef'
    | '\u0a66' ..'\u0a6f'
    | '\u0ae6' ..'\u0aef'
    | '\u0b66' ..'\u0b6f'
    | '\u0be7' ..'\u0bef'
    | '\u0c66' ..'\u0c6f'
    | '\u0ce6' ..'\u0cef'
    | '\u0d66' ..'\u0d6f'
    | '\u0e50' ..'\u0e59'
    | '\u0ed0' ..'\u0ed9'
    | '\u1040' ..'\u1049'
    ;

//OBJECTCLASSREFERENCE
//	: UPPER (UPPER | LOWER | '-')
//	;
IDENTIFIER
    : LETTER (LETTER | JavaIDDigit)*
    ;