/* Generated Mon, Jun 13, 2022 8:11:58 AM EST
 *
 * Copyright (c) 2022, 2023 Ken Domino
 * Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
 * for details. All rights reserved. Use of this source code is governed by a
 * BSD-style license that can be found in the LICENSE file.
 *
 * This grammar is generated from the CFG contained in:
 * https://github.com/dart-lang/language/blob/70eb85cf9a6606a9da0de824a5d55fd06de1287f/specification/dartLangSpec.tex
 *
 * The bash script used to scrape and the refactor the gramamr is here:
 * https://github.com/kaby76/ScrapeDartSpec/blob/master/refactor.sh
 *
 * Note: the CFG in the Specification is in development, and is for approximately
 * Dart version 2.15. The Specification is not up-to-date vis-a-vis the actual
 * compiler code, located here:
 * https://github.com/dart-lang/sdk/tree/main/pkg/_fe_analyzer_shared/lib/src/parser
 * Some of the refactorings that are applied are to bring the code into a working
 * Antlr4 parser. Other refactorings replace some of the rules in the Spec because
 * the Spec is incorrect, or incomplete.
 *
 * This grammar has been checked against a large subset (~370 Dart files) of the Dart SDK:
 * https://github.com/dart-lang/sdk/tree/main/sdk/lib
 * A copy of the SDK is provided in the examples for regression testing.
 */
lexer grammar Dart2Lexer;

options { superClass=Dart2LexerBase; }

A: '&';
AA: '&&';
AE: '&=';
AT: '@';
C: ',';
CB: ']';
CBC: '}';
CIR: '^';
CIRE: '^=';
CO: ':';
CP: ')';
D: '.';
DD: '..';
DDD: '...';
DDDQ: '...?';
EE: '==';
EG: '=>';
EQ: '=';
GT: '>';
LT: '<';
LTE: '<=';
LTLT: '<<';
LTLTE: '<<=';
ME: '-=';
MINUS: '-';
MM: '--';
NE: '!=';
NOT: '!';
OB: '[';
OBC: '{';
OP: '(';
P: '|';
PC: '%';
PE: '%=';
PL: '+';
PLE: '+=';
PLPL: '++';
PO: '#';
POE: '|=';
PP: '||';
QU: '?';
QUD: '?.';
QUDD: '?..';
QUQU: '??';
QUQUEQ: '??=';
SC: ';';
SE: '/=';
SL: '/';
SQS: '~/';
SQSE: '~/=';
SQUIG: '~';
ST: '*';
STE: '*=';
ABSTRACT_:'abstract';
AS_:'as';
ASSERT_:'assert';
ASYNC_:'async';
AWAIT_:'await';
BREAK_:'break';
CASE_:'case';
CATCH_:'catch';
CLASS_:'class';
CONST_:'const';
CONTINUE_:'continue';
COVARIANT_:'covariant';
DEFAULT_:'default';
DEFERRED_:'deferred';
DO_:'do';
DYNAMIC_:'dynamic';
ELSE_:'else';
ENUM_:'enum';
EXPORT_:'export';
EXTENDS_:'extends';
EXTENSION_:'extension';
EXTERNAL_:'external';
FACTORY_:'factory';
FALSE_:'false';
FINAL_:'final';
FINALLY_:'finally';
FOR_:'for';
FUNCTION_:'Function';
GET_:'get';
GTILDE_:'gtilde';
HIDE_:'hide';
IF_:'if';
IMPLEMENTS_:'implements';
IMPORT_:'import';
IN_:'in';
INTERFACE_:'interface';
IS_:'is';
LATE_:'late';
LET_:'let';
LIBRARY_:'library';
MIXIN_:'mixin';
NATIVE_:'native';
NEW_:'new';
NULL_:'null';
OF_:'of';
ON_:'on';
OPERATOR_:'operator';
PART_:'part';
REQUIRED_:'required';
RETHROW_:'rethrow';
RETURN_:'return';
SET_:'set';
SHOW_:'show';
STATIC_:'static';
SUPER_:'super';
SWITCH_:'switch';
SYNC_:'sync';
THIS_:'this';
THROW_:'throw';
TRUE_:'true';
TRY_:'try';
TYPEDEF_:'typedef';
VAR_:'var';
VOID_:'void';
WHILE_:'while';
WITH_:'with';
YIELD_:'yield';
NUMBER : DIGIT+ ( '.' DIGIT+ )? EXPONENT? | '.' DIGIT+ EXPONENT? ;
HEX_NUMBER : '0x' HEX_DIGIT+ | '0X' HEX_DIGIT+ ;
SingleLineString : StringDQ | StringSQ | 'r\'' ~('\'' | '\n' | '\r')* '\'' | 'r"' ~('"' | '\n' | '\r')* '"' ;
MultiLineString : '"""' StringContentTDQ*? '"""' | '\'\'\'' StringContentTSQ*? '\'\'\'' | 'r"""' (~'"' | '"' ~'"' | '""' ~'"')* '"""' | 'r\'\'\'' (~'\'' | '\'' ~'\'' | '\'\'' ~'\'')* '\'\'\'' ;
IDENTIFIER : IDENTIFIER_START IDENTIFIER_PART* ;
WHITESPACE : ( '\t' | ' ' | NEWLINE )+  -> skip;
SINGLE_LINE_COMMENT : '//' ~[\r\n]* -> skip ;
MULTI_LINE_COMMENT : '/*' ( MULTI_LINE_COMMENT | . )*? '*/'  -> skip ;
fragment EXPONENT : ( 'e' | 'E' ) ( '+' | '-' )? DIGIT+ ;
fragment HEX_DIGIT : 'a' .. 'f' | 'A' .. 'F' | DIGIT ;
fragment StringDQ : '"' StringContentDQ*? '"' ;
fragment StringContentDQ : ~('\\' | '"' | '\n' | '\r' | '$') | '\\' ~('\n' | '\r') | StringDQ | '${' StringContentDQ*? '}' | '$' { CheckNotOpenBrace() }? ;
fragment StringSQ : '\'' StringContentSQ*? '\'' ;
fragment StringContentSQ : ~('\\' | '\'' | '\n' | '\r' | '$') | '\\' ~('\n' | '\r') | StringSQ | '${' StringContentSQ*? '}' | '$' { CheckNotOpenBrace() }? ;
fragment StringContentTDQ : ~('\\' | '"') | '"' ~'"' | '""' ~'"' ;
fragment StringContentTSQ : '\'' ~'\'' | '\'\'' ~'\'' | . ;
fragment ESCAPE_SEQUENCE : '\n' | '\r' | '\\f' | '\\b' | '\t' | '\\v' | '\\x' HEX_DIGIT HEX_DIGIT | '\\u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT | '\\u{' HEX_DIGIT_SEQUENCE '}' ;
fragment HEX_DIGIT_SEQUENCE : HEX_DIGIT HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? ;
fragment NEWLINE : '\n' | '\r' | '\r\n' ;
fragment BUILT_IN_IDENTIFIER : 'abstract' | 'as' | 'covariant' | 'deferred' | 'dynamic' | 'export' | 'external' | 'extension' | 'factory' | 'Function' | 'get' | 'implements' | 'import' | 'interface' | 'late' | 'library' | 'mixin' | 'operator' | 'part' | 'required' | 'set' | 'static' | 'typedef' ;
fragment OTHER_IDENTIFIER : 'async' | 'hide' | 'of' | 'on' | 'show' | 'sync' | 'await' | 'yield' ;
fragment IDENTIFIER_NO_DOLLAR : IDENTIFIER_START_NO_DOLLAR IDENTIFIER_PART_NO_DOLLAR* ;
fragment IDENTIFIER_START_NO_DOLLAR : LETTER | '_' ;
fragment IDENTIFIER_PART_NO_DOLLAR : IDENTIFIER_START_NO_DOLLAR | DIGIT ;
fragment IDENTIFIER_START : IDENTIFIER_START_NO_DOLLAR | '$' ;
fragment IDENTIFIER_PART : IDENTIFIER_START | DIGIT ;
fragment LETTER : 'a' .. 'z' | 'A' .. 'Z' ;
fragment DIGIT : '0' .. '9' ;
