/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*/

lexer grammar TomlLexer;

WS : [ \t]+ -> skip ;
NL : ('\r'? '\n')+ ;
COMMENT : '#' (~[\n])* ;
L_BRACKET : '[' ;
DOUBLE_L_BRACKET : '[[' ;
R_BRACKET : ']' ;
DOUBLE_R_BRACKET : ']]' ;
EQUALS : '=' -> pushMode(SIMPLE_VALUE_MODE);
DOT : '.' ;
COMMA : ',' ;

fragment DIGIT : [0-9] ;
fragment ALPHA : [A-Za-z] ;

// strings
fragment ESC : '\\' (["\\/bfnrt] | UNICODE | EX_UNICODE) ;
fragment UNICODE : 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT ;
fragment EX_UNICODE : 'U' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT ;
BASIC_STRING : '"' (ESC | ~["\\\n])*? '"' ;
LITERAL_STRING : '\'' (~['\n])*? '\'' ;

// keys
UNQUOTED_KEY : (ALPHA | DIGIT | '-' | '_')+ ;


mode SIMPLE_VALUE_MODE;

VALUE_WS: WS -> skip ;

L_BRACE : '{' -> mode(INLINE_TABLE_MODE) ;
ARRAY_START : L_BRACKET -> type(L_BRACKET), mode(ARRAY_MODE) ;

// booleans
BOOLEAN : ('true' | 'false') -> popMode ;

// strings
fragment ML_ESC : '\\' '\r'? '\n' | ESC ;
VALUE_BASIC_STRING : BASIC_STRING -> type(BASIC_STRING), popMode ;
ML_BASIC_STRING : '"""' (ML_ESC | ~["\\])*? '"""' -> popMode ;
VALUE_LITERAL_STRING : LITERAL_STRING -> type(LITERAL_STRING), popMode ;
ML_LITERAL_STRING : '\'\'\'' (.)*? '\'\'\'' -> popMode ;

// floating point numbers
fragment EXP : ('e' | 'E') [+-]? ZERO_PREFIXABLE_INT ;
fragment ZERO_PREFIXABLE_INT : DIGIT (DIGIT | '_' DIGIT)* ;
fragment FRAC : '.' ZERO_PREFIXABLE_INT ;
FLOAT : DEC_INT ( EXP | FRAC EXP?) -> popMode ;
INF : [+-]? 'inf' -> popMode ;
NAN : [+-]? 'nan' -> popMode ;

// integers
fragment HEX_DIGIT : [A-Fa-f] | DIGIT ;
fragment DIGIT_1_9 : [1-9] ;
fragment DIGIT_0_7 : [0-7] ;
fragment DIGIT_0_1 : [0-1] ;
DEC_INT : [+-]? (DIGIT | (DIGIT_1_9 (DIGIT | '_' DIGIT)+)) -> popMode ;
HEX_INT : '0x' HEX_DIGIT (HEX_DIGIT | '_' HEX_DIGIT)* -> popMode ;
OCT_INT : '0o' DIGIT_0_7 (DIGIT_0_7 | '_' DIGIT_0_7)* -> popMode ;
BIN_INT : '0b' DIGIT_0_1 (DIGIT_0_1 | '_' DIGIT_0_1)* -> popMode ;

// dates
fragment YEAR : DIGIT DIGIT DIGIT DIGIT ;
fragment MONTH : DIGIT DIGIT ;
fragment DAY : DIGIT DIGIT ;
fragment DELIM : 'T' | 't' | ' ' ;
fragment HOUR : DIGIT DIGIT ;
fragment MINUTE : DIGIT DIGIT ;
fragment SECOND : DIGIT DIGIT ;
fragment SECFRAC : '.' DIGIT+ ;
fragment NUMOFFSET : ('+' | '-') HOUR ':' MINUTE ;
fragment OFFSET : 'Z' | NUMOFFSET ;
fragment PARTIAL_TIME : HOUR ':' MINUTE ':' SECOND SECFRAC? ;
fragment FULL_DATE : YEAR '-' MONTH '-' DAY ;
fragment FULL_TIME : PARTIAL_TIME OFFSET ;
OFFSET_DATE_TIME : FULL_DATE DELIM FULL_TIME -> popMode ;
LOCAL_DATE_TIME : FULL_DATE DELIM PARTIAL_TIME -> popMode ;
LOCAL_DATE : FULL_DATE -> popMode ;
LOCAL_TIME : PARTIAL_TIME -> popMode ;

mode INLINE_TABLE_MODE;

INLINE_TABLE_WS : WS -> skip ;
INLINE_TABLE_KEY_DOT : DOT -> type(DOT) ;
INLINE_TABLE_COMMA : COMMA -> type(COMMA) ;
R_BRACE : '}' -> popMode ;

INLINE_TABLE_KEY_BASIC_STRING : BASIC_STRING -> type(BASIC_STRING) ;
INLINE_TABLE_KEY_LITERAL_STRING : LITERAL_STRING -> type(LITERAL_STRING) ;
INLINE_TABLE_KEY_UNQUOTED: UNQUOTED_KEY -> type(UNQUOTED_KEY) ;

INLINE_TABLE_EQUALS : EQUALS -> type(EQUALS), pushMode(SIMPLE_VALUE_MODE) ;

mode ARRAY_MODE;

ARRAY_WS : WS -> skip ;
ARRAY_NL : NL -> type(NL) ;
ARRAY_COMMENT : COMMENT -> type(COMMENT) ;
ARRAY_COMMA : COMMA -> type(COMMA) ;

ARRAY_INLINE_TABLE_START : L_BRACE -> type(L_BRACE), pushMode(INLINE_TABLE_MODE) ;
NESTED_ARRAY_START : L_BRACKET -> type(L_BRACKET), pushMode(ARRAY_MODE) ;
ARRAY_END : R_BRACKET -> type(R_BRACKET), popMode ;

ARRAY_BOOLEAN : BOOLEAN -> type(BOOLEAN) ;

ARRAY_BASIC_STRING : BASIC_STRING -> type(BASIC_STRING) ;
ARRAY_ML_BASIC_STRING : ML_BASIC_STRING -> type(ML_BASIC_STRING) ;
ARRAY_LITERAL_STRING : LITERAL_STRING -> type(LITERAL_STRING) ;
ARRAY_ML_LITERAL_STRING : ML_LITERAL_STRING -> type(ML_LITERAL_STRING) ;

ARRAY_FLOAT : FLOAT -> type(FLOAT) ;
ARRAY_INF : INF -> type(INF) ;
ARRAY_NAN : NAN -> type(NAN) ;

ARRAY_DEC_INT : DEC_INT -> type(DEC_INT) ;
ARRAY_HEX_INT : HEX_INT -> type(HEX_INT) ;
ARRAY_OCT_INT : OCT_INT -> type(OCT_INT) ;
ARRAY_BIN_INT : BIN_INT -> type(BIN_INT) ;

ARRAY_OFFSET_DATE_TIME : OFFSET_DATE_TIME -> type(OFFSET_DATE_TIME) ;
ARRAY_LOCAL_DATE_TIME : LOCAL_DATE_TIME -> type(LOCAL_DATE_TIME) ;
ARRAY_LOCAL_DATE : LOCAL_DATE -> type(LOCAL_DATE) ;
ARRAY_LOCAL_TIME : LOCAL_TIME -> type(LOCAL_TIME) ;
