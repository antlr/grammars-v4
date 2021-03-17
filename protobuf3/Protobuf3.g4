// SPDX-License-Identifier: Apache-2.0
/**
 * A Protocol Buffers 3 grammar
 *
 * Original source: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
 * Original source is published under Apache License 2.0.
 *
 * Changes from the source above:
 * - rewrite to antlr
 * - extract some group to rule.
 *
 * @author anatawa12
 */

grammar Protobuf3;

proto
  : syntax
    (
        importStatement
      | packageStatement
      | optionStatement
      | topLevelDef
      | emptyStatement
    )*
  ;

// Syntax

syntax
  : 'syntax' '=' ('\'proto3\'' | '"proto3"') ';'
  ;

// Import Statement

importStatement
  : 'import' ( 'weak' | 'public' )? strLit ';'
  ;

// Package

packageStatement
  : 'package' fullIdent ';'
  ;

// Option

optionStatement
  : 'option' optionName '=' constant ';'
  ;

optionName
  : fullIdent
  | '(' fullIdent ')' ( '.' fullIdent )?
  ;

// Normal Field

field
  : ( 'repeated' )? type fieldName '=' fieldNumber ( '[' fieldOptions ']' )? ';'
  ;

fieldOptions
  : fieldOption ( ','  fieldOption )*
  ;

fieldOption
  : optionName '=' constant
  ;

fieldNumber
  : intLit
  ;

// Oneof and oneof field

oneof
  : 'oneof' oneofName '{' ( optionStatement | oneofField | emptyStatement )* '}'
  ;

oneofField
  : type fieldName '=' fieldNumber ( '[' fieldOptions ']' )? ';'
  ;

// Map field

mapField
  : 'map' '<' keyType ',' type '>' mapName
        '=' fieldNumber ( '[' fieldOptions ']' )? ';'
  ;
keyType
  : 'int32'
  | 'int64'
  | 'uint32'
  | 'uint64'
  | 'sint32'
  | 'sint64'
  | 'fixed32'
  | 'fixed64'
  | 'sfixed32'
  | 'sfixed64'
  | 'bool'
  | 'string'
  ;

// field types

type
  : 'double'
  | 'float'
  | 'int32'
  | 'int64'
  | 'uint32'
  | 'uint64'
  | 'sint32'
  | 'sint64'
  | 'fixed32'
  | 'fixed64'
  | 'sfixed32'
  | 'sfixed64'
  | 'bool'
  | 'string'
  | 'bytes'
  | messageType
  | enumType
  ;

// Reserved

reserved
  : 'reserved' ( ranges | reservedFieldNames ) ';'
  ;

ranges
  : range ( ',' range )*
  ;

range
  : intLit ( 'to' ( intLit | 'max' ) )?
  ;

reservedFieldNames
  : strLit ( ',' strLit )*
  ;

// Top Level definitions

topLevelDef
  : messageDef
  | enumDef
  | serviceDef
  ;

// enum

enumDef
  : 'enum' enumName enumBody
  ;

enumBody
  : '{' enumElement* '}'
  ;

enumElement
  : optionStatement
  | enumField
  | emptyStatement
  ;

enumField
  : ident '=' ( '-' )? intLit enumValueOptions?';'
  ;

enumValueOptions
  : '[' enumValueOption ( ','  enumValueOption )* ']'
  ;

enumValueOption
  : optionName '=' constant
  ;

// message

messageDef
  : 'message' messageName messageBody
  ;

messageBody
  : '{' messageElement* '}'
  ;

messageElement
  : field
  | enumDef
  | messageDef
  | optionStatement
  | oneof
  | mapField
  | reserved
  | emptyStatement
  ;

// service

serviceDef
  : 'service' serviceName '{' serviceElement* '}'
  ;

serviceElement
  : optionStatement
  | rpc
  | emptyStatement
  ;

rpc
  : 'rpc' rpcName '(' ( 'stream' )? messageType ')'
        'returns' '(' ( 'stream' )? messageType ')'
        ('{' ( optionStatement | emptyStatement )* '}' | ';')
  ;

// lexical

constant
  : fullIdent
  | ('-' | '+' )? intLit
  | ( '-' | '+' )? floatLit
  | strLit
  | boolLit
  | blockLit
  ;

// not specified in specification but used in tests
blockLit
  : '{' ( ident ':' constant )* '}'
  ;

emptyStatement: ';';

// Lexical elements

ident: IDENTIFIER | keywords;
fullIdent: ident ( '.' ident )*;
messageName: ident;
enumName: ident;
fieldName: ident;
oneofName: ident;
mapName: ident;
serviceName: ident;
rpcName: ident;
messageType: ( '.' )? ( ident '.' )* messageName;
enumType: ( '.' )? ( ident '.' )* enumName;

intLit: INT_LIT;
strLit: STR_LIT;
boolLit: BOOL_LIT;
floatLit: FLOAT_LIT;

STR_LIT: ( '\'' ( CHAR_VALUE )*? '\'' ) |  ( '"' ( CHAR_VALUE )*? '"' );
fragment CHAR_VALUE: HEX_ESCAPE | OCT_ESCAPE | CHAR_ESCAPE | ~[\u0000\n\\];
fragment HEX_ESCAPE: '\\' ( 'x' | 'X' ) HEX_DIGIT HEX_DIGIT;
fragment OCT_ESCAPE: '\\' OCTAL_DIGIT OCTAL_DIGIT OCTAL_DIGIT;
fragment CHAR_ESCAPE: '\\' ( 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' | '"' );

BOOL_LIT: 'true' | 'false';

FLOAT_LIT : ( DECIMALS '.' DECIMALS? EXPONENT? | DECIMALS EXPONENT | '.' DECIMALS EXPONENT? ) | 'inf' | 'nan';
fragment EXPONENT  : ( 'e' | 'E' ) ('+' | '-')? DECIMALS;
fragment DECIMALS  : DECIMAL_DIGIT+;

INT_LIT     : DECIMAL_LIT | OCTAL_LIT | HEX_LIT;
fragment DECIMAL_LIT : ( [1-9] ) DECIMAL_DIGIT*;
fragment OCTAL_LIT   : '0' OCTAL_DIGIT*;
fragment HEX_LIT     : '0' ( 'x' | 'X' ) HEX_DIGIT+ ;

IDENTIFIER: LETTER ( LETTER | DECIMAL_DIGIT )*;

fragment LETTER: [A-Za-z_];
fragment DECIMAL_DIGIT: [0-9];
fragment OCTAL_DIGIT: [0-7];
fragment HEX_DIGIT: [0-9A-Fa-f];

// comments
WS  :   [ \t\r\n\u000C]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
COMMENT: '/*' .*? '*/' -> skip;

keywords
  : 'syntax'
  | 'import'
  | 'weak'
  | 'public'
  | 'package'
  | 'option'
  | 'repeated'
  | 'oneof'
  | 'map'
  | 'int32'
  | 'int64'
  | 'uint32'
  | 'uint64'
  | 'sint32'
  | 'sint64'
  | 'fixed32'
  | 'fixed64'
  | 'sfixed32'
  | 'sfixed64'
  | 'bool'
  | 'string'
  | 'double'
  | 'float'
  | 'bytes'
  | 'reserved'
  | 'to'
  | 'max'
  | 'enum'
  | 'message'
  | 'service'
  | 'rpc'
  | 'stream'
  | 'returns'
  ;
