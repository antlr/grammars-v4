/**
 * A Protocol Buffers 3 grammar for ANTLR v4.
 *
 * Derived and adapted from:
 * https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
 *
 * @author Marco Willemart
 */
grammar Protobuf3;

//
// Proto file
//

proto
    :   syntax (   importStatement
               |   packageStatement
               |   option
               |   topLevelDef
               |   emptyStatement
               )*
        EOF
    ;

//
// Syntax
//

syntax
    :   'syntax' '=' ('"proto3"' | '\'proto3\'' ) ';'
    ;

//
// Import Statement
//

importStatement
    :   'import' ('weak' | 'public')? StrLit ';'
    ;

//
// Package
//

packageStatement
    :   'package' fullIdent ';'
    ;

//
// Option
//

option
    :   'option' optionName '=' constant ';'
    ;

optionName
    :   (Ident | '(' fullIdent ')' ) ('.' Ident)*
    ;

//
// Top Level definitions
//

topLevelDef
   :   message
   |   enumDefinition
   |   service
   ;

// Message definition

message
    :   'message' messageName messageBody
    ;

messageBody
    :   '{' (   field
            |   enumDefinition
            |   message
            |   option
            |   oneof
            |   mapField
            |   reserved
            |   emptyStatement
            )*
       '}'
    ;

// Enum definition

enumDefinition
    :   'enum' enumName enumBody
    ;

enumBody
    :   '{' (   option
            |   enumField
            |   emptyStatement
            )*
        '}'
    ;

enumField
    :   Ident '=' '-'? IntLit ('[' enumValueOption (','  enumValueOption)* ']')? ';'
    ;

enumValueOption
    :   optionName '=' constant
    ;

// Service definition

service
    :   'service' serviceName '{' (   option
                                  |   rpc
                                  // not defined in the protobuf specification
                                  //|   stream
                                  |   emptyStatement
                                  )*
        '}'
    ;

rpc
    :   'rpc' rpcName '(' 'stream'? messageType ')'
        'returns' '(' 'stream'? messageType ')' (('{' (option | emptyStatement)* '}') | ';')
    ;

//
// Reserved
//

reserved
    :   'reserved' (ranges | fieldNames) ';'
    ;

ranges
    :   range (',' range)*
    ;

    range
    :   IntLit
    |   IntLit 'to' IntLit
    ;

fieldNames
    :   StrLit (',' StrLit)*
    ;

//
// Fields
//

type
    :   (   'double'
        |   'float'
        |   'int32'
        |   'int64'
        |   'uint32'
        |   'uint64'
        |   'sint32'
        |   'sint64'
        |   'fixed32'
        |   'fixed64'
        |   'sfixed32'
        |   'sfixed64'
        |   'bool'
        |   'string'
        |   'bytes'
        )
    |   messageOrEnumType
    ;

fieldNumber
    : IntLit
    ;

// Normal field

field
    :   'repeated'? type fieldName '=' fieldNumber ('[' fieldOptions ']')? ';'
    ;

fieldOptions
    :   fieldOption (','  fieldOption)*
    ;

fieldOption
    :   optionName '=' constant
    ;

// Oneof and oneof field

oneof
    :   'oneof' oneofName '{' (oneofField | emptyStatement)* '}'
    ;

oneofField
    :   type fieldName '=' fieldNumber ('[' fieldOptions ']')? ';'
    ;

// Map field

mapField
    :   'map' '<' keyType ',' type '>' mapName '=' fieldNumber ('[' fieldOptions ']')? ';'
    ;

keyType
    :   'int32'
    |   'int64'
    |   'uint32'
    |   'uint64'
    |   'sint32'
    |   'sint64'
    |   'fixed32'
    |   'fixed64'
    |   'sfixed32'
    |   'sfixed64'
    |   'bool'
    |   'string'
    ;

//
// Lexical elements
//

// Keywords

BOOL            : 'bool';
BYTES           : 'bytes';
DOUBLE          : 'double';
ENUM            : 'enum';
FIXED32         : 'fixed32';
FIXED64         : 'fixed64';
FLOAT           : 'float';
IMPORT          : 'import';
INT32           : 'int32';
INT64           : 'int64';
MAP             : 'map';
MESSAGE         : 'message';
ONEOF           : 'oneof';
OPTION          : 'option';
PACKAGE         : 'package';
PROTO3_DOUBLE   : '"proto3"';
PROTO3_SINGLE   : '\'proto3\'';
PUBLIC          : 'public';
REPEATED        : 'repeated';
RESERVED        : 'reserved';
RETURNS         : 'returns';
RPC             : 'rpc';
SERVICE         : 'service';
SFIXED32        : 'sfixed32';
SFIXED64        : 'sfixed64';
SINT32          : 'sint32';
SINT64          : 'sint64';
STREAM          : 'stream';
STRING          : 'string';
SYNTAX          : 'syntax';
TO              : 'to';
UINT32          : 'uint32';
UINT64          : 'uint64';
WEAK            : 'weak';

// Letters and digits

fragment
Letter
    :   [A-Za-z_]
    ;

fragment
DecimalDigit
    :   [0-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexDigit
    :   [0-9A-Fa-f]
    ;

// Identifiers

Ident
    :   Letter (Letter | DecimalDigit)*
    ;

fullIdent
    :   Ident ('.' Ident)*
    ;

messageName
    :   Ident
    ;

enumName
    :   Ident
    ;

messageOrEnumName
    :   Ident
    ;

fieldName
    :   Ident
    ;

oneofName
    :   Ident
    ;

mapName
    :   Ident
    ;

serviceName
    :   Ident
    ;

rpcName
    :   Ident
    ;

messageType
    :   '.'? (Ident '.')* messageName
    ;

messageOrEnumType
    :   '.'? (Ident '.')* messageOrEnumName
    ;

// Integer literals

IntLit
    :   DecimalLit
    |   OctalLit
    |   HexLit
    ;

fragment
DecimalLit
    :   [1-9] DecimalDigit*
    ;

fragment
OctalLit
    :   '0' OctalDigit*
    ;

fragment
HexLit
    :   '0' ('x' | 'X') HexDigit+
    ;

// Floating-point literals

FloatLit
    :   (   Decimals '.' Decimals? Exponent?
        |   Decimals Exponent
        |   '.' Decimals Exponent?
        )
    |   'inf'
    |   'nan'
    ;

fragment
Decimals
    :   DecimalDigit+
    ;

fragment
Exponent
    :   ('e' | 'E') ('+' | '-')? Decimals
    ;

// Boolean

BoolLit
    :   'true'
    |   'false'
    ;

// String literals

StrLit
    :   '\'' CharValue* '\''
    |   '"' CharValue* '"'
    ;

fragment
CharValue
    :   HexEscape
    |   OctEscape
    |   CharEscape
    |   ~[\u0000\n\\]
    ;

fragment
HexEscape
    :   '\\' ('x' | 'X') HexDigit HexDigit
    ;

fragment
OctEscape
    :   '\\' OctalDigit OctalDigit OctalDigit
    ;

fragment
CharEscape
    :   '\\' [abfnrtv\\'"]
    ;

Quote
    :   '\''
    |   '"'
    ;

// Empty Statement

emptyStatement
    :   ';'
    ;

// Constant

constant
    :   fullIdent
    |   ('-' | '+')? IntLit
    |   ('-' | '+')? FloatLit
    |   (   StrLit
        |   BoolLit
        )
    ;

// Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
LCHEVR          : '<';
RCHEVR          : '>';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';
MINUS           : '-';
PLUS            : '+';

// Operators

ASSIGN          : '=';

// Whitespace and comments

WS  :   [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;
