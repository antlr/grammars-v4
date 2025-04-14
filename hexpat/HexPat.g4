/*
MIT License

Copyright (c) 2024 Enaium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

grammar HexPat;

hexpat
    : ( importStatements += importStatement
      | usingStatements += usingStatement
      | namespaces += namespace
      | structs += struct
      | enums += enum
      | bitfields += bitfield
      | unions += union
      | fns += fn
      | structMemberStatements += structMemberStatement
      | (expression ';')
      | statement
      )* EOF
    ;

importStatement
    : 'import' parts += IDENTIFIER ('.' parts += IDENTIFIER)* ';'
    ;

usingStatement
    : 'using' (
        name = IDENTIFIER ('=' typeName typeArguments?)? attribute? ';'
        | (typeName typeArguments '=' typeName typeArguments)
    )
    ;

namespace
    : 'namespace' name = IDENTIFIER '{'
        ( structs += struct
        | structMemberStatements += structMemberStatement
        | enums += enum
        | bitfields += bitfield
        | unions += union
        | fns += fn
        | usingStatement
        | namespaces += namespace
        )*
      '}'
    ;

typeParameters
    : '<' typeParameter (',' typeParameter)* '>'
    ;

typeParameter
    : 'auto'? typeName
    ;

typeArgument
    : 'auto'? typeName typeArguments?
    | literal
    | expression
    ;

typeArguments
    : '<' typeArgument (',' typeArgument)* '>'
    ;

struct
    : 'struct' name = IDENTIFIER typeParameters? (':' typeName)? structBlock attribute?';'
    ;

structBlock
    : '{' structStatement* '}'
    ;

structStatement
    : structBlock
    | structMember
    | structIf
    | structMatch
    | 'break' ';'
    | 'continue' ';'
    | 'return' ';'
    | expression ';'
    | structTryCatch
    ;

structIf
    : 'if' parExpression structStatement ('else' structStatement)?
    ;
structMatch
    : 'match' '(' matchVariables ')' '{' structMatchCases '}'
    ;

structMatchCases
    : structMatchCase+
    ;

structMatchCase
    : '(' matchPatterns ')' ':' structStatement
    ;

bitfield
    : 'bitfield' IDENTIFIER typeParameters? bitfieldBlock attribute? ';'
    ;

bitfieldBlock
    : '{' bitfieldStatement* '}'
    ;

bitfieldStatement
    : bitfieldBlock
    | bitfieldMember attribute? ';'
    | bitfieldIf
    | bitfieldMatch
    | 'break' ';'
    | 'continue' ';'
    | 'return' ';'
    | expression ';'
    ;

bitfieldIf
    : 'if' parExpression bitfieldStatement ('else' bitfieldStatement)?
    ;

bitfieldMatch
    : 'match' '(' matchVariables ')' '{' bitfieldMatchCases '}'
    ;

bitfieldMatchCases
    : bitfieldMatchCase+
    ;

bitfieldMatchCase
    : '(' matchPatterns ')' ':' bitfieldStatement
    ;

bitfieldMember
    : (typeName? name = IDENTIFIER) ('=' expression)? (':' expression)?
    | 'padding' ':' expression
    ;

union
    : 'union' name = IDENTIFIER typeParameters? unionBlock attribute? ';'
    ;

unionBlock
    : '{' unionStatement* '}'
    ;

unionStatement
    : unionBlock
    | unionMember ';'
    | unionIf
    | unionMatch
    | 'break' ';'
    | 'continue' ';'
    | 'return' ';'
    | expression ';'
    ;

unionIf
    : 'if' parExpression unionStatement ('else' unionStatement)?
    ;

unionMatch
    : 'match' '(' matchVariables ')' '{' unionMatchCases '}'
    ;

unionMatchCases
    : unionMatchCase+
    ;

unionMatchCase
    : '(' matchPatterns ')' ':' unionStatement
    ;

unionMember
    : type = typeName name = IDENTIFIER ('[' memberSize ']')? at? attribute?
    | pointedType = typeName '*' name = IDENTIFIER ':' sizeType = typeName attribute?
    ;


attribute
    : '[' '[' attributeList ']' ']'
    ;

attributeList
    : attributeItem (',' attributeItem)*
    ;

attributeItem
    : typeName '(' expressionList? ')'
    | IDENTIFIER
    ;

structMemberStatement
    : structMember
    ;

structMemberDeclarators
    :   structMemberDeclarator (',' structMemberDeclarator)*
    ;

structMemberDeclarator
    : '*'? (name = IDENTIFIER)? ('[' (size = memberSize)? ']')? (':' typeName)? ('=' expression)? at? attribute?
    ;

structMember
    : 'const'? endian? (type = typeName typeArguments?)? structMemberDeclarators io? ';'
    | padding ';'
    ;

padding
    : 'padding' '[' (expression | whileSize) ']'
    ;

at
    : '@' section
    ;

section
    : INTEGER_LITERAL ('in' IDENTIFIER)?
    | expression
    ;

whileSize
    : 'while' parExpression
    ;

memberSize
    : expression
    | whileSize
    ;

typeName
    : (IDENTIFIER '::')* (primitiveType | IDENTIFIER)
    ;

primitiveType
    : 'u128' | 'u64' | 'u32' | 'u16' | 'u8'
    | 's128' | 's64' | 's32' | 's16' | 's8'
    | 'double' | 'float' | 'char' | 'str' | 'bool' | 'auto'
    ;

enum
    : 'enum' name = IDENTIFIER? ':' type = typeName
      enumBlock
      attribute?
      ';'
    ;

enumBlock
    :
    '{'
    (enumEntry ',')* enumEntry?
    '}'
    ;


enumEntry
    : name = IDENTIFIER ('=' patternItem)?
    ;

fn
    : 'fn' name = IDENTIFIER '(' (parameters += parameter (',' parameters += parameter)*)? ')'
      block ';'
    ;

parameter
    : 'ref'? type = typeName name = IDENTIFIER
    ;

block
    : '{' blockStatement* '}'
    ;

blockStatement
    : localVariableDeclarationStatement
    | statement
    ;

localVariableDeclarationStatement
    : localVariableDeclaration ';'
    ;

localVariableDeclaration
    : 'const'? type = typeName variableDeclarators
    ;

variableDeclarators
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    : IDENTIFIER ('[' ']')? at?
    ;

variableInitializer
    : expression
    ;

forInit
    :   localVariableDeclaration
    |   expressionList
    ;

forUpdate
    :   expressionList
    ;

forControl
    : forInit? ',' expression? ',' forUpdate?
    ;

statement
    : block
    | 'if' parExpression statement ('else' statement)?
    | matchStatement
    | 'for' '(' forControl ')' statement
    | 'while' parExpression statement
    | 'return' expression? ';'
    | 'break' ';'
    | 'continue' ';'
    | expression ';'
    | padding ';'
    | tryCatch
    ;

matchStatement
    : 'match' '(' matchVariables ')' '{' matchCases '}'
    ;

matchVariables
    : expression (',' expression)*
    ;

matchCases
    : matchCase+
    ;

matchCase
    : '(' matchPatterns ')' ':' statement
    ;

matchPatterns
    : patternElement (',' patternElement)*
    ;

patternElement
    : patternItem ('|' patternItem)*
    ;

patternItem
    : expression
    | range
    ;

range
    : expression '...' expression
    ;

tryCatch
    : 'try' block 'catch' block
    ;

structTryCatch
    : 'try' structBlock 'catch' structBlock
    ;

parExpression
    : '(' expression ')'
    ;

expression
    : primary
    |   ('+'|'-') expression
    |   ('~'|'!') expression
    | expression '.' IDENTIFIER
    | expression '::' IDENTIFIER
    | expression '[' expression ']'
    | expression '(' expressionList? ')'
    | 'addressof' '(' expression ')'
    | 'sizeof' '(' (expression | primitiveType) ')'
    | 'typenameof' '(' expression ')'
    | endian? primitiveType '(' expression ')'
    | expression ('*'|'/'|'%') expression
    | expression ('+'|'-') expression
    | expression ('+' | '-' | '*' | '/') expression
    | expression ('<<' | '>>>' | '>>') expression
    | expression ('<=' | '>=' | '>' | '<') expression
    | expression ('==' | '!=') expression
    | expression '^^' expression
    | expression '&' expression
    | expression '^' expression
    | expression '|' expression
    | expression '&&' expression
    | expression '||' expression
    | expression '?' expression ':' expression
    |   <assoc=right> expression
        (   '='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '&='
        |   '|='
        |   '^='
        |   '>>='
        |   '>>>='
        |   '<<='
        |   '%='
        )
        expression
    ;

expressionList
    : expression (',' expression)*
    ;

primary
    : '(' expression ')'
    | literal
    | IDENTIFIER
    | '$'
    ;

endian
    : 'le' | 'be'
    ;
io
    : 'in' | 'out'
    ;

literal
    : INTEGER_LITERAL
    | FLOAT_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOLEAN_LITERAL
    | CHARACTER_LITERAL
    ;

INTEGER_LITERAL
    : [0-9]+
    | HexIntegerLiteral
    | OCTAL_NUMERAL
    | BINARY_INTEGER_LITERAL
    ;

fragment
HexIntegerLiteral
    :   HexNumeral
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HEX_DIGIT (HexDigitOrUnderscore* HEX_DIGIT)?
    ;

fragment
HexDigitOrUnderscore
    :   HEX_DIGIT
    |   '\''
    ;

fragment
HEX_DIGIT
    :   [0-9a-fA-F]
    ;

fragment
OCTAL_INTEGER_LITERAL
    :   OCTAL_NUMERAL
    ;

fragment
OCTAL_NUMERAL
    :   '0' [oO] OCTAL_DIGITS
    ;

fragment
OCTAL_DIGITS
    :   OCTAL_DIGIT (OCTAL_DIGIT_OR_UNDERSCORE* OCTAL_DIGIT)?
    ;

fragment
OCTAL_DIGIT
    :   [0-7]
    ;

fragment
OCTAL_DIGIT_OR_UNDERSCORE
    :   OCTAL_DIGIT
    |   '\''
    ;

fragment
BINARY_INTEGER_LITERAL
    :   BINARY_NUMERAL
    ;

fragment
BINARY_NUMERAL
    :   '0' [bB] BINARY_DIGITS
    ;

fragment
BINARY_DIGITS
    :   BINARY_DIGIT (BINARY_DIGIT_OR_UNDERSCORE* BINARY_DIGIT)?
    ;

fragment
BINARY_DIGIT
    :   [01]
    ;

fragment
BINARY_DIGIT_OR_UNDERSCORE
    :   BINARY_DIGIT
    |   '\''
    ;

FLOAT_LITERAL
    : [0-9]* '.' [0-9]+ ([eE] [+-]? [0-9]+)?
    ;

CHAR_LITERAL
    : '\'' ( '\\'. | ~['\\] ) '\''
    ;

STRING_LITERAL
    : '"' ( '\\'. | ~["\\] )* '"'
    ;

BOOLEAN_LITERAL
    :   'true'
    |   'false'
    ;

CHARACTER_LITERAL
    :   '\'' SINGLE_CHARACTER '\''
    |   '\'' ESCAPE_SEQUENCE '\''
    ;

fragment
SINGLE_CHARACTER
    :   ~['\\]
    ;

fragment
ESCAPE_SEQUENCE
    :   '\\' [btnfr"'\\]
    |   OCTAL_ESCAPE
    |   UNICODE_ESCAPE
    ;

fragment
UNICODE_ESCAPE
    :   '\\' 'x' HEX_DIGIT HEX_DIGIT
    ;

fragment
OCTAL_ESCAPE
    :   '\\' OCTAL_DIGIT
    |   '\\' OCTAL_DIGIT OCTAL_DIGIT
    |   '\\' ZERO_TO_THREE OCTAL_DIGIT OCTAL_DIGIT
    ;

fragment
ZERO_TO_THREE
    :   [0-3]
    ;


IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

PREPROCESSOR
    : '#' ~[\r\n]* -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> channel(HIDDEN)
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> channel(HIDDEN)
    ;

WS
    : [ \t\r\n]+ -> channel(HIDDEN)
    ;