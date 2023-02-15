grammar Bicep;

// program -> statement* EOF
program
 : statement* EOF
 ;

// statement ->
//   targetScopeDecl |
//   importDecl |
//   metadataDecl |
//   parameterDecl |
//   typeDecl |
//   variableDecl |
//   resourceDecl |
//   moduleDecl |
//   outputDecl |
//   NL
statement
 : targetScopeDecl
 | importDecl
 | metadataDecl
 | parameterDecl
 | typeDecl
 | variableDecl
 | resourceDecl
 | moduleDecl
 | outputDecl
 | NL
 ;

// targetScopeDecl -> "targetScope" "=" expression
targetScopeDecl
 : TARGET_SCOPE ASSIGN expression
 ;

// importDecl -> decorator* "import" interpString(specification) importWithClause? importAsClause? NL
importDecl
 : decorator* IMPORT specification=interpString importWithClause? importAsClause? NL
 ;

// importWithClause -> "with" object
importWithClause
 : WITH object
 ;

// importAsClause -> "as" IDENTIFIER(alias)
importAsClause
 : AS alias=identifier
 ;

// metadataDecl -> "metadata" IDENTIFIER(name) "=" expression NL
metadataDecl
 : METADATA name=identifier ASSIGN expression NL
 ;

// parameterDecl ->
//   decorator* "parameter" IDENTIFIER(name) typeExpression parameterDefaultValue? NL |
//   decorator* "parameter" IDENTIFIER(name) "resource" interpString(type) parameterDefaultValue? NL |
parameterDecl
 : decorator* PARAM name=identifier ( typeExpression parameterDefaultValue?
                                    | RESOURCE type=interpString parameterDefaultValue?
                                    )
   NL
 ;

// parameterDefaultValue -> "=" expression
parameterDefaultValue
 : ASSIGN expression
 ;

// typeDecl -> decorator* "type" IDENTIFIER(name) "=" typeExpression NL
typeDecl
 :  decorator* TYPE name=identifier ASSIGN typeExpression NL
 ;

// variableDecl -> decorator* "variable" IDENTIFIER(name) "=" expression NL
variableDecl
 : decorator* VAR name=identifier ASSIGN expression NL
 ;

// resourceDecl -> decorator* "resource" IDENTIFIER(name) interpString(type) "existing"? "=" (ifCondition | object | forExpression) NL
resourceDecl
 : decorator* RESOURCE name=identifier type=interpString EXISTING? ASSIGN ( ifCondition | object | forExpression ) NL
 ;

// moduleDecl -> decorator* "module" IDENTIFIER(name) interpString(type) "=" (ifCondition | object | forExpression) NL
moduleDecl
 : decorator* MODULE name=identifier type=interpString ASSIGN ( ifCondition | object | forExpression ) NL
 ;

// outputDecl ->
//   decorator* "output" IDENTIFIER(name) IDENTIFIER(type) "=" expression NL
//   decorator* "output" IDENTIFIER(name) "resource" interpString(type) "=" expression NL
outputDecl
 : decorator* OUTPUT name=identifier ( type1=identifier
                                     | RESOURCE type2=interpString
                                     )
   ASSIGN expression NL
 ;

// decorator -> "@" decoratorExpression NL
decorator
 : AT decoratorExpression NL
 ;

// expression ->
//   binaryExpression |
//   binaryExpression "?" expression ":" expression
expression
 : binaryExpression ( QMARK expression COL expression )?
 ;

// binaryExpression ->
//   equalityExpression |
//   binaryExpression "&&" equalityExpression |
//   binaryExpression "||" equalityExpression |
//   binaryExpression "??" equalityExpression
binaryExpression
 : binaryExpression ( AND | OR | COALESCE ) equalityExpression
 | equalityExpression
 ;

// equalityExpression ->
//   relationalExpression |
//   equalityExpression "==" relationalExpression |
//   equalityExpression "!=" relationalExpression
equalityExpression
 : equalityExpression ( EQ | NEQ ) relationalExpression
 | relationalExpression
 ;

// relationalExpression ->
//   additiveExpression |
//   relationalExpression ">" additiveExpression |
//   relationalExpression ">=" additiveExpression |
//   relationalExpression "<" additiveExpression |
//   relationalExpression "<=" additiveExpression
relationalExpression
 : relationalExpression ( GT | GTE | LT | LTE ) additiveExpression
 | additiveExpression
 ;

// additiveExpression ->
//   multiplicativeExpression |
//   additiveExpression "+" multiplicativeExpression |
//   additiveExpression "-" multiplicativeExpression
additiveExpression
 : additiveExpression ( ADD | MIN ) multiplicativeExpression
 | multiplicativeExpression
 ;

// multiplicativeExpression ->
//   unaryExpression |
//   multiplicativeExpression "*" unaryExpression |
//   multiplicativeExpression "/" unaryExpression |
//   multiplicativeExpression "%" unaryExpression
multiplicativeExpression
 : multiplicativeExpression ( MUL | DIV | MOD ) unaryExpression
 | unaryExpression
 ;

// unaryExpression ->
//   memberExpression |
//   unaryOperator unaryExpression
unaryExpression
 : memberExpression
 | unaryOperator unaryExpression
 ;

// unaryOperator -> "!" | "-" | "+"
unaryOperator
 : NOT
 | MIN
 | ADD
 ;

//
// memberExpression ->
//   primaryExpression |
//   memberExpression "[" expression "]" |
//   memberExpression "." IDENTIFIER(property) |
//   memberExpression "." functionCall
//   memberExpression ":" IDENTIFIER(name)
memberExpression
 : memberExpression OBRACK expression CBRACK
 | memberExpression DOT property=identifier
 | memberExpression DOT functionCall
 | memberExpression COL name=identifier
 | primaryExpression
 ;

// primaryExpression ->
//   functionCall |
//   literalValue |
//   interpString |
//   multilineString |
//   array |
//   forExpression |
//   object |
//   parenthesizedExpression |
//   lambdaExpression
primaryExpression
 : functionCall
 | literalValue
 | interpString
 | MULTILINE_STRING
 | array
 | forExpression
 | object
 | parenthesizedExpression
 | lambdaExpression
 ;

// decoratorExpression -> functionCall | memberExpression "." functionCall
decoratorExpression
 : functionCall
 | memberExpression DOT functionCall
 ;

// functionCall -> IDENTIFIER "(" argumentList? ")"
functionCall
 : identifier OPAR argumentList? CPAR
 ;

// argumentList -> expression ("," expression)*
argumentList
 : expression ( COMMA expression )*
 ;

// parenthesizedExpression -> "(" expression ")"
parenthesizedExpression
 : OPAR expression CPAR
 ;

// lambdaExpression -> ( "(" argumentList? ")" | IDENTIFIER ) "=>" expression
lambdaExpression
 : ( OPAR argumentList? CPAR | identifier ) ARROW expression
 ;

// ifCondition -> "if" parenthesizedExpression object
ifCondition
 : IF parenthesizedExpression object
 ;

// forExpression -> "[" "for" (IDENTIFIER(item) | forVariableBlock) "in" expression ":" forBody "]"
forExpression
 : OBRACK FOR ( item=identifier | forVariableBlock ) IN expression COL forBody CBRACK
 ;

// forVariableBlock -> "(" IDENTIFIER(item) "," IDENTIFIER(index) ")"
forVariableBlock
 : OPAR item=identifier COMMA index=identifier CPAR
 ;

// forBody -> expression(body) | ifCondition
forBody
 : body=expression
 | ifCondition
 ;

// interpString ->  stringLeftPiece ( expression stringMiddlePiece )* expression stringRightPiece | stringComplete
interpString
 : STRING_LEFT_PIECE ( expression STRING_MIDDLE_PIECE )* expression STRING_RIGHT_PIECE
 | STRING_COMPLETE
 ;

// literalValue -> NUMBER | "true" | "false" | "null"
literalValue
 : NUMBER
 | TRUE
 | FALSE
 | NULL
 | identifier
 ;

// object -> "{" ( NL+ ( objectProperty NL+ )* )? "}"
object
 : OBRACE ( NL+ ( objectProperty NL+ )* )? CBRACE
 ;

// objectProperty -> ( IDENTIFIER(name) | interpString ) ":" expression
objectProperty
 : ( name=identifier | interpString ) COL expression
 ;

// array -> "[" ( NL+ arrayItem* )? "]"
array
 : OBRACK ( NL+ arrayItem* )? CBRACK
 ;

// arrayItem -> expression NL+
arrayItem
 : expression NL+
 ;

// typeExpression -> singularTypeExpression ("|" singularTypeExpression)*
typeExpression
 : singularTypeExpression ( PIPE singularTypeExpression )*
 ;

// singularTypeExpression ->
//   primaryTypeExpression |
//   singularTypeExpression "[]" |
//   parenthesizedTypeExpression
singularTypeExpression
 : primaryTypeExpression
 | singularTypeExpression OBRACK CBRACK
 | parenthesizedTypeExpression
 ;

// primaryTypeExpression ->
//   ambientTypeReference |
//   IDENTIFIER(type) |
//   literalValue |
//   unaryOperator literalValue |
//   stringComplete |
//   multilineString |
//   objectType |
//   tupleType
primaryTypeExpression
 : ambientTypeReference
 | type=identifier
 | unaryOperator? literalValue
 | STRING_COMPLETE
 | MULTILINE_STRING
 | objectType
 | tupleType
 ;

// ambientTypeReference -> "string" | "int" | "bool" | "array" | "object"
ambientTypeReference
 : STRING
 | INT
 | INT
 | ARRAY
 | OBJECT
 ;

// objectType -> "{" (NL+ ((objectTypeProperty | objectTypeAdditionalPropertiesMatcher) NL+ )* )? "}"
objectType
 : OBRACE (NL+ ( ( objectTypeProperty | objectTypeAdditionalPropertiesMatcher ) NL+ )* )? CBRACE
 ;

// objectTypeProperty -> decorator* ( IDENTIFIER(name) | stringComplete | multilineString ) ":" typeExpression
objectTypeProperty
 : decorator* ( name=identifier | STRING_COMPLETE | MULTILINE_STRING ) COL typeExpression
 ;

// objectTypeAdditionalPropertiesMatcher -> decorator* "*:" typeExpression
objectTypeAdditionalPropertiesMatcher
 : decorator* STAR_COL typeExpression
 ;

// tupleType -> "[" (NL+ tupleItem* )? "]"
tupleType
 : OBRACK ( NL+ tupleItem* )? CBRACK
 ;

// tupleItem -> decorator* typeExpression NL+
tupleItem
 : decorator* typeExpression NL+
 ;

// parenthesizedTypeExpression -> "(" typeExpression ")"
parenthesizedTypeExpression
 : OPAR typeExpression CPAR
 ;

identifier
 : IDENTIFIER
 | IMPORT
 | WITH
 | AS
 | METADATA
 | PARAM
 | RESOURCE
 | MODULE
 | OUTPUT
 | EXISTING
 | TYPE
 | VAR
 | IF
 | FOR
 | IN
 | TRUE
 | FALSE
 | NULL
 | TARGET_SCOPE
 | STRING
 | INT
 | BOOL
 | ARRAY
 | OBJECT
 ;

// disableNextLineDiagnosticsDirective-> #disable-next-line diagnosticCode1 diagnosticCode2 diagnosticCode3 NL
DISABLE_NEXT_LINE_DIAGNOSTIC_DIRECTIVE
 : '#disable-next-line' ~[\r\n]+ NL -> skip
 ;

SINGLE_LINE_COMMENT
 : '//' ~[\r\n]* -> skip
 ;

MULTI_LINE_COMMENT
 : '/*' .*? '*/' -> skip
 ;

// multilineString -> "'''" + MULTILINESTRINGCHAR+ + "'''"
MULTILINE_STRING
 : '\'\'\'' .*? '\'\'\''
 ;

// stringLeftPiece -> "'" STRINGCHAR* "${"
STRING_LEFT_PIECE
 : '\'' STRINGCHAR* '${'
 ;

// stringMiddlePiece -> "}" STRINGCHAR* "${"
STRING_MIDDLE_PIECE
 : '}' STRINGCHAR* '${'
 ;

// stringRightPiece -> "}" STRINGCHAR* "'"
STRING_RIGHT_PIECE
 : '}' STRINGCHAR* '\''
 ;

// stringComplete -> "'" STRINGCHAR* "'"
STRING_COMPLETE
 : '\'' STRINGCHAR* '\''
 ;

ARROW : '=>';
AT : '@';
COMMA : ',';
PIPE : '|';
STAR_COL : '*:';
OBRACK  : '[';
CBRACK : ']';
OPAR : '(';
CPAR : ')';
DOT : '.';
NOT : '!';
MUL : '*';
DIV : '/';
MOD : '%';
ADD : '+';
MIN : '-';
GT : '>';
GTE : '>=';
LT : '<';
LTE : '<=';
EQ : '==';
NEQ : '!=';
AND : '&&';
OR : 'OR';
COALESCE : '??';
QMARK : '?';
COL : ':';
ASSIGN : '=';
OBRACE : '{';
CBRACE : '}';

IMPORT : 'import';
WITH : 'with';
AS : 'as';
METADATA : 'metadata';
PARAM : 'param';
RESOURCE : 'resource';
MODULE : 'module';
OUTPUT : 'output';
EXISTING : 'existing';
TYPE : 'type';
VAR : 'var';
IF : 'if';
FOR : 'for';
IN : 'in';
TRUE : 'true';
FALSE : 'false';
NULL : 'null';
TARGET_SCOPE : 'targetScope';
STRING : 'string';
INT : 'int';
BOOL : 'bool';
ARRAY : 'array';
OBJECT : 'object';

IDENTIFIER : [a-zA-Z_] [a-zA-Z_0-9]*;

NUMBER : [0-9]+ ( '.' [0-9]+ )?;

// NL -> ("\n" | "\r")+
NL : [\r\n]+;

SPACES : [ \t]+ -> skip;

UNKNOWN : .;

// https://learn.microsoft.com/en-us/azure/azure-resource-manager/bicep/data-types#strings
fragment STRINGCHAR
 : ~[\\'\n\r\t$]
 | ESCAPE
 ;

fragment ESCAPE
 : '\\' ( [\\'nrt$] | 'u{' HEX+ '}' )
 ;

fragment HEX
 : [0-9a-fA-F]
 ;