/**
 * Kotlin language Antl4 grammar
 * based on Kotlin's official grammar https://kotlinlang.org/docs/reference/grammar.html
 */

lexer grammar KotlinLexer;

MULTILINE_COMMENT
    : '/*' .*? '*/' -> channel(HIDDEN)
    ;

SINGLELINE_COMMENT
    : '//' .*? '\n' -> channel(HIDDEN)
    ;

WHITESPACE
    : [\t\n\r\f ]+ -> channel(HIDDEN)
;

fragment
DIGIT
    : '0' .. '9'
    ;

IntegerLiteral
    : ('0' | '1'..'9' DIGIT*)
    ;

fragment
HEX_DIGIT
    :  (DIGIT | 'A' .. 'F' | 'a' .. 'f')
    ;

SEMI
    : ';';

OPEN_BLOCK
    : '{' -> pushMode(DEFAULT_MODE)
    ;

CLOSE_BLOCK
    : '}' -> popMode
    ;

HexadecimalLiteral
    : '0x' HEX_DIGIT+
    ;

fragment
LETTER
    : 'a' .. 'z'
    | 'A' .. 'Z'
    | '_'
    ;

fragment
EscapeChar
    :   'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

CharacterLiteral
    : '\'' (EscapeChar | .) '\'';

TRIPLE_QUOTE
    : '"""' -> pushMode(InMultiLineString)
    ;

SINGLE_QUOTE
    : '"' -> pushMode(InSingleLineString)
    ;

// keywords
PACKAGE
    : 'package';

IMPORT
    : 'import';

DOT
    : '.';

STAR : '*';

COMMA : ',';

LT : '<';
LTE : '<=';
GT : '>';
GTE : '>=';
EQ : '=';
EQ_EQ : '==';
EQ_EQ_EQ : '===';
NEQ : '!=';

COLON : ':';

BRACE_OPEN : '(';
BRACE_CLOSE : ')';

Q : '?';
DA : '!!.';

DISJ : '||';
CONJ: '&&';

ELVIS : '?:' ;
LONG_RANGE : '...';
RANGE : '..';

REFERENCE : '::';

TRUE : 'true';
FALSE : 'false';
NULL : 'null';

OP_ASTERISK : '->';
OP_DIV : '/';
OP_MOD : '%';
OP_PLUS : '+';
OP_MUNUS : '-';

OP_IN : 'in';
OP_NOT_IN: '!in';
OP_IS : 'is';
OP_NOT_IS: '!is';
OP_AS: 'as';
OP_AS_SAFE: 'as?';

OP_PLUS_ASSIGN: '+=';
OP_MINUS_ASSIGN: '-=';
OP_MULT_ASSIGN: '*=';
OP_DIV_ASSIGN: '/=';
OP_MOD_ASSIGN: '%=';

OP_DECREMENT: '--';
OP_INCREMENT: '++';

OP_NULL_ASSERT: '!!';
OP_NOT: '!';

SQ_OPEN: '[';
SQ_CLOSE: ']';

KEYWORD_val : 'val';
KEYWORD_var : 'var';

KEYWORD_by : 'by';

KEYWORD_dynamic : 'dynamic';
KEYWORD_where : 'where';

GET : 'get';
SET : 'set';

HierarchyModifier_abstract : 'abstract';
HierarchyModifier_open     : 'open';
HierarchyModifier_final    : 'final';
HierarchyModifier_override : 'override';

ClassModifier_enum : 'enum';
ClassModifier_annotation : 'annotation';
ClassModifier_data : 'data';

AccessModifier_private : 'private';
AccessModifier_protected : 'protected';
AccessModifier_public : 'public';
AccessModifier_internal : 'internal';


//VarianceAnnotation_in : 'in';
VarianceAnnotation_out : 'out';


DOG : '@';

AnnotationUseSiteTarget_file: 'file';
AnnotationUseSiteTarget_field: 'field';
AnnotationUseSiteTarget_property: 'property';
AnnotationUseSiteTarget_param: 'param';
AnnotationUseSiteTarget_sparam: 'sparam';

Jump_throw: 'throw';
Jump_continue: 'continue';
Jump_return: 'return';
Jump_break: 'break';

KEYWORD_constructor: 'constructor';

ConstructorDelegationCall_this: 'this';
ConstructorDelegationCall_super: 'super';

Declaration_class: 'class';
Declaration_interface: 'interface';
Declaration_object: 'object';
Declaration_companion: 'companion';
Declaration_fun: 'fun';
Declaration_init: 'init';

CF_if: 'if';
CF_else: 'else';
CF_when: 'when';
CF_while: 'while';
CF_for: 'for';
CF_do: 'do';
CF_try: 'try';
CF_catch: 'catch';
CF_FINALLY: 'finally';

BAX: '$';

SimpleName
       :     LETTER (LETTER | DIGIT)*
    |    '`' ~('`')+? '`'
    ;


mode InSingleLineString;

SINLE_QUOTE_WHITESPACE
    : [\t\n\r\f ]+
;

SINGLE_TEXT
    : ~('\\' | '"' | '$')+;

SINLE_QUOTE_CLOSE
    : '"' -> popMode;

SINLE_QUOTE_ESCAPED_CHAR
    : '\\' (EscapeChar | .);

SINLE_QUOTE_EXPRESSION_START
    : '${' -> pushMode(DEFAULT_MODE);

SINGLE_QUOTE_REF
    : '$' LETTER (LETTER | DIGIT)*
    ;

mode InMultiLineString;

MULTILINE_QUOTE_TEXT
    : ~('"' | '$')+
    ;

MULTILINE_QUOTE_CLOSE
    : '"""' -> popMode;

MULTILINE_QUOTES
    : '""' | '"';

MULTILINE_QUOTE_EXPRESSION_START
    : '${' -> pushMode(DEFAULT_MODE);

MULTILINE_QUOTE_REF
    : '$' LETTER (LETTER | DIGIT)*
    ;

