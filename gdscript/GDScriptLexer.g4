lexer grammar GDScriptLexer
	;

tokens {
	INDENT,
	DEDENT
}

options {
	superClass = GDScriptLexerBase;
}

EXTENDS: 'extends';
CLASS_NAME: 'class_name';
ONREADY: 'onready';
VAR: 'var';
SETGET: 'setget';
EXPORT: 'export';
CONST: 'const';
SIGNAL: 'signal';
ENUM: 'enum';
STATIC: 'static';
FUNC: 'func';
REMOTE: 'remote';
MASTER: 'master';
PUPPET: 'puppet';
REMOTESYNC: 'remotesync';
MASTERSYNC: 'mastersync';
PUPPETSYNC: 'puppetsync';
CLASS: 'class';
BREAKPOINT: 'breakpoint';
PASS: 'pass';
IF: 'if';
ELIF: 'elif';
WHILE: 'while';
FOR: 'for';
IN: 'in';
MATCH: 'match';
CONTINUE: 'continue';
BREAK: 'break';
RETURN: 'return';
ASSERT: 'assert';
YIELD: 'yield';
PRELOAD: 'preload';
AS: 'as';
ELSE: 'else';
OR: 'or';
AND: 'and';
NOT: 'not';
IS: 'is';
TRUE: 'true';
FALSE: 'false';
NULL: 'null';
SELF: 'self';
TOOL: 'tool';

NEWLINE
	: (
		{atStartOfInput()}? SPACE
		| ( '\r'? '\n' | '\r' | '\f') SPACE?
	) {onNewLine();}
	;

IDENTIFIER
	: [_a-zA-Z][_0-9a-zA-Z]*
	;
BUILTINTYPE
	: 'bool'
	| 'int'
	| 'float'
	| 'String'
	| 'Vector2'
	| 'Vector2i'
	| 'Rect2'
	| 'Rect2i'
	| 'Transform2D'
	| 'Vector3'
	| 'Vector3i'
	| 'AABB'
	| 'Plane'
	| 'Quat'
	| 'Basis'
	| 'Transform'
	| 'Color'
	| 'RID'
	| 'Object'
	| 'StringName'
	| 'NodePath'
	| 'Dictionary'
	| 'Callable'
	| 'Signal'
	| 'Array'
	| 'PackedByteArray'
	| 'PackedInt32Array'
	| 'PackedInt64Array'
	| 'PackedFloat32Array'
	| 'PackedFloat64Array'
	| 'PackedStringArray'
	| 'PackedVector2Array'
	| 'PackedVector3Array'
	| 'PackedColorArray'
	;
CONSTANT // TODO: really?
	: 'PI'
	| 'TAU'
	| 'INF'
	| 'NAN'
	;

STRING
	: '"' STRING_CONTENT* '"'
	| '\'' STRING_CONTENT* '\''
	;
fragment STRING_CONTENT
	: ~[\\\r\n'"]
	| '\\' [abfnrtv'"\\]
	| '\\u' HEX HEX HEX HEX
	;

INTEGER
	: '0' [xX] HEX+
	| DEC+
	| '0' [bB] [01]+
	;
fragment DEC
	: [0-9]
	;
fragment HEX
	: [0-9a-fA-F]
	;
FLOAT
	: DEC? '.' DEC ([eE] [+-] DEC)?
	| DEC [eE] [+-] DEC
	;

DOT: '.';
COMMA: ',';
COLON: ':';
ASSIGN: '=';
COLON_ASSIGN: ':=';
ADD_ASSIGN: '+=';
MINUS_ASSIGN: '-=';
MUL_ASSIGN: '*=';
DIV_ASSIGN: '/=';
MOD_ASSIGN: '%=';
AND_ASSIGN: '&=';
OR_ASSIGN: '|=';
XOR_ASSIGN: '^=';
OPEN_PAREN: '(' {openBrace();};
CLOSE_PAREN: ')' {closeBrace();};
OPEN_BRACE: '{' {openBrace();};
CLOSE_BRACE: '}' {closeBrace();};
ARROW: '->';
UNDERSCORE: '_';
OPEN_BRACK: '[' {openBrace();};
CLOSE_BRACK: ']' {closeBrace();};
DOTDOT: '..';
SEMI_COLON : ';';
LOGIC_OR: '||';
LOGIC_AND: '&&';
LOGIC_NOT: '!';
LESS_THAN: '<';
GREATER_THAN: '>';
EQUALS: '==';
GT_EQ: '>=';
LT_EQ: '<=';
NOT_EQ: '!=';
OR_OP: '|';
XOR: '^';
AND_OP: '&';
LEFT_SHIFT: '<<';
RIGHT_SHIFT: '>>';
ADD: '+';
MINUS: '-';
STAR: '*';
DIV: '/';
MOD: '%';
NOT_OP: '~';
DOLLAR: '$';

SKIP_
	: (SPACE | COMMENT | LINE_JOINING) -> skip
	;

fragment SPACE
	: [ \t]+
	;

fragment COMMENT
	: '#' ~[\r\n]*
	;

fragment LINE_JOINING
	: '\\' SPACE? ('\r\n' | [\r\n])
	;