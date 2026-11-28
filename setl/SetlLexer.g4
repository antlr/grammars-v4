lexer grammar SetlLexer;

// SETL ("SET Language") is a very-high-level programming language built
// around finite sets and tuples as first-class data structures, designed in
// the late 1960s by Jacob T. Schwartz at NYU. This grammar models the
// classical SETL language described in "Programming with Sets: An
// Introduction to SETL" (Schwartz, Dewar, Dubinsky, Schonberg, 1986):
// PROGRAM/PROCEDURE blocks, imperative statements (IF/WHILE/FOR/CASE),
// set/tuple formers, quantifiers (EXISTS/FORALL), and the set-theoretic
// operators (UNION, INTER, WITH, LESS, IN, SUBSET, ...). See README.md for
// the simplifications made where secondary sources disagreed or later
// dialects (e.g. GNU SETL) diverged from the classical language.

PROGRAM: 'program';
PROCEDURE: 'procedure';
END: 'end';
RETURN: 'return';
VAR: 'var';
CONST: 'const';
IF: 'if';
THEN: 'then';
ELSE: 'else';
ELSEIF: 'elseif';
WHILE: 'while';
UNTIL: 'until';
LOOP: 'loop';
FOR: 'for';
IN: 'in';
NOTIN: 'notin';
CASE: 'case';
OF: 'of';
OTHERWISE: 'otherwise';
PRINT: 'print';
EXISTS: 'exists';
FORALL: 'forall';
UNION: 'union';
INTER: 'inter';
WITH: 'with';
LESS: 'less';
SUBSET: 'subset';
MOD: 'mod';
DIV: 'div';
AND: 'and';
OR: 'or';
NOT: 'not';
TRUE_: 'true';
FALSE_: 'false';
OM: 'om';

IDENTIFIER: [A-Za-z] [A-Za-z0-9_]*;

INT_LIT: DIGIT+;
REAL_LIT: DIGIT+ '.' DIGIT+ (('e'|'E') ('+'|'-')? DIGIT+)?;
fragment DIGIT: [0-9];

STRING_LIT: '"' (~["\r\n])* '"';

ASSIGN: ':=';
ARROW: '=>';
RANGE: '..';
CARD: '#';
NE: '/=';
LE: '<=';
GE: '>=';
EQ: '=';
LT: '<';
GT: '>';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
LBRACK: '[';
RBRACK: ']';
COMMA: ',';
SEMI: ';';
COLON: ':';
PIPE: '|';
DOT: '.';

LINE_COMMENT: '$' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n\f]+ -> skip;
