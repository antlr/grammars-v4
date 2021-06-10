parser grammar GDScriptParser;

options {
    tokenVocab=GDScriptLexer;
}

program
    : (inheritance NEWLINE)? className? topLevelDecl* EOF
    ;

inheritance
    : 'extends' (IDENTIFIER | STRING) ('.' IDENTIFIER)*
    ;
className
    : 'class_name' IDENTIFIER (',' STRING)? NEWLINE
    ;

topLevelDecl
    : classVarDecl
    | constDecl
    | signalDecl
    | enumDecl
    | methodDecl
    | constructorDecl
    | innerClass
    | 'tool'
    ;

classVarDecl
    : 'onready'? export? 'var' IDENTIFIER ((':' typeHint)? ('=' expression)? | ':=' expression) setget? NEWLINE
    ;
setget
    : 'setget' IDENTIFIER? (',' IDENTIFIER)?
    ;
export
    : 'export' ('('(BUILTINTYPE | IDENTIFIER (',' literal)*)?')')?
    ;
typeHint
    : BUILTINTYPE
    | IDENTIFIER
    ;

constDecl
    : 'const' IDENTIFIER (':' typeHint)? '=' expression NEWLINE
    ;

signalDecl
    : 'signal' IDENTIFIER signalParList? NEWLINE
    ;
signalParList
    : '(' (IDENTIFIER (',' IDENTIFIER)*)? ')'
    ;

enumDecl
    : 'enum' IDENTIFIER? '{' (IDENTIFIER ('=' expression)? (',' IDENTIFIER ('=' expression)?)* ','?) '}' NEWLINE
    ;

methodDecl
    : rpc? 'static'? 'func' IDENTIFIER '(' parList? ')' ('->' typeHint)? ':' stmtOrSuite
    ;
parList
    : parameter (',' parameter)?
    ;
parameter
    : 'var'? IDENTIFIER (':' typeHint)? ('=' expression)?
    ;
rpc
    : 'remote'
    | 'master'
    | 'puppet'
    | 'remotesync'
    | 'mastersync'
    | 'puppetsync'
    ;

constructorDecl
    : 'func' IDENTIFIER '(' parList? ')' ('.' '(' argList? ')') ':' stmtOrSuite
    ;
argList
    : expression (',' expression)*
    ;

innerClass
    : 'class' IDENTIFIER inheritance? ':' NEWLINE
        INDENT (inheritance NEWLINE)? topLevelDecl+
        DEDENT
    ;

stmtOrSuite
    : stmt
    | NEWLINE
        INDENT suite
        DEDENT
    ;
suite
    : stmt+
    ;

stmt
    : varDeclStmt
    | ifStmt
    | forStmt
    | whileStmt
    | matchStmt
    | flowStmt
    | assignmentStmt
    | exprStmt
    | assertStmt
    | yieldStmt
    | preloadStmt
    | 'breakpoint' stmtEnd
    | 'pass' stmtEnd
    ;
stmtEnd
    : NEWLINE
    | ';'
    ;

ifStmt
    : 'if' expression ':' stmtOrSuite ('elif' expression ':' stmtOrSuite)* ('else' ':' stmtOrSuite)?
    ;
whileStmt
    : 'while' expression ':' stmtOrSuite
    ;
forStmt
    : 'for' IDENTIFIER 'in' expression ':' stmtOrSuite
    ;

matchStmt
    : 'match' expression NEWLINE
        INDENT matchBlock
        DEDENT
    ;
matchBlock
    : (patternList ':' stmtOrSuite)+
    ;
patternList
    : pattern (',' pattern)*
    ;
// Note: you can't have a binding in a pattern list, but to not complicate the grammar more
// it won't be restricted syntactically
pattern
    : literal
    | BUILTINTYPE
    | CONSTANT
    | '_'
    | bindingPattern
    | arrayPattern
    | dictPattern
    ;
bindingPattern
    : 'var' IDENTIFIER
    ;
arrayPattern
    : '[' (pattern (',' pattern)* '..'?)? ']'
    ;
dictPattern
    : '{' keyValuePattern? (',' keyValuePattern)* '..'? '}'
    ;
keyValuePattern
    : STRING (':' pattern)?
    ;

flowStmt
    : 'continue' stmtEnd
    | 'break' stmtEnd
    | 'return' expression? stmtEnd
    ;

assignmentStmt
    : subscription ('='|'+='|'-='|'*='|'/='|'%='|'&='|'|='|'^=') expression stmtEnd
    ;
varDeclStmt
    : 'var' IDENTIFIER ('=' expression)? stmtEnd
    ;

assertStmt
    : 'assert' '(' expression (',' STRING)? ')' stmtEnd
    ;
yieldStmt
    : 'yield' '(' (expression ',' expression) ')'
    ;
preloadStmt
    : 'preload' '(' CONSTANT ')'
    ;
// This expression grammar encodes precedence. Items later in the list have
// higher precedence than the ones before.
exprStmt
    : expression stmtEnd
    ;
expression
    : cast ('[' expression ']')?
    ;
cast
    : ternaryExpr ('as' typeHint)?
    ;
ternaryExpr
    : logicOr ('if' logicOr 'else' logicOr)?
    ;
logicOr
    : logicAnd (('or' | '||') logicAnd)*
    ;
logicAnd
    : logicNot (('and' | '&&') logicNot)*
    ;
logicNot
    : ('!' | 'not') logicNot | in
    ;
in
    : comparison ('in' comparison)*
    ;
comparison
    : bitOr (('<' | '>' | '<=' | '>=' | '==' | '!=' )bitOr)*
    ;
bitOr
    : bitXor ('|' bitXor)*
    ;
bitXor
    : bitAnd ('^' bitAnd)*
    ;
bitAnd
    : bitShift ('&' bitShift)*
    ;
bitShift
    : minus (('<<'|'>>')minus)*
    ;
minus
    : plus ('-' plus)*
    ;
plus
    : factor ('+' factor)*
    ;
factor
    : sign (('*'|'/'|'%') sign)*
    ;
sign
    : ('-'|'+') sign | bitNot
    ;
bitNot
    : '~' bitNot | is
    ;
is
    : call ('is' (IDENTIFIER | BUILTINTYPE))?
    ;
call
    : attribute ( '(' argList? ')')?
    | '.' IDENTIFIER ( '(' argList? ')')?
    | getNode
    ;
attribute
    : subscription ('.' IDENTIFIER)*
    ;
subscription
    : primary ('[' expression ']')?
    ;
primary
    : 'true'
    | 'false'
    | 'null'
    | 'self'
    | literal
    | arrayDecl
    | dictDecl
    | '(' expression ')'
    ;

literal
    : STRING
    | INTEGER
    | FLOAT
    | IDENTIFIER
    | BUILTINTYPE
    | CONSTANT
    ;
arrayDecl
    : '[' (expression (',' expression)*)? ']'
    ;
dictDecl
    : '{' (keyValue (',' keyValue)*)? '}'
    ;
keyValue
    : expression ':' expression
    | IDENTIFIER '=' expression
    ;

getNode
    : '$' (STRING | IDENTIFIER ('/' IDENTIFIER)*)
    ;