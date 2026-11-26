/*
BSD License

Parser grammar for the Koka programming language (https://koka-lang.github.io/koka/).
See KokaLexer.g4 for background: this grammar targets Koka's explicit
'{ '}' ';' syntax rather than its layout-sensitive sugar (both are accepted
by the real compiler; see README.md).

The real Koka grammar (doc/spec/grammar/parser.y) also lets user-defined
operators carry their own precedence and associativity via "infix" /
"infixl" / "infixr" declarations, which a static context-free grammar
cannot honor without semantic information. This grammar therefore combines
all binary operator applications at a single, flat, left-associative level
rather than guessing a fixed precedence ladder.

Entry rule: program
*/

parser grammar KokaParser;

options {
    tokenVocab = KokaLexer;
}

program
    : moduleHeader? topItems? EOF
    ;

moduleHeader
    : MODULE modPath SEMI?
    ;

modPath
    : IDENTIFIER (SLASH IDENTIFIER)*
    ;

topItems
    : topDecl (SEMI? topDecl)* SEMI?
    ;

topDecl
    : importDecl
    | fixityDecl
    | funcDecl
    | valDecl
    | aliasDecl
    | structDecl
    | typeDecl
    | effectDecl
    ;

importDecl
    : IMPORT modPath (EQ modPath)?
    ;

fixityDecl
    : (INFIX | INFIXL | INFIXR) INT_LIT? LPAREN opName RPAREN
    ;

opName
    : OP
    | LT
    | GT
    | PIPE
    | SLASH
    ;

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

funcDecl
    : PUB? (FUN | FN) IDENTIFIER genericParams? LPAREN paramList? RPAREN (COLON type)? (EQ expr | block)
    ;

genericParams
    : LT typeParam (COMMA typeParam)* GT
    ;

typeParam
    : IDENTIFIER (DCOLON kind)?
    ;

kind
    : IDENTIFIER
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER (COLON type)? (EQ expr)?
    ;

valDecl
    : PUB? (VAL | VAR) pattern (COLON type)? EQ expr
    ;

aliasDecl
    : PUB? ALIAS IDENTIFIER genericParams? EQ type
    ;

structDecl
    : PUB? STRUCT IDENTIFIER genericParams? LBRACE fieldList? RBRACE
    ;

fieldList
    : fieldDecl (SEMI fieldDecl)* SEMI?
    ;

fieldDecl
    : IDENTIFIER COLON type
    ;

typeDecl
    : PUB? TYPE IDENTIFIER genericParams? LBRACE conList? RBRACE
    ;

conList
    : conDecl (SEMI conDecl)* SEMI?
    ;

conDecl
    : PUB? CON? CONID (LPAREN fieldList? RPAREN)?
    ;

effectDecl
    : PUB? EFFECT IDENTIFIER genericParams? LBRACE effectOpList? RBRACE
    ;

effectOpList
    : effectOpDecl (SEMI effectOpDecl)* SEMI?
    ;

effectOpDecl
    : (FUN | CTL) IDENTIFIER LPAREN paramList? RPAREN COLON type
    ;

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

pattern
    : IDENTIFIER (AS IDENTIFIER)?
    | CONID (LPAREN pattern (COMMA pattern)* RPAREN)?
    | LPAREN pattern (COMMA pattern)* RPAREN
    | LBRACK pattern (COMMA pattern)* RBRACK
    | constant
    | WILDCARD
    ;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type
    : effectRow? typeApp (ARROW effectRow? type)?
    ;

effectRow
    : LT (typeApp (COMMA typeApp)* (PIPE typeApp)?)? GT
    ;

typeApp
    : CONID typeArgs?
    | IDENTIFIER typeArgs?
    | LPAREN type (COMMA type)* RPAREN
    | LBRACK type RBRACK
    ;

typeArgs
    : LT type (COMMA type)* GT
    ;

// ---------------------------------------------------------------------------
// Statements and blocks
// ---------------------------------------------------------------------------

block
    : LBRACE blockItems? RBRACE
    ;

blockItems
    : blockItem (SEMI blockItem)* SEMI?
    ;

blockItem
    : valDecl
    | varAssignStat
    | withStat
    | returnStat
    | expr
    ;

varAssignStat
    : postfixExpr ASSIGN_OP expr
    ;

withStat
    : WITH (pattern LARROW)? expr
    ;

returnStat
    : RETURN expr?
    ;

// ---------------------------------------------------------------------------
// Expressions (a single flat, left-associative binary-operator level - see
// the file header for why. Highest to lowest otherwise: unary prefix,
// postfix member/index/call, everything else.)
// ---------------------------------------------------------------------------

expr
    : opExpr
    ;

opExpr
    : opExpr (OP | LT | GT | PIPE | SLASH) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : OP unaryExpr
    | postfixExpr
    ;

postfixExpr
    : postfixExpr DOT INT_LIT
    | postfixExpr DOT IDENTIFIER call?
    | postfixExpr DOT LPAREN argList? RPAREN
    | postfixExpr call
    | postfixExpr LBRACK expr RBRACK
    | factor
    ;

call
    : LPAREN argList? RPAREN
    ;

argList
    : arg (COMMA arg)*
    ;

arg
    : (IDENTIFIER EQ)? expr
    ;

factor
    : LPAREN expr (COMMA expr)* RPAREN
    | LBRACK (expr (COMMA expr)*)? RBRACK
    | ifExpr
    | matchExpr
    | handleExpr
    | handlerExpr
    | lambdaExpr
    | constant
    | CONID
    | IMPLICIT_ID
    | WILDCARD
    | IDENTIFIER
    ;

ifExpr
    : IF expr THEN (block | expr) (ELIF expr THEN (block | expr))* (ELSE (block | expr))?
    ;

matchExpr
    : MATCH expr LBRACE matchArmList RBRACE
    ;

matchArmList
    : matchArm (SEMI matchArm)* SEMI?
    ;

matchArm
    : pattern ARROW (block | expr)
    ;

handleExpr
    : HANDLE LPAREN expr RPAREN LBRACE handlerClauseList? RBRACE
    ;

handlerExpr
    : HANDLER LBRACE handlerClauseList? RBRACE
    ;

handlerClauseList
    : handlerClause (SEMI handlerClause)* SEMI?
    ;

handlerClause
    : (FUN | CTL) IDENTIFIER LPAREN paramList? RPAREN ARROW (block | expr)
    | FINAL CTL IDENTIFIER LPAREN paramList? RPAREN ARROW (block | expr)
    | RETURN LPAREN pattern RPAREN ARROW (block | expr)
    ;

lambdaExpr
    : FN LPAREN paramList? RPAREN (COLON type)? block
    | FN LPAREN paramList? RPAREN (COLON type)? ARROW expr
    ;

constant
    : INT_LIT
    | FLOAT_LIT
    | STRING_LIT
    | RAW_STRING_LIT
    | CHAR_LIT
    ;
