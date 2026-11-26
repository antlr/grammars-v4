/*
BSD License

Parser grammar for the Vale programming language (https://vale.dev/,
https://github.com/ValeLang/Vale). See ValeLexer.g4 for background.

The vale.dev guide is a terse, partial reference for an alpha version of the
language, and several points are underspecified or only described as
"planned" (full attribute/modifier syntax, operator precedence, "else if",
a formal lexical grammar, etc). This grammar fills those gaps with the
common, unambiguous C-family reading: a conventional left-to-right binary
operator precedence ladder, braces for all blocks, and the documented
examples (constructors, generics, region/ownership-prefixed types,
static/runtime array literals, match, foreach, pattern destructuring) taken
as the grammar to support.

Entry rule: program
*/

parser grammar ValeParser;

options {
    tokenVocab = ValeLexer;
}

program
    : topDecl* EOF
    ;

// ---------------------------------------------------------------------------
// Top level
// ---------------------------------------------------------------------------

topDecl
    : importDecl
    | funcDecl
    | structDecl
    | interfaceDecl
    | implDecl
    ;

importDecl
    : IMPORT IDENTIFIER (DOT IDENTIFIER)* DOT STAR SEMI
    ;

attribute
    : HASHBANG IDENTIFIER
    ;

funcDecl
    : attribute* EXPORTED? FUNC IDENTIFIER genericParams? LPAREN paramList? RPAREN type? (block | SEMI)
    ;

genericParams
    : LT genericParam (COMMA genericParam)* GT
    ;

genericParam
    : IDENTIFIER QUOTE
    | IDENTIFIER (IMPL type)?
    ;

paramList
    : param (COMMA param)*
    ;

param
    : pattern
    ;

structDecl
    : attribute* WEAKABLE? STRUCT IDENTIFIER genericParams? IMM? LBRACE structMember* RBRACE
    ;

structMember
    : IDENTIFIER type SEMI
    | funcDecl
    ;

interfaceDecl
    : INTERFACE IDENTIFIER genericParams? SEALED? LBRACE interfaceMember* RBRACE
    ;

interfaceMember
    : FUNC IDENTIFIER genericParams? LPAREN ifaceParamList? RPAREN type? SEMI
    ;

ifaceParamList
    : ifaceParam (COMMA ifaceParam)*
    ;

ifaceParam
    : VIRTUAL? (SELF | THIS | IDENTIFIER) type?
    ;

implDecl
    : IMPL IDENTIFIER (FOR IDENTIFIER)? SEMI
    | IMPL IDENTIFIER LBRACE funcDecl* RBRACE
    ;

// ---------------------------------------------------------------------------
// Patterns (used for params, local bindings, destructuring, match arms)
// ---------------------------------------------------------------------------

pattern
    : (SELF | THIS | IDENTIFIER) type?
    | IDENTIFIER LBRACK pattern (COMMA pattern)* RBRACK
    | LBRACK pattern (COMMA pattern)* RBRACK
    ;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type
    : typePrefix? typeCore
    ;

typePrefix
    : AMP AMP
    | AMP
    | CARET
    | QUESTION AMP
    ;

typeCore
    : (IDENTIFIER QUOTE)? baseType
    ;

baseType
    : VOID
    | KW_INT
    | KW_STR
    | KW_BOOL
    | KW_FLOAT
    | LBRACK HASH INT_LIT? RBRACK type
    | LBRACK RBRACK type
    | LPAREN type (PIPE type)+ RPAREN
    | IDENTIFIER typeArgs?
    ;

typeArgs
    : LT type (COMMA type)* GT
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

block
    : LBRACE stat* expr? RBRACE
    ;

stat
    : localDecl SEMI
    | setStat SEMI
    | returnStat SEMI
    | breakStat SEMI
    | continueStat SEMI
    | destructStat SEMI
    | pureBlockStat
    | ifStat
    | whileStat
    | foreachStat
    | matchStat
    | exprStat SEMI
    | block
    ;

localDecl
    : pattern ASSIGN expr
    ;

setStat
    : SET expr ASSIGN expr
    ;

returnStat
    : RETURN expr?
    ;

breakStat
    : BREAK
    ;

continueStat
    : CONTINUE
    ;

destructStat
    : DESTRUCT expr
    ;

pureBlockStat
    : PURE BLOCK block
    ;

ifStat
    : IF expr block (ELSE (ifStat | block))?
    ;

whileStat
    : WHILE expr block
    ;

foreachStat
    : FOREACH pattern IN expr block
    ;

matchStat
    : MATCH expr LBRACE matchArm+ RBRACE
    ;

matchArm
    : matchPattern block
    ;

matchPattern
    : INT_LIT
    | STRING_LIT
    | IDENTIFIER (LPAREN pattern (COMMA pattern)* RPAREN)?
    ;

exprStat
    : expr
    ;

// ---------------------------------------------------------------------------
// Expressions (precedence, highest to lowest: unary/ownership prefixes,
// postfix member/index/call, multiplicative, additive, relational,
// equality, logical-and, logical-or)
// ---------------------------------------------------------------------------

expr
    : logicOrExpr
    ;

logicOrExpr
    : logicOrExpr OR logicAndExpr
    | logicAndExpr
    ;

logicAndExpr
    : logicAndExpr AND eqExpr
    | eqExpr
    ;

eqExpr
    : eqExpr (EQ | NEQ) relExpr
    | relExpr
    ;

relExpr
    : relExpr (LT | GT | LE | GE) addExpr
    | addExpr
    ;

addExpr
    : addExpr (PLUS | MINUS) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH | PERCENT) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : (NOT | MINUS | AMP AMP | AMP | CARET) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : postfixExpr DOT INT_LIT
    | postfixExpr DOT IDENTIFIER call?
    | postfixExpr LBRACK expr RBRACK
    | postfixExpr call
    | factor
    ;

call
    : LPAREN (expr (COMMA expr)*)? RPAREN
    ;

factor
    : LPAREN expr (COMMA expr)* RPAREN
    | constant
    | ifExpr
    | lambdaExpr
    | staticArrayLit
    | runtimeArrayLit
    | SELF
    | THIS
    | IDENTIFIER typeArgs? call?
    ;

ifExpr
    : IF expr block ELSE block
    ;

lambdaExpr
    : LPAREN lambdaParamList? RPAREN ARROW2 (block | expr)
    | lambdaParamList ARROW2 (block | expr)
    ;

lambdaParamList
    : lambdaParam (COMMA lambdaParam)*
    ;

lambdaParam
    : IDENTIFIER type?
    ;

staticArrayLit
    : LBRACK HASH INT_LIT? RBRACK (LBRACK (expr (COMMA expr)*)? RBRACK | LPAREN expr RPAREN)
    ;

runtimeArrayLit
    : LBRACK RBRACK type? LPAREN (expr (COMMA expr)*)? RPAREN
    ;

constant
    : INT_LIT
    | FLOAT_LIT
    | STRING_LIT
    | TRUE
    | FALSE
    ;
