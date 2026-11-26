/*
BSD License

Parser grammar for the Lobster programming language (https://github.com/aardappel/lobster).
Built against the language reference at
https://aardappel.github.io/lobster/language_reference.html

Lobster's own reference grammar is given as a terse EBNF that leaves a few
points genuinely ambiguous or underspecified (associativity, the exact
precedence of 'and'/'or' versus the bitwise operators, generic function call
syntax, operator-overload declarations, etc). This grammar resolves those by
picking the common, unambiguous reading used by most C-family languages:

  - a conventional, fully left-to-right precedence ladder for binary
    operators (see opExpr .. logicOrExpr below) instead of the single,
    flatter level given in the reference grammar;
  - trailing function-value ("block") arguments and 'fn' lambdas are
    restricted to a single inline statement body (no indented block), to
    keep the interaction between expression-level constructs and the
    indentation-sensitive statement grammar unambiguous; named 'def'
    functions and the control-flow statements (if/while/for/switch/guard)
    still support full multi-line indented bodies.

Entry rule: program
*/

parser grammar LobsterParser;

options {
    tokenVocab = LobsterLexer;
}

program
    : NEWLINE? stat* EOF
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

stat
    : simpleStat
    | compoundStat
    ;

simpleStat
    : smallStat NEWLINE
    ;

smallStat
    : varDef
    | returnStat
    | breakStat
    | continueStat
    | importStat
    | namespaceStat
    | attrDef
    | guardSimple
    | exp
    ;

compoundStat
    : ifStat
    | whileStat
    | forStat
    | switchStat
    | guardBodyStat
    | functionDef
    | classDef
    | enumDef
    ;

// a single-line body, or an indented block of statements
body
    : smallStat NEWLINE
    | NEWLINE INDENT stat+ DEDENT
    ;

varDef
    : (VAR | LET) identWithType (COMMA identWithType)* (ASSIGN exp (COMMA exp)*)?
    ;

identWithType
    : IDENTIFIER (COLON type)?
    ;

returnStat
    : RETURN (exp (COMMA exp)*)? (FROM (PROGRAM | IDENTIFIER))?
    ;

breakStat
    : BREAK
    ;

continueStat
    : CONTINUE
    ;

importStat
    : IMPORT FROM? (STRING_LIT | DOT? IDENTIFIER (DOT IDENTIFIER)*)
    ;

namespaceStat
    : NAMESPACE IDENTIFIER
    ;

attrDef
    : ATTRIBUTE IDENTIFIER (ASSIGN (STRING_LIT | numericConst | IDENTIFIER))?
    ;

guardSimple
    : GUARD condExpr
    ;

guardBodyStat
    : GUARD condExpr COLON body
    ;

ifStat
    : IF condExpr COLON body (ELIF condExpr COLON body)* (ELSE COLON body)?
    ;

whileStat
    : WHILE LPAREN RPAREN COLON body DO COLON body
    | WHILE condExpr COLON body
    ;

forStat
    : FOR LPAREN exp? (COMMA exp)* RPAREN (IDENTIFIER (COMMA IDENTIFIER)*)? COLON body
    ;

switchStat
    : SWITCH exp COLON NEWLINE INDENT caseClause+ DEDENT
    ;

caseClause
    : CASE caseLabel (COMMA caseLabel)* COLON body
    | DEFAULT COLON body
    | OUT_OF_RANGE COLON body
    ;

caseLabel
    : exp (RANGE exp)?
    ;

condExpr
    : condPart (AND condPart)*
    ;

condPart
    : varDef
    | exp
    ;

functionDef
    : PRIVATE? (DEF | CONSTRUCTOR) IDENTIFIER genericParams? LPAREN paramList? RPAREN (ARROW type)? COLON body
    ;

genericParams
    : LT IDENTIFIER (COMMA IDENTIFIER)* GT
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER ((COLON | DOUBLECOLON) type)? (ASSIGN exp)?
    ;

classDef
    : PRIVATE? (CLASS | STRUCT) IDENTIFIER ASSIGN IDENTIFIER specializers? NEWLINE
    | PRIVATE? (CLASS | STRUCT) IDENTIFIER NEWLINE
    | PRIVATE? ABSTRACT? UNION? (CLASS | STRUCT) IDENTIFIER genericParams? COLON baseClause? NEWLINE INDENT classMember+ DEDENT
    ;

baseClause
    : IDENTIFIER specializers?
    ;

classMember
    : PRIVATE? IDENTIFIER (COLON type)? (ASSIGN exp)? NEWLINE
    | functionDef
    ;

enumDef
    : PRIVATE? (ENUM | ENUM_FLAGS) IDENTIFIER COLON NEWLINE INDENT enumMember+ DEDENT
    ;

enumMember
    : IDENTIFIER (ASSIGN INT_LIT)? NEWLINE
    ;

// ---------------------------------------------------------------------------
// Expressions (precedence, highest to lowest: unary, postfix/deref,
// multiplicative, additive, shift, relational, equality, bitwise-and,
// bitwise-xor, bitwise-or, logical-and, logical-or, assignment)
// ---------------------------------------------------------------------------

exp
    : opExpr (assignOp exp)?
    ;

assignOp
    : ASSIGN
    | PLUS_ASSIGN
    | MINUS_ASSIGN
    | STAR_ASSIGN
    | SLASH_ASSIGN
    | PERCENT_ASSIGN
    ;

opExpr
    : logicOrExpr
    ;

logicOrExpr
    : logicOrExpr OR logicAndExpr
    | logicAndExpr
    ;

logicAndExpr
    : logicAndExpr AND bitOrExpr
    | bitOrExpr
    ;

bitOrExpr
    : bitOrExpr PIPE bitXorExpr
    | bitXorExpr
    ;

bitXorExpr
    : bitXorExpr CARET bitAndExpr
    | bitAndExpr
    ;

bitAndExpr
    : bitAndExpr AMP eqExpr
    | eqExpr
    ;

eqExpr
    : eqExpr (EQ | NEQ) relExpr
    | relExpr
    ;

relExpr
    : relExpr (LT | GT | LE | GE) shiftExpr
    | shiftExpr
    ;

shiftExpr
    : shiftExpr (SHL | SHR) addExpr
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
    : (MINUS | INC | DEC_OP | TILDE | NOT) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : postfixExpr LBRACK exp RBRACK
    | postfixExpr DOT IDENTIFIER call?
    | postfixExpr ARROW IDENTIFIER
    | postfixExpr (INC | DEC_OP)
    | postfixExpr IS type
    | factor
    ;

factor
    : LPAREN exp (COMMA exp)* RPAREN
    | constant
    | SUPER call
    | ASSERT condExpr
    | PAKFILE STRING_LIT
    | constructorExpr
    | lambdaExpr
    | TYPEOF LPAREN type RPAREN
    | IDENTIFIER (STRING_LIT | TRIPLE_STRING_LIT)
    | IDENTIFIER call
    | IDENTIFIER
    ;

call
    : specializers? LPAREN (exp (COMMA exp)*)? RPAREN blockArg? (FN blockArg)*
    ;

blockArg
    : blockParams? COLON smallStat
    ;

blockParams
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

lambdaExpr
    : FN genericParams? LPAREN paramList? RPAREN COLON smallStat
    | FN blockParams COLON smallStat
    ;

constructorExpr
    : LBRACK (exp (COMMA exp)*)? RBRACK (DOUBLECOLON type)?
    | IDENTIFIER specializers? LBRACE (fieldInit (COMMA fieldInit)*)? RBRACE
    | LBRACE (fieldInit (COMMA fieldInit)*)? RBRACE
    ;

fieldInit
    : IDENTIFIER COLON exp
    | exp
    ;

constant
    : INT_LIT
    | FLOAT_LIT
    | STRING_LIT
    | TRIPLE_STRING_LIT
    | CHAR_LIT
    | TRUE
    | FALSE
    | NIL (DOUBLECOLON type)?
    ;

numericConst
    : INT_LIT
    | FLOAT_LIT
    ;

specializers
    : LT typeList GT
    ;

typeList
    : type (COMMA type)*
    ;

type
    : baseType QUESTION?
    ;

baseType
    : KW_INT
    | KW_FLOAT
    | KW_STRING
    | KW_ANY
    | KW_VOID
    | LBRACK type RBRACK
    | RESOURCE LT IDENTIFIER GT
    | IDENTIFIER specializers?
    ;
