/*
 * SysML v2 ANTLR4 Grammar
 * Derived from the OMG SysML v2 specification (KEBNF format).
 * Source: https://github.com/Systems-Modeling/SysML-v2-Release
 * Generator: https://github.com/daltskin/sysml-v2-grammar
 * License: MIT
 */

lexer grammar SysMLv2Lexer;

// $antlr-format alignTrailingComments true, columnLimit 150, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true
// $antlr-format alignSemicolons hanging, alignColons hanging
// $antlr-format minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false

// Keywords
ABOUT
    : 'about'
    ;

ABSTRACT
    : 'abstract'
    ;

ACCEPT
    : 'accept'
    ;

ACTION
    : 'action'
    ;

ACTOR
    : 'actor'
    ;

AFTER
    : 'after'
    ;

ALIAS
    : 'alias'
    ;

ALL
    : 'all'
    ;

ALLOCATE
    : 'allocate'
    ;

ALLOCATION
    : 'allocation'
    ;

ANALYSIS
    : 'analysis'
    ;

AND
    : 'and'
    ;

AS
    : 'as'
    ;

ASSERT
    : 'assert'
    ;

ASSIGN
    : 'assign'
    ;

ASSOC
    : 'assoc'
    ;

ASSUME
    : 'assume'
    ;

AT
    : 'at'
    ;

ATTRIBUTE
    : 'attribute'
    ;

BEHAVIOR
    : 'behavior'
    ;

BIND
    : 'bind'
    ;

BINDING
    : 'binding'
    ;

BOOL
    : 'bool'
    ;

BY
    : 'by'
    ;

CALC
    : 'calc'
    ;

CASE
    : 'case'
    ;

CHAINS
    : 'chains'
    ;

CLASS
    : 'class'
    ;

CLASSIFIER
    : 'classifier'
    ;

COMMENT
    : 'comment'
    ;

COMPOSITE
    : 'composite'
    ;

CONCERN
    : 'concern'
    ;

CONJUGATE
    : 'conjugate'
    ;

CONJUGATES
    : 'conjugates'
    ;

CONJUGATION
    : 'conjugation'
    ;

CONNECT
    : 'connect'
    ;

CONNECTION
    : 'connection'
    ;

CONNECTOR
    : 'connector'
    ;

CONST
    : 'const'
    ;

CONSTANT
    : 'constant'
    ;

CONSTRAINT
    : 'constraint'
    ;

CROSSES
    : 'crosses'
    ;

DATATYPE
    : 'datatype'
    ;

DECIDE
    : 'decide'
    ;

DEF
    : 'def'
    ;

DEFAULT
    : 'default'
    ;

DEFINED
    : 'defined'
    ;

DEPENDENCY
    : 'dependency'
    ;

DERIVED
    : 'derived'
    ;

DIFFERENCES
    : 'differences'
    ;

DISJOINING
    : 'disjoining'
    ;

DISJOINT
    : 'disjoint'
    ;

DO
    : 'do'
    ;

DOC
    : 'doc'
    ;

ELSE
    : 'else'
    ;

END
    : 'end'
    ;

ENTRY
    : 'entry'
    ;

ENUM
    : 'enum'
    ;

EVENT
    : 'event'
    ;

EXHIBIT
    : 'exhibit'
    ;

EXIT
    : 'exit'
    ;

EXPOSE
    : 'expose'
    ;

EXPR
    : 'expr'
    ;

FALSE
    : 'false'
    ;

FEATURE
    : 'feature'
    ;

FEATURED
    : 'featured'
    ;

FEATURING
    : 'featuring'
    ;

FILTER
    : 'filter'
    ;

FIRST
    : 'first'
    ;

FLOW
    : 'flow'
    ;

FOR
    : 'for'
    ;

FORK
    : 'fork'
    ;

FRAME
    : 'frame'
    ;

FROM
    : 'from'
    ;

FUNCTION
    : 'function'
    ;

HASTYPE
    : 'hastype'
    ;

IF
    : 'if'
    ;

IMPLIES
    : 'implies'
    ;

IMPORT
    : 'import'
    ;

IN
    : 'in'
    ;

INCLUDE
    : 'include'
    ;

INDIVIDUAL
    : 'individual'
    ;

INOUT
    : 'inout'
    ;

INTERACTION
    : 'interaction'
    ;

INTERFACE
    : 'interface'
    ;

INTERSECTS
    : 'intersects'
    ;

INV
    : 'inv'
    ;

INVERSE
    : 'inverse'
    ;

INVERTING
    : 'inverting'
    ;

ISTYPE
    : 'istype'
    ;

ITEM
    : 'item'
    ;

JOIN
    : 'join'
    ;

LANGUAGE
    : 'language'
    ;

LIBRARY
    : 'library'
    ;

LOCALE
    : 'locale'
    ;

LOOP
    : 'loop'
    ;

MEMBER
    : 'member'
    ;

MERGE
    : 'merge'
    ;

MESSAGE
    : 'message'
    ;

META
    : 'meta'
    ;

METACLASS
    : 'metaclass'
    ;

METADATA
    : 'metadata'
    ;

MULTIPLICITY
    : 'multiplicity'
    ;

NAMESPACE
    : 'namespace'
    ;

NEW
    : 'new'
    ;

NONUNIQUE
    : 'nonunique'
    ;

NOT
    : 'not'
    ;

NULL
    : 'null'
    ;

OBJECTIVE
    : 'objective'
    ;

OCCURRENCE
    : 'occurrence'
    ;

OF
    : 'of'
    ;

OR
    : 'or'
    ;

ORDERED
    : 'ordered'
    ;

OUT
    : 'out'
    ;

PACKAGE
    : 'package'
    ;

PARALLEL
    : 'parallel'
    ;

PART
    : 'part'
    ;

PERFORM
    : 'perform'
    ;

PORT
    : 'port'
    ;

PORTION
    : 'portion'
    ;

PREDICATE
    : 'predicate'
    ;

PRIVATE
    : 'private'
    ;

PROTECTED
    : 'protected'
    ;

PUBLIC
    : 'public'
    ;

REDEFINES
    : 'redefines'
    ;

REDEFINITION
    : 'redefinition'
    ;

REF
    : 'ref'
    ;

REFERENCES
    : 'references'
    ;

RENDER
    : 'render'
    ;

RENDERING
    : 'rendering'
    ;

REP
    : 'rep'
    ;

REQUIRE
    : 'require'
    ;

REQUIREMENT
    : 'requirement'
    ;

RETURN
    : 'return'
    ;

SATISFY
    : 'satisfy'
    ;

SEND
    : 'send'
    ;

SNAPSHOT
    : 'snapshot'
    ;

SPECIALIZATION
    : 'specialization'
    ;

SPECIALIZES
    : 'specializes'
    ;

STAKEHOLDER
    : 'stakeholder'
    ;

STANDARD
    : 'standard'
    ;

STATE
    : 'state'
    ;

STEP
    : 'step'
    ;

STRUCT
    : 'struct'
    ;

SUBCLASSIFIER
    : 'subclassifier'
    ;

SUBJECT
    : 'subject'
    ;

SUBSET
    : 'subset'
    ;

SUBSETS
    : 'subsets'
    ;

SUBTYPE
    : 'subtype'
    ;

SUCCESSION
    : 'succession'
    ;

TERMINATE
    : 'terminate'
    ;

THEN
    : 'then'
    ;

TIMESLICE
    : 'timeslice'
    ;

TO
    : 'to'
    ;

TRANSITION
    : 'transition'
    ;

TRUE
    : 'true'
    ;

TYPE
    : 'type'
    ;

TYPED
    : 'typed'
    ;

TYPING
    : 'typing'
    ;

UNIONS
    : 'unions'
    ;

UNTIL
    : 'until'
    ;

USE
    : 'use'
    ;

VAR
    : 'var'
    ;

VARIANT
    : 'variant'
    ;

VARIATION
    : 'variation'
    ;

VERIFICATION
    : 'verification'
    ;

VERIFY
    : 'verify'
    ;

VIA
    : 'via'
    ;

VIEW
    : 'view'
    ;

VIEWPOINT
    : 'viewpoint'
    ;

WHEN
    : 'when'
    ;

WHILE
    : 'while'
    ;

XOR
    : 'xor'
    ;

// Operators and punctuation
BANG_EQ_EQ
    : '!=='
    ;

COLON_COLON_GT
    : '::>'
    ;

COLON_GT_GT
    : ':>>'
    ;

EQ_EQ_EQ
    : '==='
    ;

BANG_EQ
    : '!='
    ;

STAR_STAR
    : '**'
    ;

ARROW
    : '->'
    ;

DOT_DOT
    : '..'
    ;

DOT_QUESTION
    : '.?'
    ;

COLON_COLON
    : '::'
    ;

COLON_EQ
    : ':='
    ;

COLON_GT
    : ':>'
    ;

LE
    : '<='
    ;

EQ_EQ
    : '=='
    ;

FAT_ARROW
    : '=>'
    ;

GE
    : '>='
    ;

QUESTION_QUESTION
    : '??'
    ;

AT_AT
    : '@@'
    ;

HASH
    : '#'
    ;

DOLLAR
    : '$'
    ;

PERCENT
    : '%'
    ;

AMP
    : '&'
    ;

LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

STAR
    : '*'
    ;

PLUS
    : '+'
    ;

COMMA
    : ','
    ;

MINUS
    : '-'
    ;

DOT
    : '.'
    ;

SLASH
    : '/'
    ;

COLON
    : ':'
    ;

SEMI
    : ';'
    ;

LT
    : '<'
    ;

EQ
    : '='
    ;

GT
    : '>'
    ;

QUESTION
    : '?'
    ;

AT_SIGN
    : '@'
    ;

LBRACK
    : '['
    ;

RBRACK
    : ']'
    ;

CARET
    : '^'
    ;

LBRACE
    : '{'
    ;

PIPE
    : '|'
    ;

RBRACE
    : '}'
    ;

TILDE
    : '~'
    ;

// Identifiers
IDENTIFIER
    : [a-zA-Z_] [a-zA-Z0-9_]*
    ;

// String literals
STRING
    : '\'' ('\\' . | ~['\\])* '\''
    ;

DOUBLE_STRING
    : '"' ('\\' . | ~["\\])* '"'
    ;

// Numeric literals
INTEGER
    : [0-9]+
    ;

REAL
    : [0-9]* '.' [0-9]+ ([eE] [+-]? [0-9]+)?
    | [0-9]+ [eE] [+-]? [0-9]+
    ;

// Comments
REGULAR_COMMENT
    : '/*' .*? '*/'
    ;

SINGLE_LINE_NOTE
    : '//' ~[\r\n]* -> skip
    ;

// Whitespace
WS
    : [ \t\r\n]+ -> skip
    ;