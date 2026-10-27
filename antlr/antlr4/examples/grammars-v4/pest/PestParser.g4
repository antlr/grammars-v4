
// $antlr-format alignColons hanging, alignSemicolons hanging, alignTrailingComments true, allowShortBlocksOnASingleLine true
// $antlr-format allowShortRulesOnASingleLine false, columnLimit 150, maxEmptyLinesToKeep 1, minEmptyLines 1, reflowComments false, useTab false

parser grammar PestParser;

options
{
    tokenVocab = PestLexer;
}

grammar_rules
    : grammar_rule+ EOF
    ;

grammar_rule
    : IDENTIFIER ASSIGNMENT_OPERATOR modifier? OPENING_BRACE expression CLOSING_BRACE
    ;

modifier
    : silent_modifier
    | atomic_modifier
    | compound_atomic_modifier
    | non_atomic_modifier
    ;

silent_modifier
    : '_'
    ;

atomic_modifier
    : '@'
    ;

compound_atomic_modifier
    : '$'
    ;

non_atomic_modifier
    : '!'
    ;

expression
    : term (infix_operator term)*
    ;

term
    : prefix_operator* node postfix_operator*
    ;

node
    : OPENING_PAREN expression CLOSING_PAREN
    | terminal
    ;

terminal
    : push
    | peek_slice
    | IDENTIFIER
    | STRING
    | INSENSITIVE_STRING
    | RANGE
    ;

prefix_operator
    : positive_predicate_operator
    | negative_predicate_operator
    ;

infix_operator
    : sequence_operator
    | choice_operator
    ;

postfix_operator
    : optional_operator
    | repeat_operator
    | repeat_once_operator
    | repeat_exact
    | repeat_min
    | repeat_max
    | repeat_min_max
    ;

positive_predicate_operator
    : '&'
    ;

negative_predicate_operator
    : '!'
    ;

sequence_operator
    : '~'
    ;

choice_operator
    : '|'
    ;

optional_operator
    : '?'
    ;

repeat_operator
    : '*'
    ;

repeat_once_operator
    : '+'
    ;

repeat_exact
    : OPENING_BRACE NUMBER CLOSING_BRACE
    ;

repeat_min
    : OPENING_BRACE NUMBER COMMA CLOSING_BRACE
    ;

repeat_max
    : OPENING_BRACE COMMA NUMBER CLOSING_BRACE
    ;

repeat_min_max
    : OPENING_BRACE NUMBER COMMA NUMBER CLOSING_BRACE
    ;

push
    : 'PUSH' OPENING_PAREN expression CLOSING_PAREN
    ;

peek_slice
    : 'PEEK' OPENING_BRACK INTEGER? RANGE_OPERATOR INTEGER? CLOSING_BRACK
    ;