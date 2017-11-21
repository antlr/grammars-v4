/*
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar ANTLRv3;

grammarDef
   : DOC_COMMENT? ('lexer' | 'parser' | 'tree')? 'grammar' id ';' optionsSpec? tokensSpec? attrScope* action* rule_ +
   ;

tokensSpec
   : TOKENS tokenSpec + '}'
   ;

tokenSpec
   : TOKEN_REF ('=' (STRING_LITERAL | CHAR_LITERAL) |) ';'
   ;

attrScope
   : 'scope' id ACTION
   ;

action
   : '@' (actionScopeName '::')? id ACTION
   ;

actionScopeName
   : id
   | 'lexer'
   | 'parser'
   ;

optionsSpec
   : OPTIONS (option ';') + '}'
   ;

option
   : id '=' optionValue
   ;

optionValue
   : id
   | STRING_LITERAL
   | CHAR_LITERAL
   | INT
   | '*'
   ;

rule_
   : DOC_COMMENT? (('protected' | 'public' | 'private' | 'fragment'))? id '!'? (ARG_ACTION)? ('returns' ARG_ACTION)? throwsSpec? optionsSpec? ruleScopeSpec? ruleAction* ':' altList? ';' exceptionGroup?
   ;

ruleAction
   : '@' id ACTION
   ;

throwsSpec
   : 'throws' id (',' id)*
   ;

ruleScopeSpec
   : 'scope' ACTION
   | 'scope' id (',' id)* ';'
   | 'scope' ACTION 'scope' id (',' id)* ';'
   ;

block
   : '(' ((optionsSpec)? ':')? alternative? rewrite? ('|' (alternative rewrite?)?)* ')'
   ;

altList
   : alternative rewrite? ('|' (alternative rewrite?)?)*
   ;

alternative
   : element +
   ;

exceptionGroup
   : (exceptionHandler) + (finallyClause)?
   | finallyClause
   ;

exceptionHandler
   : 'catch' ARG_ACTION ACTION
   ;

finallyClause
   : 'finally' ACTION
   ;

element
   : elementNoOptionSpec
   ;

elementNoOptionSpec
   : id ('=' | '+=') atom (ebnfSuffix?)
   | id ('=' | '+=') block (ebnfSuffix?)
   | atom (ebnfSuffix?)
   | ebnf
   | ACTION
   | SEMPRED ('=>')
   | treeSpec (ebnfSuffix)
   ;

atom
   : range
   | terminal_
   | notSet
   | RULE_REF (ARG_ACTION)?
   ;

notSet
   : '~' (notTerminal | block)
   ;

treeSpec
   : '^(' element (element) + ')'
   ;

ebnf
   : block ('?' | '*' | '+' | '=>')?
   ;

range
   : CHAR_LITERAL RANGE CHAR_LITERAL
   ;

terminal_
   : (CHAR_LITERAL | TOKEN_REF (ARG_ACTION?) | STRING_LITERAL | '.') ('^' | '!')?
   ;

notTerminal
   : CHAR_LITERAL
   | TOKEN_REF
   | STRING_LITERAL
   ;

ebnfSuffix
   : '?'
   | '*'
   | '+'
   ;

rewrite
   : ('->' SEMPREDrewrite_alternative)* '->' rewrite_alternative
   ;

rewrite_alternative
   : rewrite_template
   | rewrite_tree_alternative
   ;

rewrite_tree_block
   : '(' rewrite_tree_alternative ')'
   ;

rewrite_tree_alternative
   : rewrite_tree_element +
   ;

rewrite_tree_element
   : rewrite_tree_atom
   | rewrite_tree_atom ebnfSuffix
   | rewrite_tree (ebnfSuffix)
   | rewrite_tree_ebnf
   ;

rewrite_tree_atom
   : CHAR_LITERAL
   | TOKEN_REF ARG_ACTION?
   | RULE_REF
   | STRING_LITERAL
   | '$' id
   | ACTION
   ;

rewrite_tree_ebnf
   : rewrite_tree_block ebnfSuffix
   ;

rewrite_tree
   : '^(' rewrite_tree_atom rewrite_tree_element* ')'
   ;

rewrite_template
   :
   ;

rewrite_template_ref
   : id '(' rewrite_template_args ')'
   ;

rewrite_indirect_template_head
   : '(' ACTION ')' '(' rewrite_template_args ')'
   ;

rewrite_template_args
   : rewrite_template_arg (',' rewrite_template_arg)*
   ;

rewrite_template_arg
   : id '=' ACTION
   ;

id
   : TOKEN_REF
   | RULE_REF
   ;


CHAR_LITERAL
   : '\'' LITERAL_CHAR '\''
   ;


STRING_LITERAL
   : '\'' LITERAL_CHAR LITERAL_CHAR* '\''
   ;


fragment LITERAL_CHAR
   : ESC | ~ ('\'' | '\\')
   ;


DOUBLE_QUOTE_STRING_LITERAL
   : '"' (ESC | ~ ('\\' | '"'))* '"'
   ;


DOUBLE_ANGLE_STRING_LITERAL
   : '<<' ()*? '>>'
   ;


fragment ESC
   : '\\' ('n' | 'r' | 't' | 'b' | 'f' | '"' | '\'' | '\\' | '>' | 'u' XDIGIT XDIGIT XDIGIT XDIGIT | .)
   ;


fragment XDIGIT
   : '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
   ;


INT
   : '0' .. '9' +
   ;


ARG_ACTION
   : NESTED_ARG_ACTION
   ;


fragment NESTED_ARG_ACTION
   : '[' (NESTED_ARG_ACTION | ACTION_STRING_LITERAL | ACTION_CHAR_LITERAL | .)*? ']'
   ;


ACTION
   : NESTED_ACTION ('?')?
   ;


fragment NESTED_ACTION
   : '{' (NESTED_ACTION | SL_COMMENT | ML_COMMENT | ACTION_STRING_LITERAL | ACTION_CHAR_LITERAL | .)*? '}'
   ;


fragment ACTION_CHAR_LITERAL
   : '\'' (ACTION_ESC | ~ ('\\' | '\'')) '\''
   ;


fragment ACTION_STRING_LITERAL
   : '"' (ACTION_ESC | ~ ('\\' | '"'))* '"'
   ;


fragment ACTION_ESC
   : '\\\'' | '\\' '"' | '\\' ~ ('\'' | '"')
   ;


OPTIONS
   : 'options' WS_LOOP '{'
   ;


TOKENS
   : 'tokens' WS_LOOP '{'
   ;


fragment SRC
   : 'src' ' ' ACTION_STRING_LITERAL ' ' INT
   ;


fragment WS_LOOP
   : (WS | SL_COMMENT | ML_COMMENT)*
   ;


DOC_COMMENT
   : 'DOC_COMMENT'
   ;


PARSER
   : 'PARSER'
   ;


LEXER
   : 'LEXER'
   ;


RULE
   : 'RULE'
   ;


BLOCK
   : 'BLOCK'
   ;


OPTIONAL
   : 'OPTIONAL'
   ;


CLOSURE
   : 'CLOSURE'
   ;


POSITIVE_CLOSURE
   : 'POSITIVE_CLOSURE'
   ;


SYNPRED
   : 'SYNPRED'
   ;


CHAR_RANGE
   : 'CHAR_RANGE'
   ;


EPSILON
   : 'EPSILON'
   ;


ALT
   : 'ALT'
   ;


EOR
   : 'EOR'
   ;


EOB
   : 'EOB'
   ;


EOA
   : 'EOA'
   ;

// end of alt

ID
   : 'ID'
   ;


ARG
   : 'ARG'
   ;


ARGLIST
   : 'ARGLIST'
   ;


RET
   : 'RET'
   ;


LEXER_GRAMMAR
   : 'LEXER_GRAMMAR'
   ;


PARSER_GRAMMAR
   : 'PARSER_GRAMMAR'
   ;


TREE_GRAMMAR
   : 'TREE_GRAMMAR'
   ;


COMBINED_GRAMMAR
   : 'COMBINED_GRAMMAR'
   ;


INITACTION
   : 'INITACTION'
   ;


LABEL
   : 'LABEL'
   ;


TEMPLATE
   : 'TEMPLATE'
   ;


SCOPE
   : 'scope'
   ;


SEMPRED
   : 'SEMPRED'
   ;


GATED_SEMPRED
   : 'GATED_SEMPRED'
   ;


SYN_SEMPRED
   : 'SYN_SEMPRED'
   ;


BACKTRACK_SEMPRED
   : 'BACKTRACK_SEMPRED'
   ;


FRAGMENT
   : 'fragment'
   ;


TREE_BEGIN
   : '^('
   ;


ROOT
   : '^'
   ;


BANG
   : '!'
   ;


RANGE
   : '..'
   ;


REWRITE
   : '->'
   ;


SL_COMMENT
   : '//' ~ [\r\n]* -> skip
   ;


ML_COMMENT
   : '/*' .*? '*/' -> skip
   ;


WS
   : (' ' | '\t' | '\r'? '\n') + -> skip
   ;


TOKEN_REF
   : 'A' .. 'Z' ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')*
   ;


RULE_REF
   : 'a' .. 'z' ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')*
   ;
