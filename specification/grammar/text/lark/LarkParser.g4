parser grammar LarkParser;

options {
    tokenVocab = LarkLexer;
}

start: item* EOF;

item: rule_ | token | statement ;

rule_: RULE rule_params priority? ':' expansions ;

token: TOKEN token_params priority? ':' expansions ;

rule_params: ('{' RULE (',' RULE)* '}')? ;

token_params: ('{' TOKEN (',' TOKEN)* '}')? ;

priority: '.' NUMBER ;

statement: '%ignore' expansions
         | '%import' import_path ('->' name)?
         | '%import' import_path name_list
         | '%override' rule_
         | '%declare' name+
	 ;

import_path: '.'? name ('.' name)* ;

name_list: '(' name (',' name)* ')' ;

expansions: alias (VBAR alias)* ;

alias: expansion ('->' RULE)? ;

expansion: expr* ;

expr: atom (OP | '~' NUMBER ('..' NUMBER)? )? ;

atom: '(' expansions ')' | '[' expansions ']' | value ;

value: STRING '..' STRING
      | name
      | (REGEXP | STRING)
      | name '{' value (',' value)* '}'
      ;

name: RULE | TOKEN ;
