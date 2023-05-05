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
parser grammar ANTLRv3Parser;

options { tokenVocab = ANTLRv3Lexer; }

grammarDef
   : DOC_COMMENT? (LEXER | PARSER | TREE| ) GRAMMAR id_ SEMI optionsSpec? tokensSpec? attrScope* action* rule_+
   EOF
   ;

tokensSpec
   : TOKENS LBRACE tokenSpec+ RBRACE
   ;

tokenSpec
   : TOKEN_REF ( EQUAL (STRING_LITERAL | CHAR_LITERAL) | ) SEMI
   ;

attrScope
   : SCOPE id_ actionBlock
   ;

action
   : AT (actionScopeName COLONCOLON)? id_ actionBlock
   ;

actionScopeName
   : id_
   | LEXER
   | PARSER
   ;

optionsSpec
   : OPTIONS LBRACE option* RBRACE
   ;

option
   : id_ EQUAL optionValue SEMI
   ;

optionValue
   : id_
   | STRING_LITERAL
   | CHAR_LITERAL
   | INT
   | STAR
   ;

rule_
   : DOC_COMMENT? (PROTECTED | PUBLIC | PRIVATE | FRAGMENT)? id_ BANG? argActionBlock? (RETURNS argActionBlock)? throwsSpec? optionsSpec? ruleScopeSpec? ruleAction* COLON altList SEMI exceptionGroup?
   ;
   
ruleAction
   : AT id_ actionBlock
   ;

throwsSpec
   : THROWS id_ (COMMA id_)*
   ;

ruleScopeSpec
   : SCOPE actionBlock
   | SCOPE id_ (COMMA id_)* SEMI
   | SCOPE actionBlock SCOPE id_ (COMMA id_)* SEMI
   ;

block
   : LPAREN (optionsSpec? COLON)?
                alternative rewrite (OR alternative rewrite )*
        RPAREN
   ;

altList
   : alternative rewrite (OR alternative rewrite )*
   ;

alternative
   : element+
   |
   ;

exceptionGroup
   : exceptionHandler+ finallyClause?
   | finallyClause
   ;

exceptionHandler
   : CATCH argActionBlock actionBlock
   ;

finallyClause
   : FINALLY actionBlock
   ;

element
   : elementNoOptionSpec
   ;

elementNoOptionSpec
   : id_ (EQUAL | PEQ) atom (ebnfSuffix | )
   | id_ (EQUAL | PEQ) block (ebnfSuffix | )
   | atom (ebnfSuffix | )
   | ebnf
   | actionBlock
   | actionBlock QM ( SEMPREDOP | )
   | treeSpec (ebnfSuffix | )
   ;

actionBlock
   : BEGIN_ACTION ACTION_CONTENT* END_ACTION
   ;

argActionBlock
   : BEGIN_ARGUMENT ARGUMENT_CONTENT* END_ARGUMENT
   ;

atom
   : range_ ( ROOT | BANG | )
   | terminal_
   | notSet ( ROOT | BANG | )
   | RULE_REF argActionBlock? ( ROOT | BANG )?
   ;

notSet
   : NOT (notTerminal | block)
   ;

treeSpec
   : TREE_BEGIN element element+ RPAREN
   ;

ebnf
   : block (QM | STAR | PLUS | SEMPREDOP | )
   ;

range_
   : CHAR_LITERAL RANGE CHAR_LITERAL
   ;

terminal_
   : (CHAR_LITERAL
        | TOKEN_REF (argActionBlock | )
        | STRING_LITERAL
        | DOT
     ) (ROOT | BANG)?
   ;

notTerminal
   : CHAR_LITERAL
   | TOKEN_REF
   | STRING_LITERAL
   ;

ebnfSuffix
   : QM
   | STAR
   | PLUS
   ;

rewrite
   : (REWRITE actionBlock QM rewrite_alternative)* REWRITE rewrite_alternative
   |
   ;

rewrite_alternative
   : rewrite_template
   | rewrite_tree_alternative
   |
   ;

rewrite_tree_block
   : LPAREN rewrite_tree_alternative RPAREN
   ;

rewrite_tree_alternative
   : rewrite_tree_element+
   ;

rewrite_tree_element
   : rewrite_tree_atom
   | rewrite_tree_atom ebnfSuffix
   | rewrite_tree (ebnfSuffix | )
   | rewrite_tree_ebnf
   ;

rewrite_tree_atom
   : CHAR_LITERAL
   | TOKEN_REF argActionBlock?
   | RULE_REF
   | STRING_LITERAL
   | DOLLAR id_
   | actionBlock
   ;

rewrite_tree_ebnf
   : rewrite_tree_block ebnfSuffix
   ;

rewrite_tree
   : TREE_BEGIN rewrite_tree_atom rewrite_tree_element* RPAREN
   ;

rewrite_template
   : id_ LPAREN rewrite_template_args RPAREN
      ( DOUBLE_QUOTE_STRING_LITERAL | DOUBLE_ANGLE_STRING_LITERAL )
   | rewrite_template_ref
   | rewrite_indirect_template_head
   | actionBlock
   ;

rewrite_template_ref
   : id_ LPAREN rewrite_template_args RPAREN
   ;

rewrite_indirect_template_head
   : LPAREN actionBlock RPAREN LPAREN rewrite_template_args RPAREN
   ;

rewrite_template_args
   : rewrite_template_arg (COMMA rewrite_template_arg)*
   |
   ;

rewrite_template_arg
   : id_ EQUAL actionBlock
   ;

id_
   : TOKEN_REF
   | RULE_REF
   ;

