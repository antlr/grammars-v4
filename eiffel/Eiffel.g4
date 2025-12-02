/*
 [The "BSD licence"]
 Copyright (c) 2024, Miguel Oliveira e Silva
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

/**
 *  An Eiffel grammar based on ECMA 367 (2ed) and extended with syntax inferred
 *  from online documentation (https://www.eiffel.org/) and from all source
 *  code files provided in ISE's Eiffel_23.09_rev_107341 and Gobo Eiffel 22.01.09.4.
 *
 *  Uses ANTLR v4's left-recursive expression notation.
 *
 *  @maintainer: Miguel Oliveira e Silva
 *
 *  A group of bash scripts to ease antlr4 programming is available in:
 *  https://sweet.ua.pt/mos/antlr4
 */

grammar Eiffel;

options { caseInsensitive = true; }

@lexer::members {
   private static String[] invalidFreeOps = {
      ":=.", ":=+", ":=-", "<<-", "<<+", "<<>", "<<>>"
   };

   private boolean isFreeOperator(String tk) {
      boolean res = true;
      for(int i = 0; res && i < invalidFreeOps.length; i++)
         res = !invalidFreeOps[i].equals(tk);
      return res;
   }
}

class_declaration: notes? class_header formal_generics?
      obsolete?
      inheritance?
      creators?
      converters?
      features?
      notes?
      invariant?
      notes?
   END EOF
   ;

notes: NOTE note_list;

note_list: (note_entry (';'* note_entry)*)? ';'*;

note_entry: note_name note_values;

note_name: Identifier ':';

note_values: note_item (',' note_item)* ','?;

note_item: Identifier | manifest_constant;

class_header: header_mark* CLASS class_name; // MOS: header_mark must accept possible combinations (only one ensured by semantic predicates)!

class_name: Identifier;

header_mark
   locals[
      boolean deferred_exists=false,
      boolean expanded_exists=false,
      boolean frozen_exists=false,
      boolean external_exists=false,
      boolean once_exists=false
   ] :
     {!$deferred_exists}? DEFERRED {$deferred_exists=true;} #HeaderMarkDeferred
   | {!$expanded_exists}? EXPANDED {$expanded_exists=true;} #HeaderMarkExpanded
   | {!$frozen_exists}?   FROZEN   {$frozen_exists=true;}   #HeaderMarkFrozen
   | {!$external_exists}? EXTERNAL {$external_exists=true;} #HeaderMarkExternal
   | {!$once_exists}?     once     {$once_exists=true;}     #HeaderMarkOnce
   ;

obsolete: OBSOLETE manifest_string;

features: feature_clause+;

feature_clause: FEATURE clients? feature_declaration_list; // MOS: [Header_comment] removed!

feature_declaration_list: (feature_declaration (';'* feature_declaration)*)? ';'*;

feature_declaration: new_feature_list declaration_body;

declaration_body: formal_arguments? query_mark? notes? feature_value; // MOS: feature_value? (double optative!)

query_mark: type_mark assigner_mark?;

type_mark: ':' type;

feature_value: explicit_value? obsolete? attribute_or_routine? ('=' UNIQUE)?; // MOS: [Header_comment] removed!

explicit_value: '=' manifest_constant;

new_feature_list: new_feature (',' new_feature)*;

new_feature: FROZEN? extended_feature_name;

attribute_or_routine: precondition? local_declarations? feature_body postcondition? rescue? END;

feature_body: DEFERRED | effective_routine | attribute;

extended_feature_name: feature_name alias*; // MOS: multiple alias are possible!

feature_name: Identifier;

alias: ALIAS manifest_string CONVERT?; // MOS: replace '"' Alias_name '"' for manifest_string (TODO: semantic validation for Alias_name)

assigner_mark: ASSIGN feature_name;

inheritance: inherit_clause+;

inherit_clause: INHERIT non_conformance? parent_list;

non_conformance: {"none".equalsIgnoreCase(_input.LT(2).getText())}? '{' Identifier '}'; // MOS: NONE -> Identifier

parent_list: parent (';'* parent)* ';'*;

parent: class_type feature_adaptation?;

feature_adaptation // MOS: incorrect publicised grammar (different order in working examples)!
   locals[
      boolean undefine_exists=false,
      boolean redefine_exists=false,
      boolean rename_exists=false,
      boolean new_exports_exists=false,
      boolean select_exists=false
   ] : // MOS: semantic predicates ensure, at parse time, only one occurrence of each alternative!
   (
     {!$undefine_exists}?    undefine    {$undefine_exists=true;}
   | {!$redefine_exists}?    redefine    {$redefine_exists=true;}
   | {!$rename_exists}?      rename      {$rename_exists=true;}
   | {!$new_exports_exists}? new_exports {$new_exports_exists=true;}
   | {!$select_exists}?      select      {$select_exists=true;}
   )* END;

rename: RENAME rename_list;

rename_list: rename_pair (',' rename_pair)*;

rename_pair: feature_name AS extended_feature_name;

clients: '{' class_list? '}';

class_list: class_name (',' class_name)*;

new_exports: EXPORT new_export_list;

new_export_list: new_export_item (';'* new_export_item)* ';'*;

new_export_item: clients feature_set; // MOS: [Header_comment] removed!

feature_set: feature_list | ALL;

feature_list: (feature_name (',' feature_name)*)?;

formal_arguments: '(' entity_declaration_list ')';

entity_declaration_list: entity_declaration_group (';'* entity_declaration_group)* ';'*;

entity_declaration_group: identifier_list type_mark;

identifier_list: Identifier (',' Identifier)*;

effective_routine: internal | external;

internal: routine_mark compound;

routine_mark: DO | once;

once: ONCE ('(' key_list ')')?;

key_list: manifest_string (',' manifest_string)*;

local_declarations: LOCAL entity_declaration_list?;

compound: ';'* (instruction (';'* instruction)*)? ';'*;

instruction:
     creation_instruction
   | prec_call=expression // MOS: call -> expression. Semantic verification to ensure a procedure call.
                          //      (might_be_proc_call necessary but not sufficient).
   | assignment
   | assigner_call
   | conditional
   | multi_branch
   | loop
   | debug
   | precursor
   | check
   | retry
   | conditional_check // MOS: inferred from https://www.eiffel.org/doc/eiffel/ET-_Instructions
   | inline_separate // MOS: inferred from https://www.eiffel.org/doc/solutions/Exclusive_Access
   | iterator
   ;

iterator: OPEN_REPEAT Identifier ':' expression BAR compound CLOSE_REPEAT; // MOS: from examples!

precondition: REQUIRE ELSE? assertion;

postcondition: ENSURE THEN? assertion only?;

invariant: INVARIANT assertion;

assertion: (assertion_clause (';'* assertion_clause)*)? ';'*;

assertion_clause: tag_mark? unlabeled_assertion_clause | tag_mark; // MOS: assertion consisting only of a tag_mark should be accepted

unlabeled_assertion_clause: bool_expr=expression | CLASS; // MOS: boolean_expression -> expression & Comment removed!

tag_mark: tag ':';

tag: Identifier;

old: OLD expression;

only: ONLY feature_list;

check: CHECK assertion notes? END;

variant: VARIANT tag_mark? expression;

precursor: PRECURSOR parent_qualification? actuals?;

parent_qualification: '{' class_name '}';

redefine: REDEFINE feature_list;

undefine: UNDEFINE feature_list;

type: type_modifier* (class_or_tuple_type | formal_generic_name | anchored); // MOS: allow multiple modifiers

type_modifier: ATTACHED | DETACHABLE | SEPARATE | EXPANDED; // MOS: ATTACHED/DETACHABLE syntax inferred from https://www.eiffel.org/doc/eiffel/Void-safety-_Background%2C_definition%2C_and_tools#Types_as_%22attached%22_or_%22detachable%22

class_or_tuple_type: class_type | tuple_type;

class_type: attachment_mark? class_name actual_generics?;

attachment_mark: '?' | '!';

anchored: attachment_mark? LIKE anchor;

anchor: expression; // MOS: was feature_name | CURRENT | non_object_call (semantic verification for a proper anchor)

actual_generics: '[' type_list ']';

type_list: (type (',' type)*)?;

formal_generics: '[' formal_generic_list ']';

formal_generic_list: formal_generic (',' formal_generic)*;

formal_generic: formal_generic_modifier* formal_generic_name constraint?;

formal_generic_modifier: FROZEN | REFERENCE;

formal_generic_name: '?'? Identifier;

constraint: '->' constraining_types constraint_creators?;

constraining_types: single_constraint | multiple_constraint;

single_constraint: type renaming?;

renaming: rename END;

multiple_constraint: '{' constraint_list '}';

constraint_list: single_constraint (',' single_constraint)*;

constraint_creators: CREATE feature_list END;

manifest_array: manifest_array_type? '<<' expression_list '>>';

manifest_array_type: '{' type '}';

expression_list: (expression (',' expression)*)?;

//tuple_type: TUPLE tuple_parameter_list?; // MOS: tuple is used also as an Identifier!
tuple_type: {"tuple".equalsIgnoreCase(_input.LT(1).getText())}? Identifier tuple_parameter_list; // MOS: semantic predicate to ensure Identifier equals TUPLE

tuple_parameter_list: '[' entity_declaration_list ']'; // MOS: tuple_parameters replaced by entity_declaration_list (type_list already in class_type)

//tuple_parameters: type_list | entity_declaration_list;

manifest_tuple: '[' expression_list ']';

converters: CONVERT converter_list;

converter_list: converter (',' converter)*;

converter: conversion_procedure | conversion_query;

conversion_procedure: feature_name '(' '{' type_list '}' ')';

conversion_query: feature_name ':' '{' type_list '}';

select: SELECT feature_list;

conditional: IF then_part_list else_part? END;

then_part_list: then_part (ELSEIF then_part)*;

then_part: bool_expr=expression THEN compound; // MOS: boolean_expression -> expression (type solved semantically)

else_part: ELSE compound;

conditional_expression: IF then_part_expression_list ELSE expression END;

then_part_expression_list: then_part_expression (ELSEIF then_part_expression)*;

then_part_expression: bool_expr=expression THEN expression; // MOS: boolean_expression -> expression (type solved semantically)

multi_branch: INSPECT expression when_part_list? else_part? END;

when_part_list: when_part+;

when_part: WHEN choices THEN compound;

choices: choice (',' choice)*;

choice: constant | manifest_type | constant_interval | type_interval | non_object_call | non_object_call_interval;

constant_interval: constant '..' constant | Integer_interval; // MOS: token added because of interference with Real and FreeOperator

type_interval: manifest_type '..' manifest_type;

non_object_call_interval: non_object_call '..' non_object_call;

loop locals[boolean variant_exists=false]: // MOS: Different from ECMA 367!
   iteration?
   initialization?
   invariant?
   (variant {$variant_exists=true;})? // MOS: variant repeated here (as happens in some examples)
   exit_condition?
   loop_body
   ({!$variant_exists}? variant)?
   END
   ;

iteration: ACROSS expression AS Identifier;

initialization: FROM compound;

exit_condition: UNTIL bool_expr=expression; // MOS: boolean_expression -> expression

loop_body:
      LOOP compound |
      ALL bool_expr=expression | // MOS: boolean_expression -> expression
      SOME bool_expr=expression // MOS: reserved SOME  not present in ECMA 367
      ;

debug: DEBUG ('(' key_list ')')? compound END;

attribute: ATTRIBUTE compound;

entity: variable | read_only; // MOS: ambiguity (Identifier).
   // A semantic predicate may solve the problem at parse time.

variable: variable_attribute | local;

variable_attribute: feature_name; // MOS: ambiguity (Identifier).
   // A semantic predicate may solve the problem at parse time (not is_local(feature_name)).
   // Careful with implicit (ACROSS) local variable declarations.

local: Identifier | RESULT;

read_only: formal | constant_attribute | CURRENT; // MOS: ambiguity (Identifier).
   // A semantic predicate may solve the problem at parse time.

formal: Identifier;

constant_attribute: feature_name;

creators: creation_clause+;

creation_clause: CREATE clients? creation_procedure_list?; // MOS: [Header_comment] removed!

creation_procedure_list: creation_procedure (',' creation_procedure)*;

creation_procedure: feature_name;

creation_instruction: CREATE create_passive_region? explicit_creation_type? creation_call;

create_passive_region: {"none".equalsIgnoreCase(_input.LT(2).getText())}? '<' Identifier '>'; // MOS: EiffelStudio 15.08 Releases

explicit_creation_type: '{' type '}';

creation_call: variable explicit_creation_call?;

explicit_creation_call: '.' unqualified_call;

creation_expression: CREATE explicit_creation_type explicit_creation_call?;

//equality: expression comparison expression;

//comparison: '=' | '/=' | '~' | '/~';

assignment: variable ':=' expression;

assigner_call: expression ':=' expression; // MOS: ambiguity with assignment (solved by order in instruction)

//call: object_call | non_object_call;

//object_call: (target '.')? unqualified_call;

unqualified_call: feature_name (actuals)?;

//target: local | read_only | call | parenthesized_target;

//parenthesized_target: '(|' expression '|)'; // MOS: According to ECMA 367, error in page https://www.eiffel.org/doc/eiffel/Eiffel_programming_language_syntax

non_object_call: '{' type '}' '.' unqualified_call; // MOS: https://groups.google.com/g/eiffel-users/c/Y1FXpFBfInA

actuals: '(' actual_list ')';

actual_list: expression (',' expression)*;

object_test: '{' Identifier ':' type '}' expression;

rescue: RESCUE compound;

retry: RETRY;

conditional_check: CHECK assertion THEN compound END;

inline_separate: SEPARATE inline_separate_arguments DO compound END;

inline_separate_arguments: inline_separate_argument (',' inline_separate_argument)*;

inline_separate_argument: expression AS Identifier;

agent: call_agent | inline_agent;

call_agent: AGENT call_agent_body;

inline_agent: AGENT formal_arguments? type_mark? attribute_or_routine? agent_actuals?;

call_agent_body: agent_qualified | agent_unqualified;

agent_qualified: agent_target '.' agent_unqualified;

agent_unqualified: feature_name agent_actuals?;

agent_target: entity | parenthesized | manifest_type;

agent_actuals: '(' agent_actual_list ')';

agent_actual_list: agent_actual (',' agent_actual)*;

agent_actual: expression | placeholder;

placeholder: manifest_type? '?';

expression returns [boolean might_be_proc_call=false]: // MOS: expression (profoundly) rebuild to ensure an unambiguous free-context grammar
            //      with proper operator precedence.
            // Was: basic_expression | special_expression
            // Precedence extracted from ETL3, pg. 768; Undocumented precedence inferred (revision required)
     e1=expression '.' e2=expression {$might_be_proc_call=true;}            #ExprQualifiedCall
   | op=(OLD|NOT|'+'|'-'|FreeOperator) expression                           #ExprUnary
   | e1=expression FreeOperator e2=expression                               #ExprFreeBinaryOperator
   | <assoc=right> e1=expression '^' e2=expression                          #ExprPower
   | e1=expression op=('*'|'/'|'//'|'\\\\') e2=expression                   #ExprMultDiv
   | e1=expression op=('+'|'-') e2=expression                               #ExprAddSub
   | first=expression '..' last=expression                                  #ExprInterval
   | e1=expression op=('='|'/='|'~'|'/~'|'<'|'>'|'<='|'>=') e2=expression   #ExprRelational
   | e1=expression op=(AND|AND_THEN) e2=expression                          #ExprConjunctive
   | e1=expression op=(OR|OR_ELSE|XOR) e2=expression                        #ExprDisjunctive
   | e1=expression IMPLIES e2=expression                                    #ExprImplicative
   | expression '[' expression_list ']'                                     #ExprListAccess // MOS: inferred from examples
   | expression '[' actuals ']'                                             #ExprBracket // MOS: precedence to be reviewed!
   | '(' expression ')'                                                     #ExprParenthesized
   | '(|' expression '|)'                                                   #ExprParenthesizedTarget
   | op=(FOR_ALL|FOR_SOME) Identifier ':' expression BAR expression         #ExprFirstOrderLogic
   | Identifier {$might_be_proc_call=true;}                                 #ExprIdentifier // formal|constant_attribute |local|argumentless call
   | unqualified_call {$might_be_proc_call=true;}                           #ExprUnqualifiedCall
   | CURRENT                                                                #ExprCurrent
   | RESULT                                                                 #ExprResult
   | VOID                                                                   #ExprVoid
   | ATTACHED ('{' type '}')? expression (AS lexp=Identifier)?              #ExprAttached // MOS: precedence? // MOS: inferred from https://www.eiffel.org/doc/eiffel/Creating_a_new_void-safe_project
   | ACROSS expression AS Identifier (ALL|SOME) expression END              #ExprAcross // MOS: inferred from https://bertrandmeyer.com/2010/01/26/more-expressive-loops-for-eiffel/
   | ONCE ('{' type '}')? expression                                        #ExprOnce // MOS: inferred from examples
   | precursor                                                              #ExprPrecursor
   | creation_expression                                                    #ExprCreation
   | conditional_expression                                                 #ExprConditional
   | non_object_call {$might_be_proc_call=true;}                            #ExprNonObjectCall
   | special_expression                                                     #ExprSpecial
   ;

special_expression: manifest_constant | manifest_array | manifest_tuple | agent | object_test | once_string | address;

parenthesized: '(' expression ')';

address: '$' (variable | CURRENT | RESULT);

once_string: ONCE manifest_string;

constant: manifest_constant | constant_attribute;

manifest_constant: manifest_type? manifest_value;

manifest_type: '{' type '}';

manifest_value: boolean_constant | Character_constant | integer_constant | real_constant | manifest_string | manifest_type;

sign: '+' | '-';

integer_constant: sign? Integer;

boolean_constant: TRUE | FALSE;

real_constant: sign? Real;

manifest_string: Basic_manifest_string | Verbatim_string;

external: EXTERNAL external_language external_name?;

external_language: manifest_string; // MOS: registered_language handled semantically (it is easy and avoids lexer ambiguities).

external_name: ALIAS manifest_string;

// Explicit TOKENS:

ACROSS: 'across';
AGENT: 'agent';
ALIAS: 'alias';
ALL: 'all';
AND: 'and' | '∧';
AND_THEN: 'and' [ \t\r\n]+ 'then';
AS: 'as';
ASSIGN: 'assign';
ATTACHED: 'attached';
ATTRIBUTE: 'attribute';
CHECK: 'check';
CLASS: 'class';
CONVERT: 'convert';
CREATE: 'create';
CURRENT: 'current';
DEBUG: 'debug';
DEFERRED: 'deferred';
DETACHABLE: 'detachable';
DO: 'do';
ELSE: 'else';
ELSEIF: 'elseif';
END: 'end';
ENSURE: 'ensure';
EXPANDED: 'expanded';
EXPORT: 'export';
EXTERNAL: 'external';
FALSE: 'false';
FEATURE: 'feature';
FROM: 'from';
FROZEN: 'frozen';
IF: 'if';
IMPLIES: 'implies' | '⇒';
INHERIT: 'inherit';
INSPECT: 'inspect';
INVARIANT: 'invariant';
LIKE: 'like';
LOCAL: 'local';
LOOP: 'loop';
NOT: 'not' | '¬';
NOTE: 'note';
OBSOLETE: 'obsolete';
OLD: 'old';
ONCE: 'once';
ONLY: 'only';
OR: 'or' | '∨';
OR_ELSE: 'or' [ \t\r\n]+ 'else';
PRECURSOR: 'precursor';
REDEFINE: 'redefine';
REFERENCE: 'reference';
RENAME: 'rename';
REQUIRE: 'require';
RESCUE: 'rescue';
RESULT: 'result';
RETRY: 'retry';
SELECT: 'select';
SEPARATE: 'separate';
SOME: 'some'; // MOS: added!
THEN: 'then';
TRUE: 'true';
//TUPLE: 'tuple';
UNDEFINE: 'undefine';
UNIQUE: 'unique';
UNTIL: 'until';
VARIANT: 'variant';
VOID: 'void';
WHEN: 'when';
XOR: 'xor' | '⊻';
FOR_ALL: [∀]; // 0xE2 0x88 0x80
FOR_SOME: [∃]; // 0xE2 0x88 0x83
BAR: [¦];
OPEN_REPEAT: [⟳];
CLOSE_REPEAT: [⟲];

Identifier: Letter (Letter | [0-9_])*;
fragment Letter: [a-zA-Z\u00C0-\u00FF];
Verbatim_string: // MOS: based on example code usage (difficult to make it work correctly)
     '"[' [ \t]* '\n' (.*? '\n')? [ \t]* ']"'
   | '"{' [ \t]* '\n' (.*? '\n')? [ \t]* '}"'
   ;
Basic_manifest_string: '"' ('%"' | '%%' | StringLineBreak | ~[\n] )*? '"';
fragment StringLineBreak: '%' [ \t]* '\n' [ \t]* '%';
Character_constant: ['] (~[%'] | [%]. | [%][/]Integer[/] ) ['];
Integer: Integer_base? Digit+;
Integer_interval: Integer [ \t]* '..' [ \t]* Integer;
fragment Integer_base: '0'[bcxBCX];
fragment Digit: [0-9a-fA-F_];
Real: [0-9]* [.] [0-9]+ ([eE][+-]?[0-9]+)? | [0-9]+ [.] [0-9]* ([eE][+-]?[0-9]+)?;
FreeOperator: [!#$%&*+\-\\/:.<=>?@^|~\u0085-\u0089\u008B\u0095-\u0099\u009B\u00A1-\u00AC\u00B0-\u00B6\u00B9-\u00BF\u00D7\u00F7\u2200-\u22FF\u2A00-\u2AFF\u27C0-\u27EF\u2980-\u29FF\u2300-\u23FF\u25A0-\u25FF\u2190-\u21FF\u27F0-\u27FF\u2900-\u297F\u2B00-\u2BFF]+
   {isFreeOperator(getText())}?; // MOS: "a:=.33", <<-1>>, <<+1>>, :=+, ... recognized as FreeOperator!

WhiteSpace: [ \t\n\r\uFEFF]+ -> skip; // MOS: gobo eiffel files with <feff> (BOM) character! treated as whitespace
Comment: '--' .*? '\n' -> skip;

Error: .; // MOS: ensure no lexical errors

