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

lexer grammar EiffelLexer;

options {
    caseInsensitive = true;
    superClass = EiffelLexerBase;
}

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

SEMI_COLON: ';';
COLON: ':';
COMMA: ',';
EQUALS: '=';
OPEN_CURLY: '{';
CLOSE_CURLY: '}';
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
OPEN_BRACKET: '[';
CLOSE_BRACKET: ']';
QUESTION: '?';
DOT_DOT: '..';
LT: '<';
BANG: '!';
RIGHT_ARROW: '->';
LT_LT: '<<';
GT_GT: '>>';
GT: '>';
COLON_EQUAL: ':=';
DOT: '.';
PLUS: '+';
MINUS: '-';
OPEN_PAREN_BAR: '(|';
BAR_CLOSE_PAREN: '|)';
CIRCUMFLEX: '^';
STAR: '*';
SLASH: '/';
SLASH_SLASH: '//';
BS_BS: '\\\\';
SQUIG: '~';
SLASH_SQUIG: '/~';
SLASH_EQUAL: '/=';
LE: '<=';
GE: '>=';
DOLLAR: '$';

Identifier: Letter (Letter | [0-9_])*;
fragment Letter: [a-z\u00C0-\u00FF];
Verbatim_string: // MOS: based on example code usage (difficult to make it work correctly)
     '"[' [ \t]* '\n' (.*? '\n')? [ \t]* ']"'
   | '"{' [ \t]* '\n' (.*? '\n')? [ \t]* '}"'
   ;
Basic_manifest_string: '"' ('%"' | '%%' | StringLineBreak | ~[\n] )*? '"';
fragment StringLineBreak: '%' [ \t]* '\n' [ \t]* '%';
Character_constant: ['] (~[%'] | [%]. | [%][/]Integer[/] ) ['];
Integer: Integer_base? Digit+;
Integer_interval: Integer [ \t]* '..' [ \t]* Integer;
fragment Integer_base: '0'[bcx];
fragment Digit: [0-9a-f_];
Real: [0-9]* [.] [0-9]+ ([e][+-]?[0-9]+)? | [0-9]+ [.] [0-9]* ([e][+-]?[0-9]+)?;
FreeOperator: [!#$%&*+\-\\/:.<=>?@^|~\u0085-\u0089\u008B\u0095-\u0099\u009B\u00A1-\u00AC\u00B0-\u00B6\u00B9-\u00BF\u00D7\u00F7\u2200-\u22FF\u2A00-\u2AFF\u27C0-\u27EF\u2980-\u29FF\u2300-\u23FF\u25A0-\u25FF\u2190-\u21FF\u27F0-\u27FF\u2900-\u297F\u2B00-\u2BFF]+
   {this.IsFreeOperator()}?; // MOS: "a:=.33", <<-1>>, <<+1>>, :=+, ... recognized as FreeOperator!

WhiteSpace: [ \t\n\r\uFEFF]+ -> skip; // MOS: gobo eiffel files with <feff> (BOM) character! treated as whitespace
Comment: '--' .*? '\n' -> skip;

Error: .; // MOS: ensure no lexical errors

