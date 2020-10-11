/*
BSD License

Copyright (c) 2020, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar simula;
LETTER:
 [a-zA-Z];
DIGIT
: [0-9];
space
: ' ';

identifier
: letter { letter | digit | _ };
unsignednumber;

: decimal-number [ exponent-part ]
| exponent-part;
decimal-number


: unsigned-integer [ decimal-fraction ]
| decimal-fraction;
decimal-fraction


: . unsigned-integer;

exponent-part
: ( & | && ) [ + | - ] unsigned-integer;

unsigned-integer
: digit { digit | _ }
| radix R radix-digit { radix-digit | _ };

radix
: '2' | '4' | '8' | '16';

radix-digit
: digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F';

string
: simple-string { string-separator simple-string };

string-separator
: token-separator { token-separator };

simple-string
: " { ISO-code | non-quote-character | "" } ";

ISO-code
: ! digit [ digit ] [ digit ] !;

non-quote-character
: any printing character (incl. space) except the string qoute ".;

character-constant
: '/'' character-designator '/'';

character-designator
: ISO-code
| non-quote-character
| ";

format-effector
: BS | HT | LF | VT | FF | CR;

token-separator
: a direct comment
| a space (except in simple strings and character constants)
| a format effector (except as noted for
;

type
: value-type
| reference-type
value-type
: arithmetic-type
| boolean
| character
arithmetic-type
: integer-type
| real-type
integer-type
: [ short ] integer
real-type
: [ long ] real
reference-type
: object-reference-type
| text
object-reference-type
: ref "(" qualification ")"
qualification
: class-identifier


expression
: value-expression
| reference-expression
| designational-expression
value-expression
: arithmetic-expression
| Boolean-expression
| character-expression
reference-expression
: object-expression
| text-expression
variable
: simple-variable-1
| subscripted-variable
simple-variable-1
: identifier-1
subscripted-variable
: array-identifier-1 "(" subscript-list ")"
array-identifier-1
: identifier-1
subscript-list
: subscript-expression { , subscript-expression }
subscript-expression
: arithmetic-expression
function-designator
: procedure-identifier-1 [ actual-parameter-part ]
procedure-identifier-1
: identifier-1
actual-parameter-part
: "(" actual-parameter { , actual-parameter } ")"
actual-parameter
: expression
| array-identifier-1
| switch-identifier
| procedure-identifier-1

identifier-1
: identifier
| remote-identifier
remote-identifier
: simple-object-expression . attribute-identifier
| text-primary . attribute-identifier
attribute-identifier
: identifier
Boolean-expression
: simple-Boolean-expression
| if-clause simple-Boolean-expression else Boolean-expression
simple-Boolean-expression
: Boolean-tertiary { or else Boolean-tertiary }
Boolean-tertiary
: equivalence { and then equivalence }
equivalence
: implication { eqv implication }
implication
: Boolean-term { imp Boolean-term }
Boolean-term
: Boolean-factor { or Boolean-factor }
Boolean-factor
: Boolean-secondary { and Boolean-secondary }
Boolean-secondary
: [ not ] Boolean-primary
Boolean-primary
: logical-value
| variable
| function-designator
| relation
| "(" Boolean-expression ")"
relation
: arithmetic-relation
| character-relation
| text-value-relation
| object-relation
| object-reference-relation
| text-reference-relation
value-relational-operator
: < | <= | = | >= | > | <>
reference-comparator
= == | =/=
arithmetic-relation
: simple-arithmetic-expression
value-relational-operator simple-arithmetic-expression
character-relation
: simple-character-expression
value-relational-operator simple-character-expression
text-value-relation
: simple-text-expression
value-relational-operator simple-text-expression
object-relation
: simple-object-expression is class-identifier
| simple-object-expression in class-identifier
object-reference-relation
: simple-object-expression
reference-comparator simple-object-expression
text-reference-relation
: simple-text-expression
reference-comparator simple-text-expression
arithmetic-expression
: simple-arithmetic-expression
| if-clause simple-arithmetic-expression
else arithmetic-expression
simple-arithmetic-expression
: [ + | - ] term { ( + | - ) term }
term
: factor { ( * | / | // ) factor }
factor


: primary { ** primary }


primary
: unsigned-number
| variable
| function-designator
| "(" arithmetic-expression ")"
character-expression
: simple-character-expression
| if-clause simple-character-expression
else character-expression
simple-character-expression
: character-constant
| variable
| function-designator
| "(" character-expression ")"
text-expression
: simple-text-expression
| if-clause simple-text-expression else text-expression
simple-text-expression
: text-primary { & text-primary }
text-primary
: notext
| string
| variable
| function-designator
| "(" text-expression ")"
object-expression
: simple-object-expression
| if-clause simple-object-expression else object-expression
simple-object-expression
: none
| variable
| function-designator
| object-generator
| local-object
| qualified-object
| "(" object-expression ")"
object-generator
: new class-identifier [ actual-parameter-part ]
local-object
: this class-identifier
qualified-object
: simple-object-expression qua class-identifier
designational-expression
: simple-designational-expression
| if-clause simple-designational-expression
else designational-expression
simple-designational-expression
: label
| switch-designator
| "(" designational-expression ")"
switch-designator
: switch-identifier "(" subscript-expression ")"
switch-identifier
: identifier
label
: identifier


statement
: { label : } unconditional-statement
| { label : } conditional-statement
| { label : } for-statement
unconditional-statement
: assignment-statement
| while-statement
| goto-statement

| procedure-statement
| object-generator
| connection-statement
| compound-statement
| block
| dummy-statement
| activation-statement
assignment-statement
: value-assignment
| reference-assignment
value-assignment
: value-left-part := value-right-part
value-left-part
: destination
| simple-text-expression
value-right-part
: value-expression
| text-expression
| value-assignment
destination
= variable
| procedure-identifier
reference-assignment
= reference-left-part :- reference-right-part
reference-left-part
= destination
reference-right-part
= reference-expression
| reference-assignment
conditional-statement
= if-clause { label : } unconditional-statement [ else statement ]
| if-clause { label : } for-statement
if-clause
= if Boolean-expression then
while-statement
= while Boolean-expression do statement
for-statement
= for-clause statement
for-clause
= for simple-variable for-right-part do
simple-variable
= identifier
for-right-part
= := value-for-list-element { , value-for-list-element }
= :- reference-for-list-element { , reference-for-list-element }
value-for-list-element
= value-expression [ while Boolean-expression ]
| text-expression
| arithmetic-expression
step arithmetic-expression until arithmetic-expression
reference-for-list-element
= reference-expression [ while Boolean-expression ]
goto-statement
= ( goto | go to ) designational-expression
procedure-statement
= procedure-identifier-1 [ actual-parameter-part ]
connection-statement
= inspect object-expression when-clause { when-clause }
[ otherwise-clause ]
| inspect object-expression do connection-block-2
[ otherwise-clause ]
when-clause
= when class-identifier do connection-block-1
otherwise-clause
= otherwise statement
connection-block-1
= statement
connection-block-2
= statement

compound-statement
= begin compound-tail
compound-tail
= statement { ; statement } end
block
= subblock
| prefixed-block
subblock
= block-head ; compound-tail
block-head
= begin declaration { ; declaration }
prefixed-block
= block-prefix main-block
block-prefix
= class-identifier [ actual-parameter-part ]
main-block
= block
| compound-statement
dummy-statement
= empty


declaration
= simple-variable-declaration
| array-declaration
| switch-declaration
| procedure-declaration
| class-declaration
| external-declaration
simple-variable-declaration
= type type-list
type-list
= type-list-element { , type-list-element }
type-list-element
= identifier
| constant-element
array-declaration
= [ type ] array array-segment { , array-segment }
array-segment
= array-identifier { , array-identifier }
"(" bound-pair-list ")"
array-identifier
= identifier
bound-pair-list
= bound-pair { , bound-pair }
bound-pair
= arithmetic-expression : arithmetic-expression
switch-declaration
= switch switch-identifier := switch-list
switch-list
= designational-expression { , designational-expression }
procedure-declaration
= [ type ] procedure procedure-heading ; procedure-body
procedure-heading
= procedure-identifier
[ formal-parameter-part ; [ mode-part ] specification-part ]
procedure-body
= statement
procedure-identifier
= identifier
formal-parameter-part
= "(" formal-parameter { , formal-parameter } ")"
formal-parameter
= identifier
specification-part
= specifier identifier-list ; { specifier identifier-list ; }
specifier

= type [ array | procedure ]
| label
| switch
mode-part
= name-part [ value-part ]
| value-part [ name-part ]
name-part
= name identifier-list ;
value-part
= value identifier-list ;
identifier-list
= identifier { , identifier }
class-declaration
= [ prefix ] main-part
prefix
= class-identifier
main-part
= class class-identifier
[ formal-parameter-part ; [ value-part ]
specification-part ] ;
[ protection-part ; ] [ virtual-part ; ]
class-body
class-identifier
= identifier
class-body
= statement
| split-body
split-body
= initial-operations inner-part final-operations
initial-operations
= ( begin | block-head ; ) { statement ; }
inner-part
= { label : } inner
final-operations
= end
| ; compound-tail
virtual-part
= virtual : virtual-spec ; { virtual-spec ; }
virtual-spec
= specifier identifier-list
| procedure procedure-identifier procedure-specification
protection-part
= protection-specification { ; protection-specification }

protection-specification
= hidden identifier-list
| protected identifier-list
| hidden protected identifier-list
| protected hidden identifier-list
constant-element
= identifier = value-expression
| identifier = string


SIMULA-source-module
= [ external-head ]
( program | procedure-declaration | class-declaration )
external-head
= external-declaration ; { external-declaration ; }
external-declaration
= external-procedure-declaration
| external-class-declaration
program
= statement
external-procedure-declaration
= external [ kind ] [ type ] procedure external-list
| external kind procedure external-item procedure-specification
kind
= identifier
procedure-specification
= is procedure-declaration
external-class-declaration
= external class external-list
external-list
= external-item { , external-item }
external-item
= identifier [ = external-identification ]
external-identification
= string


activation-statement
= activation-clause [ scheduling-clause ]
activation-clause
= activator object-expression
activator
= activate
| reactivate
scheduling-clause
= timing-clause
| ( before | after ) object-expression
timing-clause
= simple-timing-clause [ prior ]
simple-timing-clause
= ( at | delay ) arithmetic-expression

WS
   : [ \r\n\t] + -> skip
   ;
