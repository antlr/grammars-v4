/* Copyright (c) Tomas Kulhanek, STFC 2018
 * All rights reserved.
 *
 * MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

grammar PROV_N;

document
   : DOCUMENT (namespaceDeclarations)? (expression)* (bundle (bundle)*)? ENDDOCUMENT EOF
   ;

/* parser */
namespaceDeclarations
   : (defaultNamespaceDeclaration | namespaceDeclaration) namespaceDeclaration*
   ;

defaultNamespaceDeclaration
   : 'default' IRI_REF
   ;

/* TODO ambiquity with PREFIX token
   prefix ex <http://mynamespace> -- is recognized as 'prefix' token QUALIFIED_NAME token and 'namespace' token
   should check whether it matches PREFX as some chars are not allowed in PREFX

*/
namespaceDeclaration
   : 'prefix' PREFX namespace_
   ;

namespace_
   : IRI_REF
   ;

bundle
   : BUNDLE identifier (namespaceDeclarations)? (expression)* ENDBUNDLE
   ;

identifier
   : PREFX | QUALIFIED_NAME
   ;

expression
   : (entityExpression | activityExpression | generationExpression | usageExpression | startExpression | endExpression | invalidationExpression | communicationExpression | agentExpression | associationExpression | attributionExpression | delegationExpression | derivationExpression | influenceExpression | alternateExpression | specializationExpression | membershipExpression | extensibilityExpression)
   ;

entityExpression
   : 'entity' '(' identifier optionalAttributeValuePairs ')'
   ;

optionalAttributeValuePairs
   : (',' '[' attributeValuePairs ']')?
   ;

attributeValuePairs
   : (| attributeValuePair (',' attributeValuePair)*)
   ;

attributeValuePair
   : attribute '=' literal
   ;

attribute
   : PREFX | QUALIFIED_NAME
   ;

literal
   : typedLiteral
   | convenienceNotation
   ;

typedLiteral
   : STRING_LITERAL '%%' datatype
   ;

datatype
   : PREFX | QUALIFIED_NAME
   ;

convenienceNotation
   : STRING_LITERAL (LANGTAG)?
   | INT_LITERAL
   | QUALIFIED_NAME_LITERAL
   ;

activityExpression
   : 'activity' '(' identifier (',' timeOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

timeOrMarker
   : (time | '-')
   ;

time
   : DATETIME
   ;

generationExpression
   : 'wasGeneratedBy' '(' optionalIdentifier eIdentifier (',' aIdentifierOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

optionalIdentifier
   : (identifierOrMarker ';')?
   ;

identifierOrMarker
   : (identifier | '-')
   ;

eIdentifier
   : identifier
   ;

eIdentifierOrMarker
   : (eIdentifier | '-')
   ;

aIdentifierOrMarker
   : (aIdentifier | '-')
   ;

aIdentifier
   : identifier
   ;

agIdentifierOrMarker
   : (agIdentifier | '-')
   ;

agIdentifier
   : identifier
   ;

cIdentifier
   : identifier
   ;

gIdentifier
   : identifier
   ;

gIdentifierOrMarker
   : (gIdentifier | '-')
   ;

uIdentifier
   : identifier
   ;

uIdentifierOrMarker
   : (uIdentifier | '-')
   ;

usageExpression
   : 'used' '(' optionalIdentifier aIdentifier (',' eIdentifierOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

startExpression
   : 'wasStartedBy' '(' optionalIdentifier aIdentifier (',' eIdentifierOrMarker ',' aIdentifierOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

endExpression
   : 'wasEndedBy' '(' optionalIdentifier aIdentifier (',' eIdentifierOrMarker ',' aIdentifierOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

invalidationExpression
   : 'wasInvalidatedBy' '(' optionalIdentifier eIdentifier (',' aIdentifierOrMarker ',' timeOrMarker)? optionalAttributeValuePairs ')'
   ;

communicationExpression
   : 'wasInformedBy' '(' optionalIdentifier aIdentifier ',' aIdentifier optionalAttributeValuePairs ')'
   ;

agentExpression
   : 'agent' '(' identifier optionalAttributeValuePairs ')'
   ;

associationExpression
   : 'wasAssociatedWith' '(' optionalIdentifier aIdentifier (',' agIdentifierOrMarker ',' eIdentifierOrMarker)? optionalAttributeValuePairs ')'
   ;

attributionExpression
   : 'wasAttributedTo' '(' optionalIdentifier eIdentifier ',' agIdentifier optionalAttributeValuePairs ')'
   ;

delegationExpression
   : 'actedOnBehalfOf' '(' optionalIdentifier agIdentifier ',' agIdentifier (',' aIdentifierOrMarker)? optionalAttributeValuePairs ')'
   ;

derivationExpression
   : 'wasDerivedFrom' '(' optionalIdentifier eIdentifier ',' eIdentifier (',' aIdentifierOrMarker ',' gIdentifierOrMarker ',' uIdentifierOrMarker)? optionalAttributeValuePairs ')'
   ;

influenceExpression
   : 'wasInfluencedBy' '(' optionalIdentifier eIdentifier ',' eIdentifier optionalAttributeValuePairs ')'
   ;

alternateExpression
   : 'alternateOf' '(' eIdentifier ',' eIdentifier ')'
   ;

specializationExpression
   : 'specializationOf' '(' eIdentifier ',' eIdentifier ')'
   ;

membershipExpression
   : 'hadMember' '(' cIdentifier ',' eIdentifier ')'
   ;

extensibilityExpression
   : (PREFX|QUALIFIED_NAME) '(' optionalIdentifier extensibilityArgument (',' extensibilityArgument)* optionalAttributeValuePairs ')'
   ;

extensibilityArgument
   : (identifierOrMarker | literal | time | extensibilityExpression | extensibilityTuple)
   ;

extensibilityTuple
   : '{' extensibilityArgument (',' extensibilityArgument)* '}'
   | '(' extensibilityArgument (',' extensibilityArgument)* ')'
   ;

/* lexer */

DOCUMENT
   : 'document'
   ;


ENDDOCUMENT
   : 'endDocument'
   ;


BUNDLE
   : 'bundle'
   ;


ENDBUNDLE
   : 'endBundle'
   ;


WS
   : [ \t\r\n\u000C] + -> channel (HIDDEN)
   ;


COMMENT
   : '/*' .*? '*/' -> channel (HIDDEN)
   ;


LINE_COMMENT
   : '//' ~ [\r\n]* -> channel (HIDDEN)
   ;


IRI_REF
   : LESS ~ ('<' | '>' | '"' | '{' | '}' | '|' | '^' | '\\' | '`' | '\u0000' .. '\u0020')* GREATER
   ;


LESS
   : '<'
   ;


GREATER
   : '>'
   ;


DOT
   : '.'
   ;


MINUS
   : '-'
   ;


fragment PN_PREFIX
   : PN_CHARS_BASE ((PN_CHARS | DOT)* PN_CHARS)?
   ;


fragment PN_CHARS_BASE
   : 'A' .. 'Z' | 'a' .. 'z' | '\u00C0' .. '\u00D6' | '\u00D8' .. '\u00F6' | '\u00F8' .. '\u02FF' | '\u0370' .. '\u037D' | '\u037F' .. '\u1FFF' | '\u200C' .. '\u200D' | '\u2070' .. '\u218F' | '\u2C00' .. '\u2FEF' | '\u3001' .. '\uD7FF' | '\uF900' .. '\uFDCF' | '\uFDF0' .. '\uFFFD'
   ;

/* Note PN_CHARS_BASE is same as NCNAMESTARTCHAR (XML Schema)
   http://www.w3.org/TR/rdf-sparql-query/#rPN_CHARS_U
   http://www.w3.org/2010/01/Turtle/#prod-turtle2-PN_CHARS_U
 */

fragment PN_CHARS_U
   : PN_CHARS_BASE | '_'
   ;

/* Note: this is the same as NCNAMECHAR (XML Schema) except for '.'
   http://www.w3.org/TR/rdf-sparql-query/#rPN_CHARS
   http://www.w3.org/2010/01/Turtle/#prod-turtle2-PN_CHARS

*/

fragment PN_CHARS
   : PN_CHARS_U | MINUS | DIGIT | '\u00B7' | '\u0300' .. '\u036F' | '\u203F' .. '\u2040'
   ;


fragment DIGIT
   : '0' .. '9'
   ;


PREFX
   : PN_PREFIX
   ;

QUALIFIED_NAME
   : (PN_PREFIX ':')? PN_LOCAL | PN_PREFIX ':'
   ;


fragment PN_LOCAL
   : (PN_CHARS_U | DIGIT | PN_CHARS_OTHERS) ((PN_CHARS | DOT | PN_CHARS_OTHERS)* (PN_CHARS | PN_CHARS_OTHERS))?
   ;


fragment PN_CHARS_OTHERS
   : '/' | '@' | '~' | '&' | '+' | '*' | '?' | '#' | '$' | '!' | PERCENT | PN_CHARS_ESC
   ;


fragment PN_CHARS_ESC
   : '\\' ('=' | '\'' | '(' | ')' | ',' | '-' | ':' | ';' | '[' | ']' | '.')
   ;


fragment PERCENT
   : '%' HEX HEX
   ;


HEX
   : DIGIT | [A-F] | [a-f]
   ;


STRING_LITERAL
   : STRING_LITERAL2 | STRING_LITERAL_LONG2
   ;


INT_LITERAL
   : ('-')? (DIGIT) +
   ;


QUALIFIED_NAME_LITERAL
   : '\'' QUALIFIED_NAME '\''
   ;


ECHAR
   : '\\' [btnfr"'\\]
   ;


STRING_LITERAL2
   : '"' ((~ ["\\\r\n]) | ECHAR)* '"'
   ;


STRING_LITERAL_LONG2
   : '"""' (('"' | '""')? (~ ["\\] | ECHAR))* '"""'
   ;


DATETIME
   : '-'? ([1-9] [0-9] [0-9] [0-9] + | '0' [0-9] [0-9] [0-9]) '-' ('0' [1-9] | '1' [0-2]) '-' ('0' [1-9] | [12] [0-9] | '3' [01]) 'T' ((([01] [0-9]) | ('2' [0-3])) ':' [0-5] [0-9] ':' [0-5] [0-9] ('.' [0-9] +)? | ('24:00:00' ('.' '0' +)?)) ('Z' | ('+' | '-') (('0' [0-9] | '1' [0-3]) ':' [0-5] [0-9] | '14:00'))?
   ;


LANGTAG
   : '@' [a-zA-Z] + ('-' [a-zA-Z0-9] +)*
   ;
