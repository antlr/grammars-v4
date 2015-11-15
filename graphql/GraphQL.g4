
/*
The MIT License (MIT)

Copyright (c) 2015 Joseph T. McBride

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

GraphQL grammar derived from:

    GraphQL Draft Specification - July 2015

    http://facebook.github.io/graphql/
    https://github.com/facebook/graphql

*/
grammar GraphQL;

document
   : definition+
   ;

definition
   : operationDefinition | fragmentDefinition
   ;

operationDefinition
   : selectionSet | operationType NAME variableDefinitions? directives? selectionSet
   ;

selectionSet
   : '{' selection ( ','? selection )* '}'
   ;

operationType
   : 'query' | 'mutation'
   ;

selection
   : field | fragmentSpread | inlineFragment
   ;

field
   : fieldName arguments? directives? selectionSet?
   ;

fieldName
   : alias | NAME
   ;

alias
   : NAME ':' NAME
   ;

arguments
   : '(' argument ( ',' argument )* ')'
   ;

argument
   : NAME ':' valueOrVariable
   ;

fragmentSpread
   : '...' fragmentName directives?
   ;

inlineFragment
   : '...' 'on' typeCondition directives? selectionSet
   ;

fragmentDefinition
   : 'fragment' fragmentName 'on' typeCondition directives? selectionSet
   ;

fragmentName
   : NAME
   ;

directives
   : directive+
   ;

directive
   : '@' NAME ':' valueOrVariable | '@' NAME | '@' NAME '(' argument ')'
   ;

typeCondition
   : typeName
   ;

variableDefinitions
   : '(' variableDefinition ( ',' variableDefinition )* ')'
   ;

variableDefinition
   : variable ':' type defaultValue?
   ;

variable
   : '$' NAME
   ;

defaultValue
   : '=' value
   ;

valueOrVariable
   : value | variable
   ;

value
   : STRING # stringValue | NUMBER # numberValue | BOOLEAN # booleanValue | array # arrayValue
   ;

type
   : typeName nonNullType? | listType nonNullType?
   ;

typeName
   : NAME
   ;

listType
   : '[' type ']'
   ;

nonNullType
   : '!'
   ;

array
   : '[' value ( ',' value )* ']' | '[' ']'
   ;


NAME
   : [_A-Za-z] [_0-9A-Za-z]*
   ;


STRING
   : '"' ( ESC | ~ ["\\] )* '"'
   ;


BOOLEAN
   : 'true' | 'false'
   ;


fragment ESC
   : '\\' ( ["\\/bfnrt] | UNICODE )
   ;


fragment UNICODE
   : 'u' HEX HEX HEX HEX
   ;


fragment HEX
   : [0-9a-fA-F]
   ;


NUMBER
   : '-'? INT '.' [0-9]+ EXP? | '-'? INT EXP | '-'? INT
   ;


fragment INT
   : '0' | [1-9] [0-9]*
   ;

// no leading zeros

fragment EXP
   : [Ee] [+\-]? INT
   ;

// \- since - means "range" inside [...]

WS
   : [ \t\n\r]+ -> skip
   ;
