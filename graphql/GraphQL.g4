/*
 The MIT License (MIT)
 
 Copyright (c) 2015 Joseph T. McBride
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 
 GraphQL grammar derived from:
 
 GraphQL Draft Specification - July 2015
 
 http://facebook.github.io/graphql/ https://github.com/facebook/graphql 
 
 AB:10-sep19: replaced type with type_ to resolve conflict for golang generator
 
 AB: 13-oct-19: added type system as per June 2018 specs
 */
grammar GraphQL;

document: definition+;

definition:
	execDefinition
	| typeSystemDefinition
	| typeSystemExtension;

// https://graphql.github.io/graphql-spec/June2018/#TypeSystemDefinition

typeSystemDefinition:
	schemaDefinition
	| typeDefinition
	| directiveDefinition;

// https://graphql.github.io/graphql-spec/June2018/#sec-Schema
schemaDefinition:
	'schema' directives rootOperationTypeDefinitionList;

rootOperationTypeDefinitionList:
	'{' rootOperationTypeDefinition (
		','? rootOperationTypeDefinition
	)* '}';
rootOperationTypeDefinition: operationType ':' namedType;
namedType: NAME;

//https://graphql.github.io/graphql-spec/June2018/#TypeDefinition
typeDefinition:
	scalarTypeDefinition
	| objectTypeDefinition
	| interfaceTypeDefinition
	| unionTypeDefinition
	| enumTypeDefinition
	| inputObjectTypeDefinition;

scalarTypeDefinition: description? 'scalar' NAME directives;
description: STRING;

// https://graphql.github.io/graphql-spec/June2018/#sec-Objects
objectTypeDefinition
: description? 'type' NAME 
implementsInterfaces?
directives?
fieldsDefinitions?;

implementsInterfaces: 'implements' '&'? type_ |
implementsInterfaces '&' type_;


fieldsDefinitions: '{'  fieldsDefinition+'}';
fieldsDefinition: description? NAME  argumentsDefinition? ':' type_  directives? ;
argumentsDefinition: '(' inputValueDefinition+ ')';
inputValueDefinition:  description? NAME ':' type_ defaultValue? directives?;

//https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
interfaceTypeDefinition
 : description? 'interface' NAME directives? fieldsDefinitions?;

// https://graphql.github.io/graphql-spec/June2018/#sec-Unions
unionTypeDefinition:  description? 'union' NAME directives? unionMemberTypes?;
unionMemberTypes: '='  type_ ('|' type_)* ;

unionTypeExtension : 'extend' unionTypeDefinition;

enumTypeDefinition:  description? 'enum' NAME directives? enumValuesDefinitions?; 
enumValuesDefinitions: '{' ( description? enumValue  directives?)+  '}';
enumValue: NAME ;// not (nullValue | booleanValue)

enumTypeExtension: 'extend' enumTypeDefinition;

//https://graphql.github.io/graphql-spec/June2018/#InputObjectTypeDefinition
inputObjectTypeDefinition: description? 'input' NAME directives? inputFieldsDefinition?;

inputFieldsDefinition: '{' inputValueDefinition+ '}';

directiveDefinition: description? 'directive' '@' NAME argumentsDefinition? 'on' directiveLocations;
directiveLocations: directiveLocation ('|' directiveLocations)*;
directiveLocation: executableDirectiveLocation | typeSystemDirectiveLocation;

executableDirectiveLocation: 
'QUERY' |
'MUTATION' |
'SUBSCRIPTION' |
'FIELD' |
'FRAGMENT_DEFINITION' |
'FRAGMENT_SPREAD' |
'INLINE_FRAGMENT';

typeSystemDirectiveLocation:
'SCHEMA' |
'SCALAR' |
'OBJECT' |
'FIELD_DEFINITION' |
'ARGUMENT_DEFINITION' |
'INTERFACE' |
'UNION' |
'ENUM' |
'ENUM_VALUE' |
'INPUT_OBJECT' |
'INPUT_FIELD_DEFINITION';

// https://graphql.github.io/graphql-spec/June2018/#sec-Type-System-Extensions
typeSystemExtension: schemaExtension | typeExtension;

schemaExtension: 'extend' schemaDefinition ;
typeExtension: 'extend' typeDefinition;

// original code: execution definitions 
// GraphQL Draft Specification - July 2015
execDefinition: operationDefinition | fragmentDefinition;

operationDefinition:
	selectionSet
	| operationType NAME variableDefinitions? directives? selectionSet;

selectionSet: '{' selection ( ','? selection)* '}';

operationType: 'query' | 'mutation' | 'subscription';

selection: field | fragmentSpread | inlineFragment;

field: fieldName arguments? directives? selectionSet?;

fieldName: alias | NAME;

alias: NAME ':' NAME;

arguments: '(' argument ( ',' argument)* ')';

argument: NAME ':' valueOrVariable;

fragmentSpread: '...' fragmentName directives?;

inlineFragment:
	'...' 'on' typeCondition directives? selectionSet;

fragmentDefinition:
	'fragment' fragmentName 'on' typeCondition directives? selectionSet;

fragmentName: NAME;

directives:  directive+;

directive:
	'@' NAME ':' valueOrVariable
	| '@' NAME
	| '@' NAME '(' argument ')';

typeCondition: typeName;

variableDefinitions:
	'(' variableDefinition (',' variableDefinition)* ')';

variableDefinition: variable ':' type_ defaultValue?;

variable: '$' NAME;

defaultValue: '=' value;

valueOrVariable: value | variable;

value:
	STRING		# stringValue
	| NUMBER	# numberValue
	| BOOLEAN	# booleanValue
	| array		# arrayValue
   | 'null'  # nullValue;



type_: typeName nonNullType? | listType nonNullType?;

typeName: NAME;

listType: '[' type_ ']';

nonNullType: '!';

array: '[' value ( ',' value)* ']' | '[' ']';

NAME: [_A-Za-z] [_0-9A-Za-z]*;

STRING: '"' ( ESC | ~ ["\\])* '"';

BOOLEAN: 'true' | 'false';

fragment ESC: '\\' ( ["\\/bfnrt] | UNICODE);

fragment UNICODE: 'u' HEX HEX HEX HEX;

fragment HEX: [0-9a-fA-F];

NUMBER: '-'? INT '.' [0-9]+ EXP? | '-'? INT EXP | '-'? INT;

fragment INT: '0' | [1-9] [0-9]*;

// no leading zeros

fragment EXP: [Ee] [+\-]? INT;

// \- since - means "range" inside [...]

WS: [ \t\n\r]+ -> skip;
