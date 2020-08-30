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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar acme;

design
   : (IMPORT (filename ';' | STRING_LITERAL ';'))* (typeDeclaration | familyDeclaration | designAnalysisDeclaration | propertyDeclaration | propertiesBlock | systemDeclaration)* EOF
   ;

filename
   : ('$' | '%')? IDENTIFIER ((('.' | ':' | '-' | '+' | '\\' | '\\\\' | '/' | '$' | '%'))+ IDENTIFIER)*
   ;

familyDeclaration
   : (FAMILY | STYLE) IDENTIFIER (';' | ('=' familyBody (';')?) | (EXTENDS lookup_SystemTypeByName (',' lookup_SystemTypeByName)* WITH familyBody (';')?))
   ;

familyBody
   : '{' '}'
   | '{' (typeDeclaration | systemStructure)+ '}'
   ;

typeDeclaration
   : elementTypeDeclaration
   | propertyTypeDeclaration
   ;

elementTypeDeclaration
   : elementProtoTypeDeclaration
   | componentTypeDeclaration
   | groupTypeDeclaration
   | connectorTypeDeclaration
   | portTypeDeclaration
   | roleTypeDeclaration
   ;

elementProtoTypeDeclaration
   : (ELEMENT TYPE IDENTIFIER ('=' parse_ElementProtoTypeDescription (';')? | ';') | ELEMENT TYPE IDENTIFIER EXTENDS lookup_ComponentTypeByName (',' lookup_ComponentTypeByName)* WITH parse_ElementProtoTypeDescription (';')?)
   ;

componentTypeDeclaration
   : (COMPONENT TYPE IDENTIFIER ('=' parse_ComponentDescription (';')? | ';') | COMPONENT TYPE IDENTIFIER EXTENDS lookup_ComponentTypeByName (',' lookup_ComponentTypeByName)* WITH parse_ComponentDescription (';')?)
   ;

groupTypeDeclaration
   : (GROUP TYPE IDENTIFIER ('=' parse_GroupDescription (';')? | ';') | GROUP TYPE IDENTIFIER EXTENDS lookup_GroupTypeByName (',' lookup_GroupTypeByName)* WITH parse_GroupDescription (';')?)
   ;

connectorTypeDeclaration
   : (CONNECTOR TYPE IDENTIFIER ('=' parse_ConnectorDescription (';')? | ';') | CONNECTOR TYPE IDENTIFIER EXTENDS lookup_ConnectorTypeByName (',' lookup_ConnectorTypeByName)* WITH parse_ConnectorDescription (';')?)
   ;

portTypeDeclaration
   : (PORT TYPE IDENTIFIER ('=' parse_PortDescription (';')? | ';') | PORT TYPE IDENTIFIER EXTENDS lookup_PortTypeByName (',' lookup_PortTypeByName)* WITH parse_PortDescription (';')?)
   ;

roleTypeDeclaration
   : (ROLE TYPE IDENTIFIER ('=' parse_RoleDescription (';')? | ';') | ROLE TYPE IDENTIFIER EXTENDS lookup_RoleTypeByName (',' lookup_RoleTypeByName)* WITH parse_RoleDescription (';')?)
   ;

lookup_SystemTypeByName
   : IDENTIFIER
   ;

lookup_ComponentTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_GroupTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_ConnectorTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_PortTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_RoleTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_PropertyTypeByName
   : (IDENTIFIER '.')? IDENTIFIER
   ;

lookup_arbitraryTypeByName
   : (propertyTypeDescription | SYSTEM | COMPONENT | GROUP | CONNECTOR | PORT | ROLE | PROPERTY | REPRESENTATION | nonPropertySetTypeExpression)
   ;

systemDeclaration
   : SYSTEM IDENTIFIER (':' lookup_SystemTypeByName (',' lookup_SystemTypeByName)*)? ('=' systemBody (';')? | ';')
   ;

systemBody
   : (NEW lookup_SystemTypeByName (',' lookup_SystemTypeByName)* | '{' '}' | '{' (systemStructure)+ '}') (EXTENDED WITH systemBody)?
   ;

systemStructure
   : componentDeclaration
   | componentsBlock
   | groupDeclaration
   | connectorDeclaration
   | connectorsBlock
   | portDeclaration
   | portsBlock
   | roleDeclaration
   | rolesBlock1
   | rolesBlock2
   | propertyDeclaration
   | propertiesBlock
   | attachmentsDeclaration
   | representationDeclaration
   | designAnalysisDeclaration
   | parse_DesignRule
   ;

parse_ElementProtoTypeDescription
   : '{' (propertyDeclaration | propertiesBlock | representationDeclaration)* '}'
   ;

groupDeclaration
   : GROUP IDENTIFIER (':' lookup_GroupTypeByName (',' lookup_GroupTypeByName)*)? (('=' parse_GroupDescription ';'?) | ';'?)
   ;

parse_GroupDescription
   : (NEW lookup_GroupTypeByName (',' lookup_GroupTypeByName)* | '{' (membersBlock | propertyDeclaration | propertiesBlock | parse_DesignRule)* '}') (EXTENDED WITH parse_GroupDescription)?
   ;

componentDeclaration
   : COMPONENT IDENTIFIER (':' lookup_ComponentTypeByName (',' lookup_ComponentTypeByName)*)? (('=' parse_ComponentDescription ';'?) | ';'?)
   ;

componentsBlock
   : COMPONENTS '{' (IDENTIFIER (':' lookup_ComponentTypeByName (',' lookup_ComponentTypeByName)*)? (('=' parse_ComponentDescription ';'?) | ';'?))* '}' (';')?
   ;

parse_ComponentDescription
   : (NEW lookup_ComponentTypeByName (',' lookup_ComponentTypeByName)* | '{' (portDeclaration | portsBlock | propertyDeclaration | propertiesBlock | representationDeclaration | parse_DesignRule)* '}') (EXTENDED WITH parse_ComponentDescription)?
   ;

connectorDeclaration
   : CONNECTOR IDENTIFIER (':' lookup_ConnectorTypeByName (',' lookup_ConnectorTypeByName)*)? (('=' parse_ConnectorDescription ';'?) | ';'?)
   ;

connectorsBlock
   : CONNECTORS '{' (IDENTIFIER (':' lookup_ConnectorTypeByName (',' lookup_ConnectorTypeByName)*)? (('=' parse_ConnectorDescription ';'?) | ';'?))* '}' (';')?
   ;

parse_ConnectorDescription
   : (NEW lookup_ConnectorTypeByName (',' lookup_ConnectorTypeByName)* | '{' (roleDeclaration | rolesBlock1 | rolesBlock2 | propertyDeclaration | propertiesBlock | representationDeclaration | parse_DesignRule)* '}') (EXTENDED WITH parse_ConnectorDescription)?
   ;

portDeclaration
   : PORT IDENTIFIER (':' lookup_PortTypeByName (',' lookup_PortTypeByName)*)? (('=' parse_PortDescription ';'?) | ';'?)
   ;

portsBlock
   : PORTS '{' (IDENTIFIER (':' lookup_PortTypeByName (',' lookup_PortTypeByName)*)? (('=' parse_PortDescription ';'?) | ';'?))* '}' (';')?
   ;

parse_PortDescription
   : (NEW lookup_PortTypeByName (',' lookup_PortTypeByName)* | '{' (propertyDeclaration | propertiesBlock | representationDeclaration | parse_DesignRule)* '}') (EXTENDED WITH parse_PortDescription)?
   ;

roleDeclaration
   : ROLE IDENTIFIER (':' lookup_RoleTypeByName (',' lookup_RoleTypeByName)*)? (('=' parse_RoleDescription ';'?) | ';'?)
   ;

membersBlock
   : MEMBERS '{' (qualifiedReference (';'))* '}' (';')?
   ;

qualifiedReference
   : IDENTIFIER (('.' IDENTIFIER))*
   ;

rolesBlock1
   : ROLES '{' (IDENTIFIER (':' lookup_RoleTypeByName (',' lookup_RoleTypeByName)*)? ('=' parse_RoleDescription ';'? | ';'?))* '}' (';')?
   ;

rolesBlock2
   : ROLES '{' (lookup_RoleTypeByName (',' lookup_RoleTypeByName)*)? '}' (';')?
   ;

parse_RoleDescription
   : (NEW lookup_RoleTypeByName (',' lookup_RoleTypeByName)* | '{' (propertyDeclaration | propertiesBlock | representationDeclaration | parse_DesignRule)* '}') (EXTENDED WITH parse_RoleDescription)?
   ;

attachmentsDeclaration
   : ((ATTACHMENTS ':'? '{' (IDENTIFIER '.' IDENTIFIER 'to' IDENTIFIER '.' IDENTIFIER ('{' (propertyDeclaration | propertiesBlock)* '}')? ';'?)* '}' (';')?) | (ATTACHMENT IDENTIFIER '.' IDENTIFIER 'to' IDENTIFIER '.' IDENTIFIER ('{' (propertyDeclaration | propertiesBlock)* '}')? ';'))
   ;

propertyDeclaration
   : PROPERTY parse_PropertyDescription ';'
   ;

propertiesBlock
   : PROPERTIES '{' (parse_PropertyDescription (';' parse_PropertyDescription | ';')*)? '}' (';')?
   ;

parse_PropertyDescription
   : (PROPERTY)? IDENTIFIER (':' propertyTypeDescription)? ('=' propertyValueDeclaration)? (PROPBEGIN parse_PropertyDescription (';' parse_PropertyDescription | ';')* PROPEND | PROPBEGIN PROPEND)?
   ;

propertyTypeDeclaration
   : PROPERTY TYPE IDENTIFIER ('=' (INT ';' | FLOAT ';' | STRING ';' | BOOLEAN ';' | ENUM ('{' IDENTIFIER (',' IDENTIFIER)* '}')? ';' | SET ('{' '}')? ';' | SET '{' propertyTypeDescription '}' ';' | SEQUENCE ('<' '>')? ';' | SEQUENCE '<' propertyTypeDescription '>' ';' | RECORD '[' parse_RecordFieldDescription (';' parse_RecordFieldDescription | ';')* ']' ';' | RECORD ('[' ']')? ';' | IDENTIFIER ';'))
   ;

propertyTypeDescription
   : ANY
   | INT
   | FLOAT
   | STRING
   | BOOLEAN
   | SET ('{' (propertyTypeDescription)? '}')?
   | SEQUENCE ('<' (propertyTypeDescription)? '>')?
   | RECORD '[' parse_RecordFieldDescription (';' parse_RecordFieldDescription | ';')* ']'
   | RECORD ('[' ']')?
   | ENUM ('{' IDENTIFIER (',' IDENTIFIER)* '}')?
   | ENUM ('{' '}')?
   | lookup_PropertyTypeByName
   ;

parse_RecordFieldDescription
   : IDENTIFIER (',' IDENTIFIER)* (':' propertyTypeDescription)?
   ;

propertyValueDeclaration
   : INTEGER_LITERAL
   | FLOATING_POINT_LITERAL
   | STRING_LITERAL
   | FALSE
   | TRUE
   | acmeSetValue
   | acmeSequenceValue
   | acmeRecordValue
   | IDENTIFIER
   ;

acmeSetValue
   : '{' '}'
   | '{' propertyValueDeclaration (',' propertyValueDeclaration)* '}'
   ;

acmeSequenceValue
   : '<' '>'
   | '<' propertyValueDeclaration (',' propertyValueDeclaration)* '>'
   ;

acmeRecordValue
   : ('[' recordFieldValue (';' recordFieldValue | ';')* ']' | '[' ']')
   ;

recordFieldValue
   : IDENTIFIER (':' propertyTypeDescription)? '=' propertyValueDeclaration
   ;

representationDeclaration
   : REPRESENTATION (IDENTIFIER '=')? '{' systemDeclaration (bindingsMapDeclaration)? '}' (';')?
   ;

bindingsMapDeclaration
   : BINDINGS '{' (bindingDeclaration)* '}' (';')?
   ;

bindingDeclaration
   : (IDENTIFIER '.')? IDENTIFIER 'to' (IDENTIFIER '.')? IDENTIFIER ('{' (propertyDeclaration | propertiesBlock)* '}')? ';'?
   ;

designAnalysisDeclaration
   : ((EXTERNAL (DESIGN)? ANALYSIS IDENTIFIER '(' formalParams ')' ':' (propertyTypeDescription | COMPONENT | GROUP | CONNECTOR | PORT | ROLE | SYSTEM | ELEMENT | TYPE) '=' javaMethodCallExpr ';') | ((DESIGN)? ANALYSIS IDENTIFIER '(' formalParams ')' ':' (propertyTypeDescription | COMPONENT | GROUP | CONNECTOR | PORT | ROLE | SYSTEM | ELEMENT | TYPE) '=' designRuleExpression ';'))
   ;

parse_DesignRule
   : (DESIGN)? (INVARIANT | HEURISTIC) designRuleExpression (PROPBEGIN parse_PropertyDescription (';' parse_PropertyDescription | ';')* PROPEND)? ';'
   ;

designRuleExpression
   : quantifiedExpression
   | booleanExpression
   ;

quantifiedExpression
   : ((FORALL | EXISTS (UNIQUE)?) IDENTIFIER ((':' | SET_DECLARE) (type | lookup_arbitraryTypeByName))? IN (setExpression | reference) '|' designRuleExpression)
   ;

booleanExpression
   : orExpression (AND orExpression)*
   ;

orExpression
   : impliesExpression (OR impliesExpression)*
   ;

impliesExpression
   : iffExpression (IMPLIES iffExpression)*
   ;

iffExpression
   : equalityExpression (IFF equalityExpression)*
   ;

equalityExpression
   : relationalExpression (EQ relationalExpression | NE relationalExpression)*
   ;

relationalExpression
   : additiveExpression ('<' additiveExpression | '>' additiveExpression | LE additiveExpression | GE additiveExpression)*
   ;

additiveExpression
   : multiplicativeExpression (PLUS multiplicativeExpression | MINUS multiplicativeExpression)*
   ;

multiplicativeExpression
   : unaryExpression (STAR unaryExpression | SLASH unaryExpression | REM unaryExpression)*
   ;

unaryExpression
   : BANG unaryExpression
   | MINUS unaryExpression
   | primitiveExpression
   ;

primitiveExpression
   : '(' designRuleExpression ')'
   | literalConstant
   | reference
   | setExpression
   ;

reference
   : IDENTIFIER (('.' IDENTIFIER) | ('.' TYPE) | ('.' COMPONENTS) | ('.' CONNECTORS) | ('.' PORTS) | ('.' ROLES) | ('.' MEMBERS) | ('.' PROPERTIES) | ('.' REPRESENTATIONS) | ('.' ATTACHEDPORTS) | ('.' ATTACHEDROLES))* ('(' actualParams ')')?
   ;

javaMethodCallExpr
   : IDENTIFIER ('.' IDENTIFIER)* '(' actualParams ')'
   ;

literalConstant
   : (INTEGER_LITERAL)
   | (FLOATING_POINT_LITERAL)
   | (STRING_LITERAL)
   | (TRUE)
   | (FALSE)
   | (COMPONENT)
   | (GROUP)
   | (CONNECTOR)
   | (PORT)
   | (ROLE)
   | (SYSTEM)
   | (ELEMENT)
   | (PROPERTY)
   | (INT)
   | (FLOAT)
   | (STRING)
   | (BOOLEAN)
   | (ENUM)
   | (SET)
   | (SEQUENCE)
   | (RECORD)
   ;

actualParams
   : (actualParam (',' actualParam)*)?
   ;

formalParams
   : (formalParam (',' formalParam)*)?
   ;

actualParam
   : designRuleExpression
   ;

formalParam
   : IDENTIFIER (',' IDENTIFIER)* ':' (ELEMENT | SYSTEM | COMPONENT | CONNECTOR | PORT | ROLE | TYPE | PROPERTY | REPRESENTATION | ANY | nonPropertySetTypeExpression | propertyTypeDescription)
   ;

nonPropertySetTypeExpression
   : SET '{' (ELEMENT | SYSTEM | COMPONENT | CONNECTOR | PORT | ROLE | TYPE | PROPERTY | REPRESENTATION | ANY) '}'
   ;

setExpression
   : (literalSet | setConstructor)
   ;

literalSet
   : ('{' '}' | '{' (literalConstant | reference) (',' (literalConstant | reference))* '}')
   ;

setConstructor
   : ('{' SELECT IDENTIFIER (':' lookup_arbitraryTypeByName)? IN (setExpression | reference) '|' designRuleExpression '}' | ('{' COLLECT IDENTIFIER '.' IDENTIFIER ':' lookup_arbitraryTypeByName '.' lookup_arbitraryTypeByName IN (setExpression | reference) '|' designRuleExpression '}'))
   ;

recordType
   : RECORD '[' recordItem (',' recordItem)* ']'
   ;

recordItem
   : IDENTIFIER ':' type
   ;

setType
   : SET '{' type '}'
   ;

sequenceType
   : SEQUENCE '{' type '}'
   ;

signature
   : type '<->' type
   ;

type
   : (IDENTIFIER ('.' IDENTIFIER)*)
   ;

primitiveType
   : COMPONENT
   | GROUP
   | CONNECTOR
   | PORT
   | ROLE
   | SYSTEM
   ;

element
   : (IDENTIFIER ('.' IDENTIFIER)*)
   | compoundElement
   ;

compoundElement
   : set
   | record
   | sequence
   ;

set
   : '{' element (',' element)* '}'
   ;

record
   : '[' IDENTIFIER '=' element (',' IDENTIFIER '=' element)* ']'
   ;

sequence
   : '<' element (',' element)* '>'
   ;

ANALYSIS
   : 'Analysis'
   ;

AND
   : 'and'
   ;

ANY
   : 'Any'
   ;

ATTACHMENT
   : 'Attachment'
   ;

ATTACHMENTS
   : 'Attachments'
   ;

ATTACHEDPORTS
   : 'AttachedPorts'
   ;

ATTACHEDROLES
   : 'AttachedRoles'
   ;

BANG
   : '!'
   ;

BINDINGS
   : 'Bindings'
   ;

COLLECT
   : 'Collect'
   ;

COMPONENT
   : 'Component'
   ;

COMPONENTS
   : 'Components'
   ;

CONNECTOR
   : 'Connector'
   ;

CONNECTORS
   : 'Connectors'
   ;

DESIGN
   : 'Design'
   ;

ELEMENT
   : 'Element'
   ;

ENUM
   : 'Enum'
   ;

EXTENDED
   : 'Extended'
   ;

EXTENDS
   : 'Extends'
   ;

EXTERNAL
   : 'External'
   ;

EXISTS
   : 'Exists'
   ;

EQ
   : '='
   ;

FAMILY
   : 'Family'
   ;

FORALL
   : 'Forall'
   ;

FLOAT
   : 'Float'
   ;

GROUP
   : 'Group'
   ;

GE
   : '>='
   ;

HEURISTIC
   : 'Heuristic'
   ;

IFF
   : 'Iff'
   ;

IMPORT
   : 'Import'
   ;

IN
   : 'In'
   ;

INT
   : 'Int'
   ;

INVARIANT
   : 'Invariant'
   ;

IMPLIES
   : 'Implies'
   ;

LE
   : '<='
   ;

NE
   : '!='
   ;

NEW
   : 'New'
   ;

MEMBERS
   : 'Members'
   ;

MINUS
   : '-'
   ;

OR
   : 'or'
   ;

PLUS
   : '+'
   ;

PORT
   : 'Port'
   ;

PORTS
   : 'Ports'
   ;

PROPERTY
   : 'Property'
   ;

PROPERTIES
   : 'Properties'
   ;

PROPBEGIN
   : 'PropBegin'
   ;

PROPEND
   : 'Propend'
   ;

RECORD
   : 'Record'
   ;

REPRESENTATION
   : 'Representation'
   ;

REM
   : 'Rem'
   ;

REPRESENTATIONS
   : 'Representations'
   ;

ROLE
   : 'Role'
   ;

ROLES
   : 'Roles'
   ;

SEQUENCE
   : 'Sequence'
   ;

SELECT
   : 'Select'
   ;

SET
   : 'Set'
   ;

SET_DECLARE
   : 'SetDeclare'
   ;

SLASH
   : '/'
   ;

STAR
   : '*'
   ;

STRING
   : 'String'
   ;

STYLE
   : 'Style'
   ;

SYSTEM
   : 'System'
   ;

TYPE
   : 'Type'
   ;

UNIQUE
   : 'Unique'
   ;

WITH
   : 'With'
   ;

TRUE
   : 'True'
   ;

FALSE
   : 'False'
   ;

BOOLEAN
   : TRUE
   | FALSE
   ;

FLOATING_POINT_LITERAL
   : [0-9]+ '.' [0-9]+
   ;

INTEGER_LITERAL
   : [0-9]+
   ;

STRING_LITERAL
   : '"' .*? '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9_-]*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

