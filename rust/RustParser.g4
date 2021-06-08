/*
Copyright (c) 2010 The Rust Project Developers
Copyright (c) 2020-2021 Student Main

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

parser grammar RustParser
   ;

options
{
   tokenVocab = RustLexer;
   superClass = RustParserBase;
}
// entry point
// 4
crate
   : innerAttribute* item* EOF
   ;

// 3
macroInvocation
   : simplePath '!' delimTokenTree
   ;
delimTokenTree
   : '(' tokenTree* ')'
   | '[' tokenTree* ']'
   | '{' tokenTree* '}'
   ;
tokenTree
   : tokenTreeToken+
   | delimTokenTree
   ;
tokenTreeToken
   : macroIdentifierLikeToken
   | macroLiteralToken
   | macroPunctuationToken
   | macroRepOp
   | '$'
   ;

macroInvocationSemi
   : simplePath '!' '(' tokenTree* ')' ';'
   | simplePath '!' '[' tokenTree* ']' ';'
   | simplePath '!' '{' tokenTree* '}'
   ;

// 3.1
macroRulesDefinition
   : 'macro_rules' '!' identifier macroRulesDef
   ;
macroRulesDef
   : '(' macroRules ')' ';'
   | '[' macroRules ']' ';'
   | '{' macroRules '}'
   ;
macroRules
   : macroRule (';' macroRule)* ';'?
   ;
macroRule
   : macroMatcher '=>' macroTranscriber
   ;
macroMatcher
   : '(' macroMatch* ')'
   | '[' macroMatch* ']'
   | '{' macroMatch* '}'
   ;
macroMatch
   : macroMatchToken+
   | macroMatcher
   | '$' (identifier | 'self') ':' macroFragSpec
   | '$' '(' macroMatch+ ')' macroRepSep? macroRepOp
   ;
macroMatchToken
   : macroIdentifierLikeToken
   | macroLiteralToken
   | macroPunctuationToken
   | macroRepOp
   ;
macroFragSpec
   : identifier // do validate here is wasting token
   ;
macroRepSep
   : macroIdentifierLikeToken
   | macroLiteralToken
   | macroPunctuationToken
   | '$'
   ;
macroRepOp
   : '*'
   | '+'
   | '?'
   ;
macroTranscriber
   : delimTokenTree
   ;

//configurationPredicate
// : configurationOption | configurationAll | configurationAny | configurationNot ; configurationOption: identifier (
// '=' (STRING_LITERAL | RAW_STRING_LITERAL))?; configurationAll: 'all' '(' configurationPredicateList? ')';
// configurationAny: 'any' '(' configurationPredicateList? ')'; configurationNot: 'not' '(' configurationPredicate ')';

//configurationPredicateList
// : configurationPredicate (',' configurationPredicate)* ','? ; cfgAttribute: 'cfg' '(' configurationPredicate ')';
// cfgAttrAttribute: 'cfg_attr' '(' configurationPredicate ',' cfgAttrs? ')'; cfgAttrs: attr (',' attr)* ','?;

// 6
item
   : outerAttribute* (visItem | macroItem)
   ;
visItem
   : visibility?
   (
      module
      | externCrate
      | useDeclaration
      | function_
      | typeAlias
      | struct_
      | enumeration
      | union_
      | constantItem
      | staticItem
      | trait_
      | implementation
      | externBlock
   )
   ;
macroItem
   : macroInvocationSemi
   | macroRulesDefinition
   ;

// 6.1
module
   : 'unsafe'? 'mod' identifier (';' | '{' innerAttribute* item* '}')
   ;

// 6.2
externCrate
   : 'extern' 'crate' crateRef asClause? ';'
   ;
crateRef
   : identifier
   | 'self'
   ;
asClause
   : 'as' (identifier | '_')
   ;

// 6.3
useDeclaration
   : 'use' useTree ';'
   ;
useTree
   : (simplePath? '::')? ('*' | '{' ( useTree (',' useTree)* ','?)? '}')
   | simplePath ('as' (identifier | '_'))?
   ;

// 6.4
function_
   : functionQualifiers 'fn' identifier genericParams? '(' functionParameters? ')' functionReturnType? whereClause?
      (blockExpression | ';')
   ;
functionQualifiers
   : 'const'? 'async'? 'unsafe'? ('extern' abi?)?
   ;
abi
   : STRING_LITERAL
   | RAW_STRING_LITERAL
   ;
functionParameters
   : selfParam ','?
   | (selfParam ',')? functionParam (',' functionParam)* ','?
   ;
selfParam
   : outerAttribute* (shorthandSelf | typedSelf)
   ;
shorthandSelf
   : ('&' lifetime?)? 'mut'? 'self'
   ;
typedSelf
   : 'mut'? 'self' ':' type_
   ;
functionParam
   : outerAttribute* (functionParamPattern | '...' | type_)
   ;
functionParamPattern
   : pattern ':' (type_ | '...')
   ;
functionReturnType
   : '->' type_
   ;

// 6.5
typeAlias
   : 'type' identifier genericParams? whereClause? ('=' type_)? ';'
   ;

// 6.6
struct_
   : structStruct
   | tupleStruct
   ;
structStruct
   : 'struct' identifier genericParams? whereClause? ('{' structFields? '}' | ';')
   ;
tupleStruct
   : 'struct' identifier genericParams? '(' tupleFields? ')' whereClause? ';'
   ;
structFields
   : structField (',' structField)* ','?
   ;
structField
   : outerAttribute* visibility? identifier ':' type_
   ;
tupleFields
   : tupleField (',' tupleField)* ','?
   ;
tupleField
   : outerAttribute* visibility? type_
   ;

// 6.7
enumeration
   : 'enum' identifier genericParams? whereClause? '{' enumItems? '}'
   ;
enumItems
   : enumItem (',' enumItem)* ','?
   ;
enumItem
   : outerAttribute* visibility? identifier
   (
      enumItemTuple
      | enumItemStruct
      | enumItemDiscriminant
   )?
   ;
enumItemTuple
   : '(' tupleFields? ')'
   ;
enumItemStruct
   : '{' structFields? '}'
   ;
enumItemDiscriminant
   : '=' expression
   ;

// 6.8
union_
   : 'union' identifier genericParams? whereClause? '{' structFields '}'
   ;

// 6.9
constantItem
   : 'const' (identifier | '_') ':' type_ ('=' expression)? ';'
   ;

// 6.10
staticItem
   : 'static' 'mut'? identifier ':' type_ ('=' expression)? ';'
   ;

// 6.11
trait_
   : 'unsafe'? 'trait' identifier genericParams? (':' typeParamBounds?)? whereClause? '{' innerAttribute* associatedItem* '}'
   ;

// 6.12
implementation
   : inherentImpl
   | traitImpl
   ;
inherentImpl
   : 'impl' genericParams? type_ whereClause? '{' innerAttribute* associatedItem* '}'
   ;
traitImpl
   : 'unsafe'? 'impl' genericParams? '!'? typePath 'for' type_ whereClause? '{' innerAttribute* associatedItem* '}'
   ;

// 6.13
externBlock
   : 'unsafe'? 'extern' abi? '{' innerAttribute* externalItem* '}'
   ;
externalItem
   : outerAttribute*
   (
      macroInvocationSemi
      | visibility? ( staticItem | function_)
   )
   ;

// 6.14
genericParams
   : '<' ((genericParam ',')* genericParam ','? )?'>'
   ;
genericParam
   : outerAttribute*
   (
      lifetimeParam
      | typeParam
      | constParam
   );
lifetimeParam
   : outerAttribute? LIFETIME_OR_LABEL (':' lifetimeBounds)?
   ;
typeParam
   : outerAttribute? identifier (':' typeParamBounds?)? ('=' type_)?
   ;
constParam
   : 'const' identifier ':' type_
   ;

whereClause
   : 'where' (whereClauseItem ',')* whereClauseItem?
   ;
whereClauseItem
   : lifetimeWhereClauseItem
   | typeBoundWhereClauseItem
   ;
lifetimeWhereClauseItem
   : lifetime ':' lifetimeBounds
   ;
typeBoundWhereClauseItem
   : forLifetimes? type_ ':' typeParamBounds?
   ;
forLifetimes
   : 'for' genericParams
   ;

// 6.15
associatedItem
   : outerAttribute*
   (
      macroInvocationSemi
      | visibility? ( typeAlias | constantItem | function_ )
   )
   ;

// 7
innerAttribute
   : '#' '!' '[' attr ']'
   ;
outerAttribute
   : '#' '[' attr ']'
   ;
attr
   : simplePath attrInput?
   ;
attrInput
   : delimTokenTree
   | '=' literalExpression
   ; // w/o suffix

//metaItem
// : simplePath ( '=' literalExpression //w | '(' metaSeq ')' )? ; metaSeq: metaItemInner (',' metaItemInner)* ','?;
// metaItemInner: metaItem | literalExpression; // w

//metaWord: identifier; metaNameValueStr: identifier '=' ( STRING_LITERAL | RAW_STRING_LITERAL); metaListPaths:
// identifier '(' ( simplePath (',' simplePath)* ','?)? ')'; metaListIdents: identifier '(' ( identifier (','
// identifier)* ','?)? ')'; metaListNameValueStr : identifier '(' (metaNameValueStr ( ',' metaNameValueStr)* ','?)? ')'
// ;

// 8
statement
   : ';'
   | item
   | letStatement
   | expressionStatement
   | macroInvocationSemi
   ;

letStatement
   : outerAttribute* 'let' pattern (':' type_)? ('=' expression)? ';'
   ;

expressionStatement
   : expression ';'
   | expressionWithBlock ';'?
   ;

// 8.2
expression
   : outerAttribute+ expression                         # AttributedExpression // technical, remove left recursive
   | literalExpression                                  # LiteralExpression_
   | pathExpression                                     # PathExpression_
   | expression '.' pathExprSegment '(' callParams? ')' # MethodCallExpression   // 8.2.10
   | expression '.' identifier                          # FieldExpression  // 8.2.11
   | expression '.' tupleIndex                          # TupleIndexingExpression   // 8.2.7
   | expression '.' 'await'                             # AwaitExpression  // 8.2.18
   | expression '(' callParams? ')'                     # CallExpression   // 8.2.9
   | expression '[' expression ']'                      # IndexExpression  // 8.2.6
   | expression '?'                                     # ErrorPropagationExpression   // 8.2.4
   | ('&' | '&&') 'mut'? expression                     # BorrowExpression // 8.2.4
   | '*' expression                                     # DereferenceExpression  // 8.2.4
   | ('-' | '!') expression                             # NegationExpression  // 8.2.4
   | expression 'as' typeNoBounds                       # TypeCastExpression  // 8.2.4
   | expression ('*' | '/' | '%') expression            # ArithmeticOrLogicalExpression   // 8.2.4
   | expression ('+' | '-') expression                  # ArithmeticOrLogicalExpression   // 8.2.4
   | expression (shl | shr) expression                  # ArithmeticOrLogicalExpression   // 8.2.4
   | expression '&' expression                          # ArithmeticOrLogicalExpression   // 8.2.4
   | expression '^' expression                          # ArithmeticOrLogicalExpression   // 8.2.4
   | expression '|' expression                          # ArithmeticOrLogicalExpression   // 8.2.4
   | expression comparisonOperator expression           # ComparisonExpression   // 8.2.4
   | expression '&&' expression                         # LazyBooleanExpression  // 8.2.4
   | expression '||' expression                         # LazyBooleanExpression  // 8.2.4
   | expression '..' expression?                        # RangeExpression  // 8.2.14
   | '..' expression?                                   # RangeExpression  // 8.2.14
   | '..=' expression                                   # RangeExpression  // 8.2.14
   | expression '..=' expression                        # RangeExpression  // 8.2.14
   | expression '=' expression                          # AssignmentExpression   // 8.2.4
   | expression compoundAssignOperator expression       # CompoundAssignmentExpression // 8.2.4
   | 'continue' LIFETIME_OR_LABEL? expression?          # ContinueExpression  // 8.2.13
   | 'break' LIFETIME_OR_LABEL? expression?             # BreakExpression  // 8.2.13
   | 'return' expression?                               # ReturnExpression // 8.2.17
   | '(' innerAttribute* expression ')'                 # GroupedExpression   // 8.2.5
   | '[' innerAttribute* arrayElements? ']'             # ArrayExpression  // 8.2.6
   | '(' innerAttribute* tupleElements? ')'             # TupleExpression  // 8.2.7
   | structExpression                                   # StructExpression_   // 8.2.8
   | enumerationVariantExpression                       # EnumerationVariantExpression_
   | closureExpression                                  # ClosureExpression_  // 8.2.12
   | expressionWithBlock                                # ExpressionWithBlock_
   | macroInvocation                                    # MacroInvocationAsExpression
   ;

comparisonOperator
   : '=='
   | '!='
   | '>'
   | '<'
   | '>='
   | '<='
   ;

compoundAssignOperator
   : '+='
   | '-='
   | '*='
   | '/='
   | '%='
   | '&='
   | '|='
   | '^='
   | '<<='
   | '>>='
   ;

expressionWithBlock
   : outerAttribute+ expressionWithBlock // technical
   | blockExpression
   | asyncBlockExpression
   | unsafeBlockExpression
   | loopExpression
   | ifExpression
   | ifLetExpression
   | matchExpression
   ;

// 8.2.1
literalExpression
   : CHAR_LITERAL
   | STRING_LITERAL
   | RAW_STRING_LITERAL
   | BYTE_LITERAL
   | BYTE_STRING_LITERAL
   | RAW_BYTE_STRING_LITERAL
   | INTEGER_LITERAL
   | FLOAT_LITERAL
   | KW_TRUE
   | KW_FALSE
   ;

// 8.2.2
pathExpression
   : pathInExpression
   | qualifiedPathInExpression
   ;

// 8.2.3
blockExpression
   : '{' innerAttribute* statements? '}'
   ;
statements
   : statement+ expression?
   | expression
   ;

asyncBlockExpression
   : 'async' 'move'? blockExpression
   ;
unsafeBlockExpression
   : 'unsafe' blockExpression
   ;

// 8.2.6
arrayElements
   : expression (',' expression)* ','?
   | expression ';' expression
   ;

// 8.2.7
tupleElements
   : (expression ',')+ expression?
   ;
tupleIndex
   : INTEGER_LITERAL
   ;

// 8.2.8
structExpression
   : structExprStruct
   | structExprTuple
   | structExprUnit
   ;
structExprStruct
   : pathInExpression '{' innerAttribute* (structExprFields | structBase)? '}'
   ;
structExprFields
   : structExprField (',' structExprField)* (',' structBase | ','?)
   ;
// outerAttribute here is not in doc
structExprField
   : outerAttribute* (identifier | (identifier | tupleIndex) ':' expression)
   ;
structBase
   : '..' expression
   ;
structExprTuple
   : pathInExpression '(' innerAttribute* (expression ( ',' expression)* ','?)? ')'
   ;
structExprUnit
   : pathInExpression
   ;

enumerationVariantExpression
   : enumExprStruct
   | enumExprTuple
   | enumExprFieldless
   ;
enumExprStruct
   : pathInExpression '{' enumExprFields? '}'
   ;
enumExprFields
   : enumExprField (',' enumExprField)* ','?
   ;
enumExprField
   : identifier
   | (identifier | tupleIndex) ':' expression
   ;
enumExprTuple
   : pathInExpression '(' (expression (',' expression)* ','?)? ')'
   ;
enumExprFieldless
   : pathInExpression
   ;

// 8.2.9
callParams
   : expression (',' expression)* ','?
   ;

// 8.2.12
closureExpression
   : 'move'? ('||' | '|' closureParameters? '|')
   (
      expression
      | '->' typeNoBounds blockExpression
   )
   ;
closureParameters
   : closureParam (',' closureParam)* ','?
   ;
closureParam
   : outerAttribute* pattern (':' type_)?
   ;

// 8.2.13
loopExpression
   : loopLabel?
   (
      infiniteLoopExpression
      | predicateLoopExpression
      | predicatePatternLoopExpression
      | iteratorLoopExpression
   )
   ;
infiniteLoopExpression
   : 'loop' blockExpression
   ;
predicateLoopExpression
   : 'while' expression /*except structExpression*/ blockExpression
   ;
predicatePatternLoopExpression
   : 'while' 'let' matchArmPatterns '=' expression blockExpression
   ;
iteratorLoopExpression
   : 'for' pattern 'in' expression blockExpression
   ;
loopLabel
   : LIFETIME_OR_LABEL ':'
   ;

// 8.2.15
ifExpression
   : 'if' expression blockExpression
   (
      'else' (blockExpression | ifExpression | ifLetExpression)
   )?
   ;
ifLetExpression
   : 'if' 'let' matchArmPatterns '=' expression blockExpression
   (
      'else' (blockExpression | ifExpression | ifLetExpression)
   )?
   ;

// 8.2.16
matchExpression
   : 'match' expression '{' innerAttribute* matchArms? '}'
   ;
matchArms
   : (matchArm '=>' matchArmExpression)* matchArm '=>' expression ','?
   ;
matchArmExpression
   : expression ','
   | expressionWithBlock ','?
   ;
matchArm
   : outerAttribute* matchArmPatterns matchArmGuard?
   ;
matchArmPatterns
   : '|'? pattern ('|' pattern)*
   ;
matchArmGuard
   : 'if' expression
   ;

// 9
pattern
   : patternWithoutRange
   | rangePattern
   ;
patternWithoutRange
   : literalPattern
   | identifierPattern
   | wildcardPattern
   | restPattern
   | obsoleteRangePattern
   | referencePattern
   | structPattern
   | tupleStructPattern
   | tuplePattern
   | groupedPattern
   | slicePattern
   | pathPattern
   | macroInvocation
   ;

literalPattern
   : KW_TRUE
   | KW_FALSE
   | CHAR_LITERAL
   | BYTE_LITERAL
   | STRING_LITERAL
   | RAW_STRING_LITERAL
   | BYTE_STRING_LITERAL
   | RAW_BYTE_STRING_LITERAL
   | '-'? INTEGER_LITERAL
   | '-'? FLOAT_LITERAL
   ;

identifierPattern
   : 'ref'? 'mut'? identifier ('@' pattern)?
   ;
wildcardPattern
   : '_'
   ;
restPattern
   : '..'
   ;
rangePattern
   : rangePatternBound '..=' rangePatternBound
   ;
obsoleteRangePattern
   : rangePatternBound '...' rangePatternBound
   ;
rangePatternBound
   : CHAR_LITERAL
   | BYTE_LITERAL
   | '-'? INTEGER_LITERAL
   | '-'? FLOAT_LITERAL
   | pathInExpression
   | qualifiedPathInExpression
   ;
referencePattern
   : ('&' | '&&') 'mut'? patternWithoutRange
   ;
structPattern
   : pathInExpression '{' structPatternElements? '}'
   ;
structPatternElements
   : structPatternFields (',' structPatternEtCetera?)?
   | structPatternEtCetera
   ;
structPatternFields
   : structPatternField (',' structPatternField)*
   ;
structPatternField
   : outerAttribute*
   (
      tupleIndex ':' pattern
      | identifier ':' pattern
      | 'ref'? 'mut'? identifier
   )
   ;
structPatternEtCetera
   : outerAttribute* '..'
   ;
tupleStructPattern
   : pathInExpression '(' tupleStructItems? ')'
   ;
tupleStructItems
   : pattern (',' pattern)* ','?
   ;
tuplePattern
   : '(' tuplePatternItems? ')'
   ;
tuplePatternItems
   : pattern ','
   | restPattern
   | pattern (',' pattern)+ ','?
   ;
groupedPattern
   : '(' pattern ')'
   ;
slicePattern
   : '[' slicePatternItems? ']'
   ;
slicePatternItems
   : pattern (',' pattern)* ','?
   ;
pathPattern
   : pathInExpression
   | qualifiedPathInExpression
   ;

// 10.1
type_
   : typeNoBounds
   | implTraitType
   | traitObjectType
   ;
typeNoBounds
   : parenthesizedType
   | implTraitTypeOneBound
   | traitObjectTypeOneBound
   | typePath
   | tupleType
   | neverType
   | rawPointerType
   | referenceType
   | arrayType
   | sliceType
   | inferredType
   | qualifiedPathInType
   | bareFunctionType
   | macroInvocation
   ;
parenthesizedType
   : '(' type_ ')'
   ;

// 10.1.4
neverType
   : '!'
   ;

// 10.1.5
tupleType
   : '(' ((type_ ',')+ type_?)? ')'
   ;

// 10.1.6
arrayType
   : '[' type_ ';' expression ']'
   ;

// 10.1.7
sliceType
   : '[' type_ ']'
   ;

// 10.1.13
referenceType
   : '&' lifetime? 'mut'? typeNoBounds
   ;
rawPointerType
   : '*' ('mut' | 'const') typeNoBounds
   ;

// 10.1.14
bareFunctionType
   : forLifetimes? functionTypeQualifiers 'fn' '(' functionParametersMaybeNamedVariadic? ')' bareFunctionReturnType?
   ;
functionTypeQualifiers
   : 'unsafe'? ('extern' abi?)?
   ;
bareFunctionReturnType
   : '->' typeNoBounds
   ;
functionParametersMaybeNamedVariadic
   : maybeNamedFunctionParameters
   | maybeNamedFunctionParametersVariadic
   ;
maybeNamedFunctionParameters
   : maybeNamedParam (',' maybeNamedParam)* ','?
   ;
maybeNamedParam
   : outerAttribute* ((identifier | '_') ':')? type_
   ;
maybeNamedFunctionParametersVariadic
   : (maybeNamedParam ',')* maybeNamedParam ',' outerAttribute* '...'
   ;

// 10.1.15
traitObjectType
   : 'dyn'? typeParamBounds
   ;
traitObjectTypeOneBound
   : 'dyn'? traitBound
   ;
implTraitType
   : 'impl' typeParamBounds
   ;
implTraitTypeOneBound
   : 'impl' traitBound
   ;

// 10.1.18
inferredType
   : '_'
   ;

// 10.6
typeParamBounds
   : typeParamBound ('+' typeParamBound)* '+'?
   ;
typeParamBound
   : lifetime
   | traitBound
   ;
traitBound
   : '?'? forLifetimes? typePath
   | '(' '?'? forLifetimes? typePath ')'
   ;
lifetimeBounds
   : (lifetime '+')* lifetime?
   ;
lifetime
   : LIFETIME_OR_LABEL
   | '\'static'
   | '\'_'
   ;

// 12.4
simplePath
   : '::'? simplePathSegment ('::' simplePathSegment)*
   ;
simplePathSegment
   : identifier
   | 'super'
   | 'self'
   | 'crate'
   | '$crate'
   ;

pathInExpression
   : '::'? pathExprSegment ('::' pathExprSegment)*
   ;
pathExprSegment
   : pathIdentSegment ('::' genericArgs)?
   ;
pathIdentSegment
   : identifier
   | 'super'
   | 'self'
   | 'Self'
   | 'crate'
   | '$crate'
   ;

//TODO: let x : T<_>=something;
genericArgs
   : '<' '>'
   | '<' genericArgsLifetimes (',' genericArgsTypes)? (',' genericArgsBindings)? ','? '>'
   | '<' genericArgsTypes (',' genericArgsBindings)? ','? '>'
   | '<' (genericArg ',')* genericArg ','? '>'
   ;
genericArg
   : lifetime
   | type_
   | genericArgsConst
   | genericArgsBinding
   ;
genericArgsConst
   : blockExpression
   | '-'? literalExpression
   | simplePathSegment
   ;
genericArgsLifetimes
   : lifetime (',' lifetime)*
   ;
genericArgsTypes
   : type_ (',' type_)*
   ;
genericArgsBindings
   : genericArgsBinding (',' genericArgsBinding)*
   ;
genericArgsBinding
   : identifier '=' type_
   ;

qualifiedPathInExpression
   : qualifiedPathType ('::' pathExprSegment)+
   ;
qualifiedPathType
   : '<' type_ ('as' typePath)? '>'
   ;
qualifiedPathInType
   : qualifiedPathType ('::' typePathSegment)+
   ;

typePath
   : '::'? typePathSegment ('::' typePathSegment)*
   ;
typePathSegment
   : pathIdentSegment '::'? (genericArgs | typePathFn)?
   ;
typePathFn
   : '(' typePathInputs? ')' ('->' type_)?
   ;
typePathInputs
   : type_ (',' type_)* ','?
   ;

// 12.6
visibility
   : 'pub' ('(' ( 'crate' | 'self' | 'super' | 'in' simplePath) ')')?
   ;

// technical
identifier
   : NON_KEYWORD_IDENTIFIER
   | RAW_IDENTIFIER
   | 'macro_rules'
   ;
keyword
   : KW_AS
   | KW_BREAK
   | KW_CONST
   | KW_CONTINUE
   | KW_CRATE
   | KW_ELSE
   | KW_ENUM
   | KW_EXTERN
   | KW_FALSE
   | KW_FN
   | KW_FOR
   | KW_IF
   | KW_IMPL
   | KW_IN
   | KW_LET
   | KW_LOOP
   | KW_MATCH
   | KW_MOD
   | KW_MOVE
   | KW_MUT
   | KW_PUB
   | KW_REF
   | KW_RETURN
   | KW_SELFVALUE
   | KW_SELFTYPE
   | KW_STATIC
   | KW_STRUCT
   | KW_SUPER
   | KW_TRAIT
   | KW_TRUE
   | KW_TYPE
   | KW_UNSAFE
   | KW_USE
   | KW_WHERE
   | KW_WHILE

   // 2018+
   | KW_ASYNC
   | KW_AWAIT
   | KW_DYN
   // reserved
   | KW_ABSTRACT
   | KW_BECOME
   | KW_BOX
   | KW_DO
   | KW_FINAL
   | KW_MACRO
   | KW_OVERRIDE
   | KW_PRIV
   | KW_TYPEOF
   | KW_UNSIZED
   | KW_VIRTUAL
   | KW_YIELD
   | KW_TRY
   | KW_UNION
   | KW_STATICLIFETIME
   ;
macroIdentifierLikeToken
   : keyword
   | identifier
   | KW_MACRORULES
   | KW_UNDERLINELIFETIME
   | KW_DOLLARCRATE
   | LIFETIME_OR_LABEL
   ;
macroLiteralToken
   : literalExpression
   ;
// macroDelimiterToken: '{' | '}' | '[' | ']' | '(' | ')';
macroPunctuationToken
   : '-'
   //| '+' | '*'
   | '/'
   | '%'
   | '^'
   | '!'
   | '&'
   | '|'
   | '&&'
   | '||'
   // already covered by '<' and '>' in macro | shl | shr
   | '+='
   | '-='
   | '*='
   | '/='
   | '%='
   | '^='
   | '&='
   | '|='
   | '<<='
   | '>>='
   | '='
   | '=='
   | '!='
   | '>'
   | '<'
   | '>='
   | '<='
   | '@'
   | '_'
   | '.'
   | '..'
   | '...'
   | '..='
   | ','
   | ';'
   | ':'
   | '::'
   | '->'
   | '=>'
   | '#'
   //| '$' | '?'
   ;

// LA can be removed, legal rust code still pass but the cost is `let c = a < < b` will pass... i hope antlr5 can add
// some new syntax? dsl? for these stuff so i needn't write it in (at least) 5 language

shl
   : '<' {this.next('<')}? '<'
   ;
shr
   : '>' {this.next('>')}? '>'
   ;