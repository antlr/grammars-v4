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
crate
   : innerAttribute* item* EOF
   ;

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
   | '<' genericArgsBindings ','? '>'
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

module
   : 'unsafe'? 'mod' identifier (';' | '{' innerAttribute* item* '}')
   ;

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

useDeclaration
   : 'use' useTree ';'
   ;
useTree
   : (simplePath? '::')? ('*' | '{' ( useTree (',' useTree)* ','?)? '}')
   | simplePath ('as' (identifier | '_'))?
   ;

function_
   : functionQualifiers 'fn' identifier generics? '(' functionParameters? ')' functionReturnType? whereClause?
      blockExpression
   ;
functionQualifiers
   : asyncConstQualifiers? 'unsafe'? ('extern' abi?)?
   ;
asyncConstQualifiers
   : 'async'
   | 'const'
   ;
abi
   : STRING_LITERAL
   | RAW_STRING_LITERAL
   ;
functionParameters
   : functionParam (',' functionParam)* ','?
   ;
functionParam
   : outerAttribute* pattern ':' type_
   ;
functionReturnType
   : '->' type_
   ;

typeAlias
   : 'type' identifier generics? whereClause? '=' type_ ';'
   ;

struct_
   : structStruct
   | tupleStruct
   ;
structStruct
   : 'struct' identifier generics? whereClause? ('{' structFields? '}' | ';')
   ;
tupleStruct
   : 'struct' identifier generics? '(' tupleFields? ')' whereClause? ';'
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

enumeration
   : 'enum' identifier generics? whereClause? '{' enumItems? '}'
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

union_
   : 'union' identifier generics? whereClause? '{' structFields '}'
   ;

constantItem
   : 'const' (identifier | '_') ':' type_ '=' expression ';'
   ;
staticItem
   : 'static' 'mut'? identifier ':' type_ '=' expression ';'
   ;

trait_
   : 'unsafe'? 'trait' identifier generics? (':' typeParamBounds?)? whereClause? '{' innerAttribute* traitItem* '}'
   ;
traitItem
   : outerAttribute* visibility?
   (
      traitFunc
      | traitMethod
      | traitConst
      | traitType
      | macroInvocationSemi
   )
   ;
traitFunc
   : traitFunctionDecl (';' | blockExpression)
   ;
traitMethod
   : traitMethodDecl (';' | blockExpression)
   ;
traitFunctionDecl
   : functionQualifiers 'fn' identifier generics? '(' traitFunctionParameters? ')' functionReturnType? whereClause?
   ;
traitMethodDecl
   : functionQualifiers 'fn' identifier generics? '(' selfParam
   (
      ',' traitFunctionParam
   )* ',' ')' functionReturnType? whereClause?
   ;
traitFunctionParameters
   : traitFunctionParam (',' traitFunctionParam)* ','?
   ;
traitFunctionParam
   : outerAttribute* (pattern ':')? type_
   ;
traitConst
   : 'const' identifier ':' type_ ('=' expression)? ';'
   ;
traitType
   : 'type' identifier (':' typeParamBounds?)? ';'
   ;

implementation
   : inherentImpl
   | traitImpl
   ;
inherentImpl
   : 'impl' generics? type_ whereClause? '{' innerAttribute* inherentImplItem* '}'
   ;
inherentImplItem
   : outerAttribute*
   (
      macroInvocationSemi
      | visibility? ( constantItem | function_ | method)
   )
   ;
traitImpl
   : 'unsafe'? 'impl' generics? '!'? typePath 'for' type_ whereClause? '{' innerAttribute* traitImplItem* '}'
   ;
traitImplItem
   : outerAttribute*
   (
      macroInvocationSemi
      | visibility? ( typeAlias | constantItem | method | function_)
   )
   ;

externBlock
   : 'extern' abi? '{' innerAttribute* externalItem* '}'
   ;
externalItem
   : outerAttribute*
   (
      macroInvocationSemi
      | visibility? ( externalStaticItem | externalFunctionItem)
   )
   ;
externalStaticItem
   : 'static' 'mut'? identifier ':' type_ ';'
   ;
externalFunctionItem
   : 'fn' identifier generics? '('
   (
      namedFunctionParameters
      | namedFunctionParametersWithVariadics
   )? ')' functionReturnType? whereClause? ';'
   ;
namedFunctionParameters
   : namedFunctionParam (',' namedFunctionParam)* ','?
   ;
namedFunctionParam
   : outerAttribute* (identifier | '_') ':' type_
   ;
namedFunctionParametersWithVariadics
   : (namedFunctionParam ',')* namedFunctionParam ',' outerAttribute* '...'
   ;

generics
   : '<' genericParams '>'
   ;
genericParams
   : lifetimeParams
   | ( lifetimeParam ',')* typeParams
   ;
lifetimeParams
   : (lifetimeParam ',')* lifetimeParam?
   ;
lifetimeParam
   : outerAttribute? LIFETIME_OR_LABEL (':' lifetimeBounds)?
   ;
typeParams
   : (typeParam ',')* typeParam?
   ;
typeParam
   : outerAttribute? identifier (':' typeParamBounds?)? ('=' type_)?
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
   : 'for' '<' lifetimeParams '>'
   ;

method
   : functionQualifiers 'fn' identifier generics? '(' selfParam
   (
      ',' functionParam
   )* ','? ')' functionReturnType? whereClause? blockExpression
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

visibility
   : 'pub' ('(' ( 'crate' | 'self' | 'super' | 'in' simplePath) ')')?
   ;

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
expression
   : outerAttribute+ expression                         # AttributedExpression // technical, remove left recursive
   | literalExpression                                  # LiteralExpression_
   | pathExpression                                     # PathExpression_
   | expression '.' pathExprSegment '(' callParams? ')' # MethodCallExpression
   | expression '.' identifier                          # FieldExpression
   | expression '.' tupleIndex                          # TupleIndexingExpression
   | expression '.' 'await'                             # AwaitExpression
   | expression '(' callParams? ')'                     # CallExpression
   | expression '[' expression ']'                      # IndexExpression
   | expression '?'                                     # ErrorPropagationExpression
   | ('&' | '&&') 'mut'? expression                     # BorrowExpression
   | '*' expression                                     # DereferenceExpression
   | ('-' | '!') expression                             # NegationExpression
   | expression 'as' typeNoBounds                       # TypeCastExpression
   | expression ('*' | '/' | '%') expression            # ArithmeticOrLogicalExpression
   | expression ('+' | '-') expression                  # ArithmeticOrLogicalExpression
   | expression (shl | shr) expression                  # ArithmeticOrLogicalExpression
   | expression '&' expression                          # ArithmeticOrLogicalExpression
   | expression '^' expression                          # ArithmeticOrLogicalExpression
   | expression '|' expression                          # ArithmeticOrLogicalExpression
   | expression comparisonOperator expression           # ComparisonExpression
   | expression '&&' expression                         # LazyBooleanExpression
   | expression '||' expression                         # LazyBooleanExpression
   | expression '..' expression?                        # RangeExpression
   | '..' expression?                                   # RangeExpression
   | '..=' expression                                   # RangeExpression
   | expression '..=' expression                        # RangeExpression
   | expression '=' expression                          # AssignmentExpression
   | expression compoundAssignOperator expression       # CompoundAssignmentExpression
   | 'continue' LIFETIME_OR_LABEL? expression?          # ContinueExpression
   | 'break' LIFETIME_OR_LABEL? expression?             # BreakExpression
   | 'return' expression?                               # ReturnExpression
   | '(' innerAttribute* expression ')'                 # GroupedExpression
   | '[' innerAttribute* arrayElements? ']'             # ArrayExpression
   | '(' innerAttribute* tupleElements? ')'             # TupleExpression
   | structExpression                                   # StructExpression_
   | enumerationVariantExpression                       # EnumerationVariantExpression_
   | closureExpression                                  # ClosureExpression_
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

pathExpression
   : pathInExpression
   | qualifiedPathInExpression
   ;

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

arrayElements
   : expression (',' expression)* ','?
   | expression ';' expression
   ;
tupleElements
   : (expression ',')+ expression?
   ;
tupleIndex
   : INTEGER_LITERAL
   ;

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

callParams
   : expression (',' expression)* ','?
   ;

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
   : '[' slicePatternItems ']'
   ;
slicePatternItems
   : pattern (',' pattern)* ','?
   ;
pathPattern
   : pathInExpression
   | qualifiedPathInExpression
   ;

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
neverType
   : '!'
   ;
tupleType
   : '(' ((type_ ',')+ type_?)? ')'
   ;
arrayType
   : '[' type_ ';' expression ']'
   ;
sliceType
   : '[' type_ ']'
   ;
referenceType
   : '&' lifetime? 'mut'? typeNoBounds
   ;
rawPointerType
   : '*' ('mut' | 'const') typeNoBounds
   ;

bareFunctionType
   : forLifetimes? functionQualifiers 'fn' '(' functionParametersMaybeNamedVariadic? ')' bareFunctionReturnType?
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
inferredType
   : '_'
   ;

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