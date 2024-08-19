/*
Copyright (c) 2010 The Rust Project Developers
Copyright (c) 2020-2022 Student Main

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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar RustParser;

// Insert here @header for C++ parser.

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
    : simplePath NOT delimTokenTree
    ;

delimTokenTree
    : LPAREN tokenTree* RPAREN
    | LSQUAREBRACKET tokenTree* RSQUAREBRACKET
    | LCURLYBRACE tokenTree* RCURLYBRACE
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
    | DOLLAR
    ;

macroInvocationSemi
    : simplePath NOT LPAREN tokenTree* RPAREN SEMI
    | simplePath NOT LSQUAREBRACKET tokenTree* RSQUAREBRACKET SEMI
    | simplePath NOT LCURLYBRACE tokenTree* RCURLYBRACE
    ;

// 3.1
macroRulesDefinition
    : KW_MACRORULES NOT identifier macroRulesDef
    ;

macroRulesDef
    : LPAREN macroRules RPAREN SEMI
    | LSQUAREBRACKET macroRules RSQUAREBRACKET SEMI
    | LCURLYBRACE macroRules RCURLYBRACE
    ;

macroRules
    : macroRule (SEMI macroRule)* SEMI?
    ;

macroRule
    : macroMatcher FATARROW macroTranscriber
    ;

macroMatcher
    : LPAREN macroMatch* RPAREN
    | LSQUAREBRACKET macroMatch* RSQUAREBRACKET
    | LCURLYBRACE macroMatch* RCURLYBRACE
    ;

macroMatch
    : macroMatchToken+
    | macroMatcher
    | DOLLAR (identifier | KW_SELFVALUE) COLON macroFragSpec
    | DOLLAR LPAREN macroMatch+ RPAREN macroRepSep? macroRepOp
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
    | DOLLAR
    ;

macroRepOp
    : STAR
    | PLUS
    | QUESTION
    ;

macroTranscriber
    : delimTokenTree
    ;

//configurationPredicate
// : configurationOption | configurationAll | configurationAny | configurationNot ; configurationOption: identifier (
// EQ (STRING_LITERAL | RAW_STRING_LITERAL))?; configurationAll: 'all' LPAREN configurationPredicateList? RPAREN;
// configurationAny: 'any' LPAREN configurationPredicateList? RPAREN; configurationNot: 'not' LPAREN configurationPredicate RPAREN;

//configurationPredicateList
// : configurationPredicate (COMMA configurationPredicate)* COMMA? ; cfgAttribute: 'cfg' LPAREN configurationPredicate RPAREN;
// cfgAttrAttribute: 'cfg_attr' LPAREN configurationPredicate COMMA cfgAttrs? RPAREN; cfgAttrs: attr (COMMA attr)* COMMA?;

// 6
item
    : outerAttribute* (visItem | macroItem)
    ;

visItem
    : visibility? (
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
    : KW_UNSAFE? KW_MOD identifier (SEMI | LCURLYBRACE innerAttribute* item* RCURLYBRACE)
    ;

// 6.2
externCrate
    : KW_EXTERN KW_CRATE crateRef asClause? SEMI
    ;

crateRef
    : identifier
    | KW_SELFVALUE
    ;

asClause
    : KW_AS (identifier | UNDERSCORE)
    ;

// 6.3
useDeclaration
    : KW_USE useTree SEMI
    ;

useTree
    : (simplePath? PATHSEP)? (STAR | LCURLYBRACE ( useTree (COMMA useTree)* COMMA?)? RCURLYBRACE)
    | simplePath (KW_AS (identifier | UNDERSCORE))?
    ;

// 6.4
function_
    : functionQualifiers KW_FN identifier genericParams? LPAREN functionParameters? RPAREN functionReturnType? whereClause? (
        blockExpression
        | SEMI
    )
    ;

functionQualifiers
    : KW_CONST? KW_ASYNC? KW_UNSAFE? (KW_EXTERN abi?)?
    ;

abi
    : STRING_LITERAL
    | RAW_STRING_LITERAL
    ;

functionParameters
    : selfParam COMMA?
    | (selfParam COMMA)? functionParam (COMMA functionParam)* COMMA?
    ;

selfParam
    : outerAttribute* (shorthandSelf | typedSelf)
    ;

shorthandSelf
    : (AND lifetime?)? KW_MUT? KW_SELFVALUE
    ;

typedSelf
    : KW_MUT? KW_SELFVALUE COLON type_
    ;

functionParam
    : outerAttribute* (functionParamPattern | DOTDOTDOT | type_)
    ;

functionParamPattern
    : pattern COLON (type_ | DOTDOTDOT)
    ;

functionReturnType
    : RARROW type_
    ;

// 6.5
typeAlias
    : KW_TYPE identifier genericParams? whereClause? (EQ type_)? SEMI
    ;

// 6.6
struct_
    : structStruct
    | tupleStruct
    ;

structStruct
    : KW_STRUCT identifier genericParams? whereClause? (LCURLYBRACE structFields? RCURLYBRACE | SEMI)
    ;

tupleStruct
    : KW_STRUCT identifier genericParams? LPAREN tupleFields? RPAREN whereClause? SEMI
    ;

structFields
    : structField (COMMA structField)* COMMA?
    ;

structField
    : outerAttribute* visibility? identifier COLON type_
    ;

tupleFields
    : tupleField (COMMA tupleField)* COMMA?
    ;

tupleField
    : outerAttribute* visibility? type_
    ;

// 6.7
enumeration
    : KW_ENUM identifier genericParams? whereClause? LCURLYBRACE enumItems? RCURLYBRACE
    ;

enumItems
    : enumItem (COMMA enumItem)* COMMA?
    ;

enumItem
    : outerAttribute* visibility? identifier (
        enumItemTuple
        | enumItemStruct
        | enumItemDiscriminant
    )?
    ;

enumItemTuple
    : LPAREN tupleFields? RPAREN
    ;

enumItemStruct
    : LCURLYBRACE structFields? RCURLYBRACE
    ;

enumItemDiscriminant
    : EQ expression
    ;

// 6.8
union_
    : KW_UNION identifier genericParams? whereClause? LCURLYBRACE structFields RCURLYBRACE
    ;

// 6.9
constantItem
    : KW_CONST (identifier | UNDERSCORE) COLON type_ (EQ expression)? SEMI
    ;

// 6.10
staticItem
    : KW_STATIC KW_MUT? identifier COLON type_ (EQ expression)? SEMI
    ;

// 6.11
trait_
    : KW_UNSAFE? KW_TRAIT identifier genericParams? (COLON typeParamBounds?)? whereClause? LCURLYBRACE innerAttribute* associatedItem* RCURLYBRACE
    ;

// 6.12
implementation
    : inherentImpl
    | traitImpl
    ;

inherentImpl
    : KW_IMPL genericParams? type_ whereClause? LCURLYBRACE innerAttribute* associatedItem* RCURLYBRACE
    ;

traitImpl
    : KW_UNSAFE? KW_IMPL genericParams? NOT? typePath KW_FOR type_ whereClause? LCURLYBRACE innerAttribute* associatedItem* RCURLYBRACE
    ;

// 6.13
externBlock
    : KW_UNSAFE? KW_EXTERN abi? LCURLYBRACE innerAttribute* externalItem* RCURLYBRACE
    ;

externalItem
    : outerAttribute* (macroInvocationSemi | visibility? ( staticItem | function_))
    ;

// 6.14
genericParams
    : LT ((genericParam COMMA)* genericParam COMMA?)? GT
    ;

genericParam
    : outerAttribute* (lifetimeParam | typeParam | constParam)
    ;

lifetimeParam
    : outerAttribute? LIFETIME_OR_LABEL (COLON lifetimeBounds)?
    ;

typeParam
    : outerAttribute? identifier (COLON typeParamBounds?)? (EQ type_)?
    ;

constParam
    : KW_CONST identifier COLON type_
    ;

whereClause
    : KW_WHERE (whereClauseItem COMMA)* whereClauseItem?
    ;

whereClauseItem
    : lifetimeWhereClauseItem
    | typeBoundWhereClauseItem
    ;

lifetimeWhereClauseItem
    : lifetime COLON lifetimeBounds
    ;

typeBoundWhereClauseItem
    : forLifetimes? type_ COLON typeParamBounds?
    ;

forLifetimes
    : KW_FOR genericParams
    ;

// 6.15
associatedItem
    : outerAttribute* (macroInvocationSemi | visibility? ( typeAlias | constantItem | function_))
    ;

// 7
innerAttribute
    : POUND NOT LSQUAREBRACKET attr RSQUAREBRACKET
    ;

outerAttribute
    : POUND LSQUAREBRACKET attr RSQUAREBRACKET
    ;

attr
    : simplePath attrInput?
    ;

attrInput
    : delimTokenTree
    | EQ literalExpression
    ; // w/o suffix

//metaItem
// : simplePath ( EQ literalExpression //w | LPAREN metaSeq RPAREN )? ; metaSeq: metaItemInner (COMMA metaItemInner)* COMMA?;
// metaItemInner: metaItem | literalExpression; // w

//metaWord: identifier; metaNameValueStr: identifier EQ ( STRING_LITERAL | RAW_STRING_LITERAL); metaListPaths:
// identifier LPAREN ( simplePath (COMMA simplePath)* COMMA?)? RPAREN; metaListIdents: identifier LPAREN ( identifier (COMMA
// identifier)* COMMA?)? RPAREN; metaListNameValueStr : identifier LPAREN (metaNameValueStr ( COMMA metaNameValueStr)* COMMA?)? RPAREN
// ;

// 8
statement
    : SEMI
    | item
    | letStatement
    | expressionStatement
    | macroInvocationSemi
    ;

letStatement
    : outerAttribute* KW_LET patternNoTopAlt (COLON type_)? (EQ expression)? SEMI
    ;

expressionStatement
    : expression SEMI
    | expressionWithBlock SEMI?
    ;

// 8.2
expression
    : outerAttribute+ expression                                     # AttributedExpression // technical, remove left recursive
    | literalExpression                                              # LiteralExpression_
    | pathExpression                                                 # PathExpression_
    | expression DOT pathExprSegment LPAREN callParams? RPAREN       # MethodCallExpression          // 8.2.10
    | expression DOT identifier                                      # FieldExpression               // 8.2.11
    | expression DOT tupleIndex                                      # TupleIndexingExpression       // 8.2.7
    | expression DOT KW_AWAIT                                        # AwaitExpression               // 8.2.18
    | expression LPAREN callParams? RPAREN                           # CallExpression                // 8.2.9
    | expression LSQUAREBRACKET expression RSQUAREBRACKET            # IndexExpression               // 8.2.6
    | expression QUESTION                                            # ErrorPropagationExpression    // 8.2.4
    | (AND | ANDAND) KW_MUT? expression                              # BorrowExpression              // 8.2.4
    | STAR expression                                                # DereferenceExpression         // 8.2.4
    | (MINUS | NOT) expression                                         # NegationExpression            // 8.2.4
    | expression KW_AS typeNoBounds                                  # TypeCastExpression            // 8.2.4
    | expression (STAR | SLASH | PERCENT) expression                  # ArithmeticOrLogicalExpression // 8.2.4
    | expression (PLUS | MINUS) expression                            # ArithmeticOrLogicalExpression // 8.2.4
    | expression (shl | shr) expression                              # ArithmeticOrLogicalExpression // 8.2.4
    | expression AND expression                                      # ArithmeticOrLogicalExpression // 8.2.4
    | expression CARET expression                                    # ArithmeticOrLogicalExpression // 8.2.4
    | expression OR expression                                       # ArithmeticOrLogicalExpression // 8.2.4
    | expression comparisonOperator expression                       # ComparisonExpression          // 8.2.4
    | expression ANDAND expression                                   # LazyBooleanExpression         // 8.2.4
    | expression OROR expression                                     # LazyBooleanExpression         // 8.2.4
    | expression DOTDOT expression?                                  # RangeExpression               // 8.2.14
    | DOTDOT expression?                                             # RangeExpression               // 8.2.14
    | DOTDOTEQ expression                                            # RangeExpression               // 8.2.14
    | expression DOTDOTEQ expression                                 # RangeExpression               // 8.2.14
    | expression EQ expression                                       # AssignmentExpression          // 8.2.4
    | expression compoundAssignOperator expression                   # CompoundAssignmentExpression  // 8.2.4
    | KW_CONTINUE LIFETIME_OR_LABEL? expression?                     # ContinueExpression            // 8.2.13
    | KW_BREAK LIFETIME_OR_LABEL? expression?                        # BreakExpression               // 8.2.13
    | KW_RETURN expression?                                          # ReturnExpression              // 8.2.17
    | LPAREN innerAttribute* expression RPAREN                       # GroupedExpression             // 8.2.5
    | LSQUAREBRACKET innerAttribute* arrayElements? RSQUAREBRACKET   # ArrayExpression               // 8.2.6
    | LPAREN innerAttribute* tupleElements? RPAREN                   # TupleExpression               // 8.2.7
    | structExpression                                               # StructExpression_             // 8.2.8
    | enumerationVariantExpression                                   # EnumerationVariantExpression_
    | closureExpression                                              # ClosureExpression_            // 8.2.12
    | expressionWithBlock                                            # ExpressionWithBlock_
    | macroInvocation                                                # MacroInvocationAsExpression
    ;

comparisonOperator
    : EQEQ
    | NE
    | GT
    | LT
    | GE
    | LE
    ;

compoundAssignOperator
    : PLUSEQ
    | MINUSEQ
    | STAREQ
    | SLASHEQ
    | PERCENTEQ
    | ANDEQ
    | OREQ
    | CARETEQ
    | SHLEQ
    | SHREQ
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
    : LCURLYBRACE innerAttribute* statements? RCURLYBRACE
    ;

statements
    : statement+ expression?
    | expression
    ;

asyncBlockExpression
    : KW_ASYNC KW_MOVE? blockExpression
    ;

unsafeBlockExpression
    : KW_UNSAFE blockExpression
    ;

// 8.2.6
arrayElements
    : expression (COMMA expression)* COMMA?
    | expression SEMI expression
    ;

// 8.2.7
tupleElements
    : (expression COMMA)+ expression?
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
    : pathInExpression LCURLYBRACE innerAttribute* (structExprFields | structBase)? RCURLYBRACE
    ;

structExprFields
    : structExprField (COMMA structExprField)* (COMMA structBase | COMMA?)
    ;

// outerAttribute here is not in doc
structExprField
    : outerAttribute* (identifier | (identifier | tupleIndex) COLON expression)
    ;

structBase
    : DOTDOT expression
    ;

structExprTuple
    : pathInExpression LPAREN innerAttribute* (expression ( COMMA expression)* COMMA?)? RPAREN
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
    : pathInExpression LCURLYBRACE enumExprFields? RCURLYBRACE
    ;

enumExprFields
    : enumExprField (COMMA enumExprField)* COMMA?
    ;

enumExprField
    : identifier
    | (identifier | tupleIndex) COLON expression
    ;

enumExprTuple
    : pathInExpression LPAREN (expression (COMMA expression)* COMMA?)? RPAREN
    ;

enumExprFieldless
    : pathInExpression
    ;

// 8.2.9
callParams
    : expression (COMMA expression)* COMMA?
    ;

// 8.2.12
closureExpression
    : KW_MOVE? (OROR | OR closureParameters? OR) (expression | RARROW typeNoBounds blockExpression)
    ;

closureParameters
    : closureParam (COMMA closureParam)* COMMA?
    ;

closureParam
    : outerAttribute* pattern (COLON type_)?
    ;

// 8.2.13
loopExpression
    : loopLabel? (
        infiniteLoopExpression
        | predicateLoopExpression
        | predicatePatternLoopExpression
        | iteratorLoopExpression
    )
    ;

infiniteLoopExpression
    : KW_LOOP blockExpression
    ;

predicateLoopExpression
    : KW_WHILE expression /*except structExpression*/ blockExpression
    ;

predicatePatternLoopExpression
    : KW_WHILE KW_LET pattern EQ expression blockExpression
    ;

iteratorLoopExpression
    : KW_FOR pattern KW_IN expression blockExpression
    ;

loopLabel
    : LIFETIME_OR_LABEL COLON
    ;

// 8.2.15
ifExpression
    : KW_IF expression blockExpression (KW_ELSE (blockExpression | ifExpression | ifLetExpression))?
    ;

ifLetExpression
    : KW_IF KW_LET pattern EQ expression blockExpression (
        KW_ELSE (blockExpression | ifExpression | ifLetExpression)
    )?
    ;

// 8.2.16
matchExpression
    : KW_MATCH expression LCURLYBRACE innerAttribute* matchArms? RCURLYBRACE
    ;

matchArms
    : (matchArm FATARROW matchArmExpression)* matchArm FATARROW expression COMMA?
    ;

matchArmExpression
    : expression COMMA
    | expressionWithBlock COMMA?
    ;

matchArm
    : outerAttribute* pattern matchArmGuard?
    ;

matchArmGuard
    : KW_IF expression
    ;

// 9
pattern
    : OR? patternNoTopAlt (OR patternNoTopAlt)*
    ;

patternNoTopAlt
    : patternWithoutRange
    | rangePattern
    ;

patternWithoutRange
    : literalPattern
    | identifierPattern
    | wildcardPattern
    | restPattern
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
    | MINUS? INTEGER_LITERAL
    | MINUS? FLOAT_LITERAL
    ;

identifierPattern
    : KW_REF? KW_MUT? identifier (AT pattern)?
    ;

wildcardPattern
    : UNDERSCORE
    ;

restPattern
    : DOTDOT
    ;

rangePattern
    : rangePatternBound DOTDOTEQ rangePatternBound # InclusiveRangePattern
    | rangePatternBound DOTDOT                    # HalfOpenRangePattern
    | rangePatternBound DOTDOTDOT rangePatternBound # ObsoleteRangePattern
    ;

rangePatternBound
    : CHAR_LITERAL
    | BYTE_LITERAL
    | MINUS? INTEGER_LITERAL
    | MINUS? FLOAT_LITERAL
    | pathPattern
    ;

referencePattern
    : (AND | ANDAND) KW_MUT? patternWithoutRange
    ;

structPattern
    : pathInExpression LCURLYBRACE structPatternElements? RCURLYBRACE
    ;

structPatternElements
    : structPatternFields (COMMA structPatternEtCetera?)?
    | structPatternEtCetera
    ;

structPatternFields
    : structPatternField (COMMA structPatternField)*
    ;

structPatternField
    : outerAttribute* (tupleIndex COLON pattern | identifier COLON pattern | KW_REF? KW_MUT? identifier)
    ;

structPatternEtCetera
    : outerAttribute* DOTDOT
    ;

tupleStructPattern
    : pathInExpression LPAREN tupleStructItems? RPAREN
    ;

tupleStructItems
    : pattern (COMMA pattern)* COMMA?
    ;

tuplePattern
    : LPAREN tuplePatternItems? RPAREN
    ;

tuplePatternItems
    : pattern COMMA
    | restPattern
    | pattern (COMMA pattern)+ COMMA?
    ;

groupedPattern
    : LPAREN pattern RPAREN
    ;

slicePattern
    : LSQUAREBRACKET slicePatternItems? RSQUAREBRACKET
    ;

slicePatternItems
    : pattern (COMMA pattern)* COMMA?
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
    : LPAREN type_ RPAREN
    ;

// 10.1.4
neverType
    : NOT
    ;

// 10.1.5
tupleType
    : LPAREN ((type_ COMMA)+ type_?)? RPAREN
    ;

// 10.1.6
arrayType
    : LSQUAREBRACKET type_ SEMI expression RSQUAREBRACKET
    ;

// 10.1.7
sliceType
    : LSQUAREBRACKET type_ RSQUAREBRACKET
    ;

// 10.1.13
referenceType
    : AND lifetime? KW_MUT? typeNoBounds
    ;

rawPointerType
    : STAR (KW_MUT | KW_CONST) typeNoBounds
    ;

// 10.1.14
bareFunctionType
    : forLifetimes? functionTypeQualifiers KW_FN LPAREN functionParametersMaybeNamedVariadic? RPAREN bareFunctionReturnType?
    ;

functionTypeQualifiers
    : KW_UNSAFE? (KW_EXTERN abi?)?
    ;

bareFunctionReturnType
    : RARROW typeNoBounds
    ;

functionParametersMaybeNamedVariadic
    : maybeNamedFunctionParameters
    | maybeNamedFunctionParametersVariadic
    ;

maybeNamedFunctionParameters
    : maybeNamedParam (COMMA maybeNamedParam)* COMMA?
    ;

maybeNamedParam
    : outerAttribute* ((identifier | UNDERSCORE) COLON)? type_
    ;

maybeNamedFunctionParametersVariadic
    : (maybeNamedParam COMMA)* maybeNamedParam COMMA outerAttribute* DOTDOTDOT
    ;

// 10.1.15
traitObjectType
    : KW_DYN? typeParamBounds
    ;

traitObjectTypeOneBound
    : KW_DYN? traitBound
    ;

implTraitType
    : KW_IMPL typeParamBounds
    ;

implTraitTypeOneBound
    : KW_IMPL traitBound
    ;

// 10.1.18
inferredType
    : UNDERSCORE
    ;

// 10.6
typeParamBounds
    : typeParamBound (PLUS typeParamBound)* PLUS?
    ;

typeParamBound
    : lifetime
    | traitBound
    ;

traitBound
    : QUESTION? forLifetimes? typePath
    | LPAREN QUESTION? forLifetimes? typePath RPAREN
    ;

lifetimeBounds
    : (lifetime PLUS)* lifetime?
    ;

lifetime
    : LIFETIME_OR_LABEL
    | KW_STATICLIFETIME
    | KW_UNDERLINELIFETIME
    ;

// 12.4
simplePath
    : PATHSEP? simplePathSegment (PATHSEP simplePathSegment)*
    ;

simplePathSegment
    : identifier
    | KW_SUPER
    | KW_SELFVALUE
    | KW_CRATE
    | KW_DOLLARCRATE
    ;

pathInExpression
    : PATHSEP? pathExprSegment (PATHSEP pathExprSegment)*
    ;

pathExprSegment
    : pathIdentSegment (PATHSEP genericArgs)?
    ;

pathIdentSegment
    : identifier
    | KW_SUPER
    | KW_SELFVALUE
    | KW_SELFTYPE
    | KW_CRATE
    | KW_DOLLARCRATE
    ;

//TODO: let x : T<_>=something;
genericArgs
    : LT GT
    | LT genericArgsLifetimes (COMMA genericArgsTypes)? (COMMA genericArgsBindings)? COMMA? GT
    | LT genericArgsTypes (COMMA genericArgsBindings)? COMMA? GT
    | LT (genericArg COMMA)* genericArg COMMA? GT
    ;

genericArg
    : lifetime
    | type_
    | genericArgsConst
    | genericArgsBinding
    ;

genericArgsConst
    : blockExpression
    | MINUS? literalExpression
    | simplePathSegment
    ;

genericArgsLifetimes
    : lifetime (COMMA lifetime)*
    ;

genericArgsTypes
    : type_ (COMMA type_)*
    ;

genericArgsBindings
    : genericArgsBinding (COMMA genericArgsBinding)*
    ;

genericArgsBinding
    : identifier EQ type_
    ;

qualifiedPathInExpression
    : qualifiedPathType (PATHSEP pathExprSegment)+
    ;

qualifiedPathType
    : LT type_ (KW_AS typePath)? GT
    ;

qualifiedPathInType
    : qualifiedPathType (PATHSEP typePathSegment)+
    ;

typePath
    : PATHSEP? typePathSegment (PATHSEP typePathSegment)*
    ;

typePathSegment
    : pathIdentSegment PATHSEP? (genericArgs | typePathFn)?
    ;

typePathFn
    : LPAREN typePathInputs? RPAREN (RARROW type_)?
    ;

typePathInputs
    : type_ (COMMA type_)* COMMA?
    ;

// 12.6
visibility
    : KW_PUB (LPAREN ( KW_CRATE | KW_SELFVALUE | KW_SUPER | KW_IN simplePath) RPAREN)?
    ;

// technical
identifier
    : NON_KEYWORD_IDENTIFIER
    | RAW_IDENTIFIER
    | KW_MACRORULES
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

// macroDelimiterToken: LCURLYBRACE | RCURLYBRACE | LSQUAREBRACKET | RSQUAREBRACKET | LPAREN | RPAREN;
macroPunctuationToken
    : MINUS
    //| PLUS | STAR
    | SLASH
    | PERCENT
    | CARET
    | NOT
    | AND
    | OR
    | ANDAND
    | OROR
    // already covered by LT and GT in macro | shl | shr
    | PLUSEQ
    | MINUSEQ
    | STAREQ
    | SLASHEQ
    | PERCENTEQ
    | CARETEQ
    | ANDEQ
    | OREQ
    | SHLEQ
    | SHREQ
    | EQ
    | EQEQ
    | NE
    | GT
    | LT
    | GE
    | LE
    | AT
    | UNDERSCORE
    | DOT
    | DOTDOT
    | DOTDOTDOT
    | DOTDOTEQ
    | COMMA
    | SEMI
    | COLON
    | PATHSEP
    | RARROW
    | FATARROW
    | POUND
    //| DOLLAR | QUESTION
    ;

// LA can be removed, legal rust code still pass but the cost is `let c = a < < b` will pass... i hope antlr5 can add
// some new syntax? dsl? for these stuff so i needn't write it in (at least) 5 language

shl
    : LT {this.next('<')}? LT
    ;

shr
    : GT {this.next('>')}? GT
    ;
