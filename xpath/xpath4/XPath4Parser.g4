// XPath v4.0
// Author--Ken Domino
// Based on the XPath 4.0 WG Review Draft at https://qt4cg.org/specifications/xquery-40/xPath-40.html
//
// This is an implementation of the XPath version 4.0 grammar.

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar XPath4Parser;

options {
    tokenVocab = XPath4Lexer;
    superClass = XPath4ParserBase;
}

// Abbreviated steps: "..", "@nodeTest", or a simple node test (child axis abbreviation)
abbreviatedStep
    : '..'
    | '@' nodeTest
    | simpleNodeTest
    ;

absolutePathExpr
    : '/' relativePathExpr?
    | '//' relativePathExpr
    ;

additiveExpr
    : multiplicativeExpr (( '+' | '-') multiplicativeExpr)*
    ;

andExpr
    : comparisonExpr ('and' comparisonExpr)*
    ;

anyArrayType
    : 'array' '(' '*' ')'
    ;

anyFunctionType
    : ('function' | 'fn') '(' '*' ')'
    ;

anyItemType
    : 'item' '(' ')'
    ;

anyMapType
    : 'map' '(' '*' ')'
    ;

anyRecordType
    : 'record' '(' '*' ')'
    ;

anyXNodeType
    : 'node' '(' ')'
    ;

argument
    : exprSingle
    | argumentPlaceholder
    ;

// Argument list: now supports keyword arguments (new in XPath 4.0)
argumentList
    : '(' (positionalArguments ( ',' keywordArguments)? | keywordArguments)? ')'
    ;

argumentPlaceholder
    : '?'
    ;

arrayConstructor
    : squareArrayConstructor
    | curlyArrayConstructor
    ;

// Array types
arrayType
    : anyArrayType
    | typedArrayType
    ;

// ArrowExpr: supports both "=>" (sequence arrow) and "=!>" (mapping arrow)
arrowExpr
    : unaryExpr (sequenceArrowTarget | mappingArrowTarget)*
    ;

// Arrow target: named function call or restricted dynamic call
arrowTarget
    : functionCall
    | restrictedDynamicCall
    ;

attributeName
    : eqName
    ;

attributeNodeType
    : 'attribute' '(' (nameTestUnion ( ',' typeName_)?)? ')'
    ;

// XPath 4.0 adds four new or-self axes and four new or-self siblings
axis
    : (
        'ancestor'
        | 'ancestor-or-self'
        | 'attribute'
        | 'child'
        | 'descendant'
        | 'descendant-or-self'
        | 'following'
        | 'following-or-self'
        | 'following-sibling'
        | 'following-sibling-or-self'
        | 'namespace'
        | 'parent'
        | 'preceding'
        | 'preceding-or-self'
        | 'preceding-sibling'
        | 'preceding-sibling-or-self'
        | 'self'
    ) '::'
    ;

// Axis step: abbreviated or full, followed by zero or more predicates/lookups
axisStep
    : (abbreviatedStep | fullStep) (predicate | lookup)*
    ;

// Block form for if: if (cond) { expr }  (new in XPath 4.0)
bracedAction
    : enclosedExpr
    ;

// Expression operators in precedence order (lowest to highest)

// CastableExpr: now uses CastTarget + OccurrenceIndicator instead of SingleType
castableExpr
    : castExpr ('castable' 'as' castTarget occurrenceIndicator?)?
    ;

castExpr
    : pipelineExpr ('cast' 'as' castTarget occurrenceIndicator?)?
    ;

// CastTarget: richer than XPath 3.1's SingleType -- can target composite types
castTarget
    : typeName_
    | choiceItemType
    | enumerationType
    | typedArrayType
    | typedMapType
    | typedRecordType
    ;

// ChoiceItemType: parenthesized item type or union of item types, e.g. (T) or (T1 | T2)
choiceItemType
    : '(' itemType ('|' itemType)* ')'
    ;

commentNodeType
    : 'comment' '(' ')'
    ;

comparisonExpr
    : otherwiseExpr ((valueComp | generalComp | nodeComp) otherwiseExpr)?
    ;

compAttrConstructor
    : 'attribute' compNodeName enclosedExpr
    ;

compCommentConstructor
    : 'comment' enclosedExpr
    ;

compDocConstructor
    : 'document' enclosedExpr
    ;

compElemConstructor
    : 'element' compNodeName enclosedContentExpr
    ;

compNamespaceConstructor
    : 'namespace' compNodeNCName enclosedExpr
    ;

// Computed node name: a QName literal (#name) or dynamic expression ({expr})
compNodeName
    : qNameLiteral
    | '{' expr '}'
    ;

// Computed NCName: a marked NCName (#ncname) or dynamic expression ({expr})
compNodeNCName
    : markedNCName
    | '{' expr '}'
    ;

compPIConstructor
    : 'processing-instruction' compNodeNCName enclosedExpr
    ;

compTextConstructor
    : 'text' enclosedExpr
    ;

computedConstructor
    : compDocConstructor
    | compElemConstructor
    | compAttrConstructor
    | compNamespaceConstructor
    | compTextConstructor
    | compCommentConstructor
    | compPIConstructor
    ;

// Constant: literal value, QName literal, or boolean function call
constant
    : StringLiteral
    | '-' numericLiteral
    | qNameLiteral
    | eqName '(' ')'
    ;

// ContextValueRef replaces ContextItemExpr from XPath 3.1 (still just ".")
contextValueRef
    : '.'
    ;

curlyArrayConstructor
    : 'array' enclosedExpr
    ;

// Namespace declarations (new in XPath 4.0)
defaultElementNamespaceDecl
    : 'declare' 'default' 'element' 'namespace' uriLiteral
    ;

documentNodeType
    : 'document-node' '(' (elementNodeType | schemaElementNodeType | nameTestUnion)? ')'
    ;

// Dynamic node test: { expr } -- node type computed at runtime (new in XPath 4.0)
dynamicNodeTest
    : enclosedExpr
    ;

elementName
    : eqName
    ;

elementNodeType
    : 'element' '(' (nameTestUnion ( ',' typeName_ '?'?)?)? ')'
    ;

enclosedContentExpr
    : enclosedExpr
    ;

enclosedExpr
    : '{' expr? '}'
    ;

// Enumeration types (new in XPath 4.0): enum("value1", "value2", ...)
enumerationType
    : 'enum' '(' StringLiteral (',' StringLiteral)* ')'
    ;

// EQName: an expanded QName -- keywords are also valid names in XPath
eqName
    : QName
    | URIQualifiedName
    | 'ancestor'
    | 'ancestor-or-self'
    | 'and'
    | 'array'
    | 'as'
    | 'at'
    | 'attribute'
    | 'cast'
    | 'castable'
    | 'child'
    | 'comment'
    | 'declare'
    | 'default'
    | 'descendant'
    | 'descendant-or-self'
    | 'div'
    | 'document-node'
    | 'document'
    | 'element'
    | 'else'
    | 'empty-sequence'
    | 'enum'
    | 'eq'
    | 'every'
    | 'except'
    | 'fn'
    | 'following'
    | 'following-or-self'
    | 'following-sibling'
    | 'following-sibling-or-self'
    | 'follows'
    | 'follows-or-is'
    | 'for'
    | 'function'
    | 'ge'
    | 'gnode'
    | 'gt'
    | 'idiv'
    | 'if'
    | 'in'
    | 'instance'
    | 'intersect'
    | 'is'
    | 'is-not'
    | 'item'
    | 'jnode'
    | 'key'
    | 'le'
    | 'let'
    | 'lt'
    | 'map'
    | 'member'
    | 'mod'
    | 'namespace'
    | 'namespace-node'
    | 'ne'
    | 'node'
    | 'of'
    | 'or'
    | 'otherwise'
    | 'parent'
    | 'precedes'
    | 'precedes-or-is'
    | 'preceding'
    | 'preceding-or-self'
    | 'preceding-sibling'
    | 'preceding-sibling-or-self'
    | 'processing-instruction'
    | 'record'
    | 'return'
    | 'satisfies'
    | 'schema-attribute'
    | 'schema-element'
    | 'self'
    | 'some'
    | 'text'
    | 'then'
    | 'to'
    | 'treat'
    | 'union'
    | 'value'
    ;

// [1]
expr
    : exprSingle (',' exprSingle)*
    ;

exprSingle
    : forExpr
    | letExpr
    | quantifiedExpr
    | ifExpr
    | orExpr
    ;

// FieldDeclaration: "?" marks the field as optional; "as SequenceType" gives its type
fieldDeclaration
    : fieldName '?'? ('as' sequenceType)?
    ;

fieldName
    : QName
    | StringLiteral
    ;

forBinding
    : forItemBinding
    | forMemberBinding
    | forEntryBinding
    ;

forClause
    : 'for' forBinding
    ;

// Entry binding (new in XPath 4.0): iterates over map key-value entries
forEntryBinding
    : (forEntryKeyBinding forEntryValueBinding | forEntryValueBinding) positionalVar? 'in' exprSingle
    ;

forEntryKeyBinding
    : 'key' varNameAndType
    ;

forEntryValueBinding
    : 'value' varNameAndType
    ;

// For expression: single ForClause followed by chained ForLetReturn body.
// Multiple bindings are expressed by chaining: for $x in A for $y in B return C
forExpr
    : forClause forLetReturn
    ;

// Standard item binding: for $x [as T] [at $p] in expr
forItemBinding
    : varNameAndType positionalVar? 'in' exprSingle
    ;

// The body of a for/let expression -- can chain another for, let, or terminate with return.
// This avoids the indirect left-recursion that the spec's ForLetReturn would otherwise create.
forLetReturn
    : forClause forLetReturn
    | letClause forLetReturn
    | 'return' exprSingle
    ;

// Member binding (new in XPath 4.0): iterates over array members
forMemberBinding
    : 'member' varNameAndType positionalVar? 'in' exprSingle
    ;

// Full step: axis "::" nodeTest
fullStep
    : axis nodeTest
    ;

functionBody
    : enclosedExpr
    ;

functionCall
    : { this.IsFuncCall() }? eqName argumentList
    ;

// Function item expressions
functionItemExpr
    : namedFunctionRef
    | inlineFunctionExpr
    ;

// FunctionSignature separates the parameter list from the return type
functionSignature
    : '(' paramList? ')' typeDeclaration?
    ;

// Function types: "fn" is now an alias for "function"
functionType
    : anyFunctionType
    | typedFunctionType
    ;

generalComp
    : '='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
    ;

// Generic node type (new in XPath 4.0): gnode()
gNodeType
    : 'gnode' '(' ')'
    ;

// If expression: now allows both traditional "then/else" and block "{...}" forms
ifExpr
    : 'if' '(' expr ')' (unbracedActions | bracedAction)
    ;

// InlineFunctionExpr: "fn" is now an alias for "function" (new in XPath 4.0)
inlineFunctionExpr
    : ('function' | 'fn') functionSignature functionBody
    ;

instanceofExpr
    : treatExpr ('instance' 'of' sequenceType)?
    ;

intersectExceptExpr
    : recordPutExpr (( 'intersect' | 'except') recordPutExpr)*
    ;

// ItemType: restructured in XPath 4.0
itemType
    : regularItemType
    | functionType
    | typeName_
    | choiceItemType
    ;

// JSON node type (new in XPath 4.0): jnode(selector [, SequenceType])
jNodeType
    : 'jnode' '(' ('*' | jRootSelector | QName | constant) (',' sequenceType)? ')'
    ;

jRootSelector
    : '(' ')'
    ;

// KeySpecifier: extended in XPath 4.0 to include ContextValueRef, VarRef, and LookupWildcard
keySpecifier
    : QName
    | literal
    | contextValueRef
    | varRef
    | parenthesizedExpr
    | lookupWildcard
    ;

keywordArgument
    : eqName ':=' argument
    ;

keywordArguments
    : keywordArgument (',' keywordArgument)*
    ;

// let $[ $x [as T] ] [as T] := expr  -- destructuring array binding
letArrayBinding
    : '$' '[' varNameAndType ']' typeDeclaration? ':=' exprSingle
    ;

letBinding
    : letValueBinding
    | letSequenceBinding
    | letArrayBinding
    | letMapBinding
    ;

letClause
    : 'let' letBinding
    ;

// Let expression: single LetClause followed by chained ForLetReturn body.
letExpr
    : letClause forLetReturn
    ;

// let ${ $x [as T] } [as T] := expr  -- destructuring map binding
letMapBinding
    : '$' '{' varNameAndType '}' typeDeclaration? ':=' exprSingle
    ;

// let $( $x [as T] ) [as T] := expr  -- destructuring sequence binding
letSequenceBinding
    : '$' '(' varNameAndType ')' typeDeclaration? ':=' exprSingle
    ;

// let $x [as T] := expr
letValueBinding
    : varNameAndType ':=' exprSingle
    ;

literal
    : numericLiteral
    | StringLiteral
    ;

lookup
    : '?' keySpecifier
    ;

lookupWildcard
    : '*'
    ;

mapConstructor
    : 'map' '{' (mapConstructorEntry ( ',' mapConstructorEntry)*)? '}'
    ;

// MapConstructorEntry: simplified to ExprSingle ":" ExprSingle (no separate key/value rules)
mapConstructorEntry
    : exprSingle ':' exprSingle
    ;

// =!> target(...args)  -- maps arrow operator (new in XPath 4.0)
mappingArrowTarget
    : '=!>' arrowTarget
    ;

// Map types
mapType
    : anyMapType
    | typedMapType
    ;

// #NCName -- marks an unqualified name for namespace/PI constructors
markedNCName
    : '#' QName
    ;

// XPath 4.0 adds Unicode × (U+00D7) and ÷ (U+00F7) as aliases for * and div
multiplicativeExpr
    : unionExpr (( '*' | '\u00D7' | 'div' | '\u00F7' | 'idiv' | 'mod') unionExpr)*
    ;

namedFunctionRef
    : eqName '#' IntegerLiteral /* xgc: reserved-function-names */
    ;

namespaceDecl
    : 'declare' 'namespace' QName '=' uriLiteral
    ;

namespaceNodeType
    : 'namespace-node' '(' ')'
    ;

nameTest
    : eqName
    | wildcard
    ;

// NameTestUnion: used in element/attribute node type tests
nameTestUnion
    : nameTest
    ;

// NodeComp: extended in XPath 4.0 with is-not, precedes, follows, precedes-or-is, follows-or-is
nodeComp
    : 'is'
    | 'is-not'
    | nodePrecedes
    | nodeFollows
    | 'precedes-or-is'
    | 'follows-or-is'
    ;

// Node constructors (computed constructors -- new in XPath 4.0 for XPath; from XQuery)
nodeConstructor
    : computedConstructor
    ;

nodeFollows
    : '>>'
    | 'follows'
    ;

nodePrecedes
    : '<<'
    | 'precedes'
    ;

// Node test: union, simple, or dynamic (new in XPath 4.0)
nodeTest
    : unionNodeTest
    | simpleNodeTest
    | dynamicNodeTest
    ;

numericLiteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

occurrenceIndicator
    : '?'
    | '*'
    | '+'
    ;

orExpr
    : andExpr ('or' andExpr)*
    ;

// OtherwiseExpr: new in XPath 4.0 -- sequence coalescing with "otherwise"
otherwiseExpr
    : stringConcatExpr ('otherwise' stringConcatExpr)*
    ;

// Shared sub-parts
paramList
    : varNameAndType (',' varNameAndType)*
    ;

parenthesizedExpr
    : '(' expr? ')'
    ;

// Path expressions
pathExpr
    : absolutePathExpr
    | relativePathExpr
    ;

// PipelineExpr: thin wrapper around ArrowExpr (placeholder for future pipeline operators)
pipelineExpr
    : arrowExpr
    ;

positionalArgumentList
    : '(' positionalArguments? ')'
    ;

positionalArguments
    : argument (',' argument)*
    ;

// Positional variable binding: at $pos
positionalVar
    : 'at' '$' eqName
    ;

// Postfix expressions: primaryExpr followed by zero or more postfix operations.
// PostfixExpr covers: FilterExpr, DynamicFunctionCall, LookupExpr, MethodCall
postfixExpr
    : primaryExpr (
        predicate
        | positionalArgumentList
        | lookup
        | '=?>' QName positionalArgumentList
    )*
    ;

predicate
    : '[' expr ']'
    ;

predicateList
    : predicate*
    ;

// Primary expressions: extended in XPath 4.0 with NodeConstructor and StringTemplate
primaryExpr
    : literal
    | varRef
    | parenthesizedExpr
    | contextValueRef
    | functionCall
    | nodeConstructor
    | functionItemExpr
    | mapConstructor
    | arrayConstructor
    | stringTemplate
    | unaryLookup
    ;

processingInstructionNodeType
    : 'processing-instruction' '(' (QName | StringLiteral)? ')'
    ;

// #EQName -- marks a qualified name for element/attribute constructors
qNameLiteral
    : '#' eqName
    ;

// Quantified expression: some/every with one or more bindings
quantifiedExpr
    : ('some' | 'every') quantifierBinding (',' quantifierBinding)* 'satisfies' exprSingle
    ;

quantifierBinding
    : varNameAndType 'in' exprSingle
    ;

rangeExpr
    : additiveExpr ('to' additiveExpr)?
    ;

// RecordPutExpr: new in XPath 4.0 -- record field update with "+:="
recordPutExpr
    : instanceofExpr ('+:=' instanceofExpr)*
    ;

// Record types (new in XPath 4.0): structural typing for maps
recordType
    : anyRecordType
    | typedRecordType
    ;

regularItemType
    : anyItemType
    | xNodeType
    | gNodeType
    | jNodeType
    | mapType
    | arrayType
    | recordType
    | enumerationType
    ;

relativePathExpr
    : stepExpr (( '/' | '//') stepExpr)*
    ;

// Restricted set of targets allowed for dynamic arrow calls
restrictedDynamicCall
    : (varRef | parenthesizedExpr | functionItemExpr | mapConstructor | arrayConstructor) positionalArgumentList
    ;

schemaAttributeNodeType
    : 'schema-attribute' '(' attributeName ')'
    ;

schemaElementNodeType
    : 'schema-element' '(' elementName ')'
    ;

// Name selector: a qualified name or wildcard (for abbreviated child/attribute axis)
selector
    : eqName
    | wildcard
    ;

// => target(...args)
sequenceArrowTarget
    : '=>' arrowTarget
    ;

sequenceType
    : 'empty-sequence' '(' ')'
    | itemType occurrenceIndicator?
    ;

// Simple map expression
simpleMapExpr
    : pathExpr ('!' pathExpr)*
    ;

// Simple node test: type test or element/attribute name selector
simpleNodeTest
    : typeTest
    | selector
    ;

squareArrayConstructor
    : '[' (exprSingle ( ',' exprSingle)*)? ']'
    ;

stepExpr
    : postfixExpr
    | axisStep
    ;

stringConcatExpr
    : rangeExpr ('||' rangeExpr)*
    ;

// String template: backtick-delimited interpolated string (new in XPath 4.0).
// The lexer captures the full template as a single token (simplified).
// A production implementation would parse embedded { expr } with lexer modes.
stringTemplate
    : StringTemplate
    ;

textNodeType
    : 'text' '(' ')'
    ;

treatExpr
    : castableExpr ('treat' 'as' sequenceType)?
    ;

typedArrayType
    : 'array' '(' sequenceType ')'
    ;

typeDeclaration
    : 'as' sequenceType
    ;

typedFunctionParam
    : ('$' eqName 'as')? sequenceType
    ;

// TypedFunctionParam: optional named parameter ("$name as") followed by a SequenceType
typedFunctionParamList
    : typedFunctionParam (',' typedFunctionParam)*
    ;

typedFunctionType
    : ('function' | 'fn') '(' typedFunctionParamList? ')' 'as' sequenceType
    ;

// TypedMapType now uses ItemType for key (not just AtomicOrUnionType)
typedMapType
    : 'map' '(' itemType ',' sequenceType ')'
    ;

typedRecordType
    : 'record' '(' fieldDeclaration (',' fieldDeclaration)* ')'
    ;

// Type name
typeName_
    : eqName
    ;

simpleTypeName
    : typeName_
    ;

// Type test: covers gnode, XML node types, and JSON node types
typeTest
    : gNodeType
    | xNodeType
    | jNodeType
    ;

unaryExpr
    : ('-' | '+')* valueExpr
    ;

unaryLookup
    : '?' keySpecifier
    ;

// Type declarations and sequence types

unbracedActions
    : 'then' exprSingle 'else' exprSingle
    ;

unionExpr
    : intersectExceptExpr (( 'union' | '|') intersectExceptExpr)*
    ;

// Union node test: (T1 | T2 | ...) -- selects nodes matching any of the tests
unionNodeTest
    : '(' simpleNodeTest ('|' simpleNodeTest)+ ')'
    ;

uriLiteral
    : StringLiteral
    ;

valueComp
    : 'eq'
    | 'ne'
    | 'lt'
    | 'le'
    | 'gt'
    | 'ge'
    ;

valueExpr
    : simpleMapExpr
    ;

// Variable name with optional type (replaces old "$ varname typeDeclaration?")
varNameAndType
    : '$' eqName typeDeclaration?
    ;

varRef
    : '$' eqName
    ;

// Wildcard: extended in XPath 4.0 with URIQualifiedStar (BracedURILiteral "*")
wildcard
    : '*'
    | QName ':*'
    | '*:' QName
    | BracedURILiteral '*'
    ;

// XML node types (XNodeType) -- replaces and renames the old KindTest rules
xNodeType
    : documentNodeType
    | elementNodeType
    | attributeNodeType
    | schemaElementNodeType
    | schemaAttributeNodeType
    | processingInstructionNodeType
    | commentNodeType
    | textNodeType
    | namespaceNodeType
    | anyXNodeType
    ;

// Top-level XPath expression. XPath 4.0 allows optional namespace declarations at the start.
xPath
    : (defaultElementNamespaceDecl ';')? (namespaceDecl ';')* expr EOF
    ;

// Entry point for testing: semicolon-separated expressions
auxilary
    : (expr ';')+ EOF
    ;