// XPath v4.0
// Author--Ken Domino
// Based on the XPath 4.0 WG Review Draft at https://qt4cg.org/specifications/xquery-40/xpath-40.html#nt-bnf
//
// This is an implementation of the XPath version 4.0 grammar.

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar XPath4Parser;

options {
    tokenVocab = XPath4Lexer;
    superClass = XPath4ParserBase;
}


// Not in official Spec EBNF, but provided for testing and Trash toolkit.
// Official entry is xPath.
// See https://github.com/qt4cg/qtspecs/blob/060ec4f3a70b78326248be58691aca5e7b107e0d/specifications/grammar-40/xpath-grammar.xml#L34-L36

auxilary
    : (expr ';')+ EOF
    ;

// Start of official Spec EBNF translation.

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

arrayType
    : anyArrayType
    | typedArrayType
    ;

arrowExpr
    : unaryExpr (sequenceArrowTarget | mappingArrowTarget)*
    ;

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

axisStep
    : (abbreviatedStep | fullStep) predicate*
    ;

bracedAction
    : enclosedExpr
    ;

castableExpr
    : castExpr ('castable' 'as' castTarget occurrenceIndicator?)?
    ;

castExpr
    : pipelineExpr ('cast' 'as' castTarget occurrenceIndicator?)?
    ;

castTarget
    : typeName_
    | choiceItemType
    | enumerationType
    | typedArrayType
    | typedMapType
    | typedRecordType
    ;

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

compNodeName
    : qNameLiteral
    | '{' expr '}'
    ;

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

// Constant: literal value, QName literal, or boolean function call.
// Spec: ("true" "()")  |  ("false" "()")  — "true"/"false" are not reserved words,
// so they tokenise as QName; QName '(' ')' is the faithful syntactic approximation.
constant
    : StringLiteral
    | '-'? numericLiteral
    | qNameLiteral
    | QName '(' ')'
    ;

contextValueRef
    : '.'
    ;

curlyArrayConstructor
    : 'array' enclosedExpr
    ;

defaultElementNamespaceDecl
    : 'declare' 'default' 'element' 'namespace' uriLiteral
    ;

documentNodeType
    : 'document-node' '(' (elementNodeType | schemaElementNodeType | nameTestUnion)? ')'
    ;

// dynamicFunctionCall — intentionally absent as a named rule.
// The spec defines it as: PostfixExpr PositionalArgumentList, which creates indirect
// left recursion across PostfixExpr → DynamicFunctionCall → PostfixExpr.
// ANTLR4 cannot handle cross-rule left recursion, so FilterExpr, DynamicFunctionCall,
// LookupExpr, and MethodCall are all inlined into postfixExpr as a (…)* suffix loop.

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

enumerationType
    : 'enum' '(' StringLiteral (',' StringLiteral)* ')'
    ;

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

fieldDeclaration
    : fieldName ('as' sequenceType)?
    ;

// fieldName uses QName where the spec says NCName.
// Reason: the lexer defines QName before NCName, and QName matches bare unqualified
// names (via FragUnprefixedName) as well as prefix:local names. ANTLR4's first-match
// rule means the lexer always produces QName for a bare identifier — NCName is never
// emitted for unqualified names — so using NCName here would be dead code.
// The trade-off is that QName is over-general: it also accepts prefix:local names,
// which the spec disallows for FieldName. Fixing this properly would require
// restructuring the lexer so bare names tokenise as NCName and the parser composes
// QName as NCName (':' NCName)?, which would ripple through the entire grammar.
fieldName
    : QName
    | StringLiteral
    | URIQualifiedName
    ;

// filterExpr — intentionally absent as a named rule.
// The spec defines it as: PostfixExpr Predicate, which creates indirect
// left recursion across PostfixExpr → FilterExpr → PostfixExpr.
// ANTLR4 cannot handle cross-rule left recursion, so FilterExpr, DynamicFunctionCall,
// LookupExpr, and MethodCall are all inlined into postfixExpr as a (…)* suffix loop.

forBinding
    : forItemBinding
    | forMemberBinding
    | forEntryBinding
    ;

forClause
    : 'for' forBinding (',' forBinding)*
    ;

forEntryBinding
    : (forEntryKeyBinding forEntryValueBinding? | forEntryValueBinding) positionalVar? 'in' exprSingle
    ;

forEntryKeyBinding
    : 'key' varNameAndType
    ;

forEntryValueBinding
    : 'value' varNameAndType
    ;

// For expression: single ForClause followed by chained ForLetReturn body.
// Multiple bindings may appear within a clause (for $x in A, $y in B return C)
// or by chaining clauses (for $x in A for $y in B return C).
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
    : (forClause | letClause) forLetReturn
    | 'return' exprSingle
    ;

forMemberBinding
    : 'member' varNameAndType positionalVar? 'in' exprSingle
    ;

fullStep
    : axis nodeTest
    ;

functionBody
    : enclosedExpr
    ;

functionCall
    : { this.IsFuncCall() }? eqName argumentList
    ;

functionItemExpr
    : namedFunctionRef
    | inlineFunctionExpr
    ;

functionSignature
    : '(' paramList ')' typeDeclaration?
    ;

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

gNodeType
    : 'gnode' '(' ')'
    ;

ifExpr
    : 'if' '(' expr ')' (unbracedActions | bracedAction)
    ;

inlineFunctionExpr
    : ('function' | 'fn') functionSignature? functionBody
    ;

instanceofExpr
    : treatExpr ('instance' 'of' sequenceType)?
    ;

intersectExceptExpr
    : recordPutExpr (( 'intersect' | 'except') recordPutExpr)*
    ;

itemType
    : regularItemType
    | functionType
    | typeName_
    | choiceItemType
    ;

jNodeType
    : 'jnode' '(' (('*' | jRootSelector | QName | constant) (',' sequenceType)?)? ')'
    ;

jRootSelector
    : '(' ')'
    ;

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

letArrayBinding
    : '$' '[' varNameAndType (',' varNameAndType)* ']' typeDeclaration? ':=' exprSingle
    ;

letBinding
    : letValueBinding
    | letSequenceBinding
    | letArrayBinding
    | letMapBinding
    ;

letClause
    : 'let' letBinding (',' letBinding)*
    ;

letExpr
    : letClause forLetReturn
    ;

letMapBinding
    : '$' '{' varNameAndType (',' varNameAndType)* '}' typeDeclaration? ':=' exprSingle
    ;

letSequenceBinding
    : '$' '(' varNameAndType (',' varNameAndType)* ')' typeDeclaration? ':=' exprSingle
    ;

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

// lookupExpr — intentionally absent as a named rule.
// The spec defines it as: PostfixExpr Lookup, which creates indirect
// left recursion across PostfixExpr → LookupExpr → PostfixExpr.
// ANTLR4 cannot handle cross-rule left recursion, so FilterExpr, DynamicFunctionCall,
// LookupExpr, and MethodCall are all inlined into postfixExpr as a (…)* suffix loop.

lookupWildcard
    : '*'
    ;

mapConstructor
    : 'map'? '{' (mapConstructorEntry ( ',' mapConstructorEntry)*)? '}'
    ;

mapConstructorEntry
    : exprSingle (':' exprSingle)?
    ;

mappingArrowTarget
    : '=!>' arrowTarget
    ;

mapType
    : anyMapType
    | typedMapType
    ;

markedNCName
    : '#' QName
    ;

// methodCall — intentionally absent as a named rule.
// The spec defines it as: PostfixExpr "=?>" NCName PositionalArgumentList, which creates
// indirect left recursion across PostfixExpr → MethodCall → PostfixExpr.
// ANTLR4 cannot handle cross-rule left recursion, so FilterExpr, DynamicFunctionCall,
// LookupExpr, and MethodCall are all inlined into postfixExpr as a (…)* suffix loop.

multiplicativeExpr
    : unionExpr (( '*' | '\u00D7' | 'div' | '\u00F7' | 'idiv' | 'mod') unionExpr)*
    ;

namedFunctionRef
    : eqName '#' IntegerLiteral
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

nameTestUnion
    : nameTest ('|' nameTest)*
    ;

nodeComp
    : 'is'
    | 'is-not'
    | nodePrecedes
    | nodeFollows
    | 'precedes-or-is'
    | 'follows-or-is'
    ;

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

otherwiseExpr
    : stringConcatExpr ('otherwise' stringConcatExpr)*
    ;

paramList
    : (varNameAndType (',' varNameAndType)*)?
    ;

parenthesizedExpr
    : '(' expr? ')'
    ;

pathExpr
    : absolutePathExpr
    | relativePathExpr
    ;

pipelineExpr
    : arrowExpr ('->' arrowExpr)*
    ;

positionalArgumentList
    : '(' positionalArguments? ')'
    ;

positionalArguments
    : argument (',' argument)*
    ;

positionalVar
    : 'at' '$' eqName
    ;

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

qNameLiteral
    : '#' eqName
    ;

quantifiedExpr
    : ('some' | 'every') quantifierBinding (',' quantifierBinding)* 'satisfies' exprSingle
    ;

quantifierBinding
    : varNameAndType 'in' exprSingle
    ;

rangeExpr
    : additiveExpr ('to' additiveExpr)?
    ;

recordPutExpr
    : instanceofExpr ('+:=' instanceofExpr)*
    ;

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

restrictedDynamicCall
    : (varRef | parenthesizedExpr | functionItemExpr | mapConstructor | arrayConstructor) positionalArgumentList
    ;

schemaAttributeNodeType
    : 'schema-attribute' '(' attributeName ')'
    ;

schemaElementNodeType
    : 'schema-element' '(' elementName ')'
    ;

selector
    : eqName
    | wildcard
    ;

sequenceArrowTarget
    : '=>' arrowTarget
    ;

sequenceType
    : 'empty-sequence' '(' ')'
    | itemType occurrenceIndicator?
    ;

simpleMapExpr
    : pathExpr ('!' pathExpr)*
    ;

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

typedFunctionType
    : ('function' | 'fn') '(' (typedFunctionParam (',' typedFunctionParam)*)? ')' 'as' sequenceType
    ;

typedMapType
    : 'map' '(' itemType ',' sequenceType ')'
    ;

typedRecordType
    : 'record' '(' (fieldDeclaration (',' fieldDeclaration)* (',' '...')?)? ')'
    ;

typeName_
    : eqName
    ;

simpleTypeName
    : typeName_
    ;

typeTest
    : gNodeType
    | xNodeType
    | jNodeType
    ;

unaryExpr
    : ('-' | '+')* valueExpr
    ;

unaryLookup
    : lookup
    ;

unbracedActions
    : 'then' exprSingle 'else' exprSingle
    ;

unionExpr
    : intersectExceptExpr (( 'union' | '|') intersectExceptExpr)*
    ;

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

varNameAndType
    : '$' eqName typeDeclaration?
    ;

varRef
    : '$' eqName
    ;

wildcard
    : '*'
    | QName ':*'
    | '*:' QName
    | BracedURILiteral '*'
    ;

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

xPath
    : (defaultElementNamespaceDecl ';')? (namespaceDecl ';')* expr EOF
    ;
