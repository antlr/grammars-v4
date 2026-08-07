// XQuery v4.0
// Author: Ken Domino
// Based on the XQuery 4.0 spec at https://qt4cg.org/pr/2796/xquery-40/xquery-40-autodiff.html
//
// XQuery 4.0 is a superset of XPath 4.0. This grammar incorporates all XPath 4.0
// expression rules and adds XQuery-specific module structure, prolog, FLWOR enhancements,
// direct constructors, switch/typeswitch, try/catch, validate, and extension expressions.

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar XQuery4Parser;

options {
    tokenVocab = XQuery4Lexer;
    superClass = XQuery4ParserBase;
}


/*
   "Semi-Official" entry is QueryList.
   See https://github.com/qt4cg/qtspecs/blob/060ec4f3a70b78326248be58691aca5e7b107e0d/specifications/grammar-40/xpath-grammar.xml#L34-L36

   NB: According to the Spec:
   "The QueryList production is not in the official grammar,
   and is not shown in the bnf.  It is here only for the purpose
   of testing a series of queries."
   https://github.com/qt4cg/qtspecs/blob/060ec4f3a70b78326248be58691aca5e7b107e0d/specifications/grammar-40/xpath-grammar.xml#L41-L43

   The entry point for XQuery 3.0 EBNF is "Module".
*/

queryList
    : module_ ('%%%' module_?)* EOF
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

allowingEmpty
    : 'allowing' 'empty'
    ;

andExpr
    : comparisonExpr ('and' comparisonExpr)*
    ;

annotation
    : '%' eqName ('(' constant (',' constant)+ ')')?
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
//        | 'namespace' // missing from XQuery4 Spec but in XPath4 Spec.
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

baseURIDecl
    : 'declare' 'base-uri' uriLiteral
    ;

boundarySpaceDecl
    : 'declare' 'boundary-space' ('preserve' | 'strip')
    ;

bracedAction
    : enclosedExpr
    ;

bracedSwitchCases
    : '{' switchCases '}'
    ;

bracedTypeswitchCases
    : '{' typeswitchCases '}'
    ;

caseClause
    : 'case' (varName 'as')? sequenceTypeUnion 'return' exprSingle
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

catchClause
    : 'catch' nameTestUnion enclosedExpr
    ;

cDataSection
    : CDataSection
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
    | unreservedName
    | OC expr CC
    ;

compNodeNCName
    : markedNCName
    | unreservedNCName
    | OC expr CC
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

constructionDecl
    : 'declare' 'construction' ('strip' | 'preserve')
    ;

contextValueDecl
    : 'declare' 'context' (('value' ('as' sequenceType)?) | ('item' ('as' itemType)?)) (
        (':=' varValue)
        | ('external' (':=' varDefaultValue)?)
    )
    ;

contextValueRef
    : '.'
    ;

copyNamespacesDecl
    : 'declare' 'copy-namespaces' preserveMode ',' inheritMode
    ;

countClause
    : 'count' varName
    ;

curlyArrayConstructor
    : 'array' enclosedExpr
    ;

currentVar
    : varName
    ;

decimalFormatDecl
    : 'declare' ('decimal-format' eqName | 'default' 'decimal-format') (
        dfPropertyName EQ StringLiteral
    )*
    ;

defaultCollationDecl
    : 'declare' 'default' 'collation' uriLiteral
    ;

defaultNamespaceDecl
    : 'declare' 'fixed'? 'default' ('element' | 'function') 'namespace' uriLiteral
    ;

dfPropertyName
    : (
        'decimal-separator'
        | 'grouping-separator'
        | 'infinity'
        | 'minus-sign'
        | 'NaN'
        | 'percent'
        | 'per-mille'
        | 'zero-digit'
        | 'digit'
        | 'pattern-separator'
        | 'exponent-separator'
    )
    ;

dirCommentConstructor
    : DirCommentContents
    ;

directConstructor
//    : //dirElemConstructor
//    | dirCommentConstructor
    : dirCommentConstructor
    | dirPIConstructor
    ;

dirPIConstructor
    : DirPIContents
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
    : 'element' '(' (nameTestUnion (',' typeName_ '?'?)?)? ')'
    ;

emptyOrderDecl
    : 'declare' 'default' 'order' 'empty' ('greatest' | 'least')
    ;

enclosedContentExpr
    : enclosedExpr
    ;

enclosedExpr
    : OC expr? CC
    ;

enumerationType
    : 'enum' '(' StringLiteral (',' StringLiteral)* ')'
    ;

eqName
    : QName
    | URIQualifiedName
    | 'after'
    | 'allowing'
    | 'ancestor'
    | 'ancestor-or-self'
    | 'and'
    | 'array'
    | 'as'
    | 'ascending'
    | 'at'
    | 'attribute'
    | 'base-uri'
    | 'before'
    | 'boundary-space'
    | 'by'
    | 'case'
    | 'cast'
    | 'castable'
    | 'catch'
    | 'child'
    | 'collation'
    | 'comment'
    | 'construction'
    | 'context'
    | 'copy-namespaces'
    | 'count'
    | 'decimal-format'
    | 'decimal-separator'
    | 'declare'
    | 'default'
    | 'descendant'
    | 'descendant-or-self'
    | 'descending'
    | 'digit'
    | 'div'
    | 'document-node'
    | 'document'
    | 'element'
    | 'else'
    | 'empty-sequence'
    | 'empty'
    | 'encoding'
    | 'end'
    | 'enum'
    | 'eq'
    | 'every'
    | 'except'
    | 'exponent-separator'
    | 'external'
    | 'finally'
    | 'first'
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
    | 'greatest'
    | 'group'
    | 'grouping-separator'
    | 'gt'
    | 'idiv'
    | 'if'
    | 'import'
    | 'in'
    | 'infinity'
    | 'inherit'
    | 'instance'
    | 'intersect'
    | 'is'
    | 'is-not'
    | 'item'
    | 'jnode'
    | 'key'
    | 'last'
    | 'lax'
    | 'le'
    | 'least'
    | 'let'
    | 'lt'
    | 'map'
    | 'member'
    | 'minus-sign'
    | 'mod'
    | 'module'
    | 'NaN'
    | 'namespace'
    | 'namespace-node'
    | 'ne'
    | 'next'
    | 'no-inherit'
    | 'no-preserve'
    | 'node'
    | 'of'
    | 'only'
    | 'option'
    | 'or'
    | 'order'
    | 'ordered'
    | 'ordering'
    | 'otherwise'
    | 'parent'
    | 'pattern-separator'
    | 'percent'
    | 'per-mille'
    | 'precedes'
    | 'precedes-or-is'
    | 'preceding'
    | 'preceding-or-self'
    | 'preceding-sibling'
    | 'preceding-sibling-or-self'
    | 'preserve'
    | 'previous'
    | 'processing-instruction'
    | 'record'
    | 'return'
    | 'satisfies'
    | 'schema'
    | 'schema-attribute'
    | 'schema-element'
    | 'self'
    | 'sliding'
    | 'some'
    | 'stable'
    | 'start'
    | 'strict'
    | 'strip'
    | 'switch'
    | 'text'
    | 'then'
    | 'to'
    | 'trace'
    | 'treat'
    | 'try'
    | 'tumbling'
    | 'type'
    | 'typeswitch'
    | 'union'
    | 'unordered'
    | 'validate'
    | 'value'
    | 'variable'
    | 'version'
    | 'when'
    | 'where'
    | 'while'
    | 'window'
    | 'xquery'
    | 'zero-digit'
    ;

expr
    : exprSingle (',' exprSingle)*
    ;

exprSingle
    : flworExpr
    | quantifiedExpr
    | switchExpr
    | typeswitchExpr
    | ifExpr
    | tryCatchExpr
// XQuery Update
    | insertExpr
    | deleteExpr
    | renameExpr
    | replaceExpr
    | transformExpr
//
    | orExpr
    ;

extendedFieldDeclaration
    : fieldDeclaration (':=' exprSingle)?
    ;

extensionExpr
    : Pragma+ '{' expr? '}'
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
    ;

//filterExpr
//    : postfixExpr predicate
//    ;

finallyClause
    : 'finally' enclosedExpr
    ;

flworExpr
    : initialClause intermediateClause* returnClause
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

forItemBinding
    : varNameAndType allowingEmpty? positionalVar? 'in' exprSingle
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

functionDecl
    : 'declare' annotation* 'function' eqName '(' paramListWithDefaults? ')' typeDeclaration? (
        functionBody
        | 'external'
    )
    ;

functionItemExpr
    : namedFunctionRef
    | inlineFunctionExpr
    ;

functionSignature
    : '(' paramList ')' typeDeclaration?
    ;

functionType
    : annotation* (anyFunctionType | typedFunctionType)
    ;

generalComp
    : EQ
    | '!='
    | LT
    | '<='
    | GT
    | '>='
    ;

gNodeType
    : 'gnode' '(' ')'
    ;

groupByClause
    : 'group' 'by' groupingSpec (',' groupingSpec)*
    ;

groupingSpec
    : varName (typeDeclaration? ':=' exprSingle)? ('collation' uriLiteral)?
    ;

ifExpr
    : 'if' '(' expr ')' (unbracedActions | bracedAction)
    ;

import_
    : schemaImport
    | moduleImport
    ;

inheritMode
    : 'inherit'
    | 'no-inherit'
    ;

initialClause
    : forClause
    | letClause
    | windowClause
    ;

inlineFunctionExpr
    : annotation* ('function' | 'fn') functionSignature? functionBody
    ;

instanceofExpr
    : treatExpr ('instance' 'of' sequenceType)?
    ;

intermediateClause
    : initialClause
    | whereClause
    | groupByClause
    | orderByClause
    | countClause
    | whileClause
    | traceClause
    ;

intersectExceptExpr
    : recordPutExpr (('intersect' | 'except') recordPutExpr)*
    ;

itemType
    : regularItemType
    | functionType
    | typeName_
    | choiceItemType
    ;

itemTypeDecl
    : 'declare' annotation* 'type' eqName 'as' itemType
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

letMapBinding
    : '$' OC varNameAndType (',' varNameAndType)* CC typeDeclaration? ':=' exprSingle
    ;

letSequenceBinding
    : '$' '(' varNameAndType (',' varNameAndType)* ')' typeDeclaration? ':=' exprSingle
    ;

letValueBinding
    : varNameAndType ':=' exprSingle
    ;

libraryModule
    : moduleDecl prolog
    ;

literal
    : numericLiteral
    | StringLiteral
    | qNameLiteral
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

mainModule
    : prolog queryBody
    ;

mapConstructor
    : 'map'? OC (mapConstructorEntry ( ',' mapConstructorEntry)*)? CC
    ;

mapConstructorEntry
    : exprSingle (COLON exprSingle)?
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

module_
    : versionDecl? (libraryModule | mainModule)
    ;

moduleDecl
    : 'module' 'namespace' NCName EQ uriLiteral ';'
    ;

moduleImport
    : 'import' 'module' ('namespace' NCName EQ)? uriLiteral ('at' uriLiteral ( ',' uriLiteral)*)?
    ;

multiplicativeExpr
    : unionExpr (('*' | '×' | 'div' | '÷' | 'idiv' | 'mod') unionExpr)*
    ;

namedFunctionRef
    : eqName '#' IntegerLiteral
    ;

namedRecordTypeDecl
    : 'declare' annotation* 'record' eqName '(' (extendedFieldDeclaration (',' extendedFieldDeclaration)*)? ')'
    ;

namespaceDecl
    : 'declare' 'namespace' QName EQ uriLiteral
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

nextVar
    : 'next' varName
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
    : directConstructor
    | computedConstructor
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
    | HexIntegerLiteral
    | BinaryIntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

occurrenceIndicator
    : '?'
    | '*'
    | '+'
    ;

optionDecl
    : 'declare' 'option' eqName StringLiteral
    ;

orderByClause
    : 'stable'? 'order' 'by' orderSpec (',' orderSpec)*
    ;

orderedExpr
    : 'ordered' enclosedExpr
    ;

orderingModeDecl
    : 'declare' 'ordering' ('ordered' | 'unordered')
    ;

orderModifier
    : ('ascending' | 'descending')? ('empty' ('greatest' | 'least'))? ('collation' uriLiteral)?
    ;

orderSpec
    : exprSingle orderModifier
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

paramListWithDefaults
    : paramWithDefault (',' paramWithDefault)*
    ;

paramWithDefault
    : varNameAndType (':=' exprSingle)?
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
    : 'at' varName
    ;

postfixExpr
    : primaryExpr (
        predicate
        | positionalArgumentList
        | lookup
        | '=?>' QName positionalArgumentList
    )*
//    | filterExpr
//    | dynamicFunctionCall
//    | lookupExpr
//    | methodCall
    ;

//pragma
//    : '(#' S eqName (S PragmaContents)? '#)'
//    ;

predicate
    : '[' expr ']'
    ;

preserveMode
    : 'preserve'
    | 'no-preserve'
    ;

previousVar
    : 'previous' varName
    ;

primaryExpr
    : arrayConstructor
    | literal
    | contextValueRef
    | functionCall
    | functionItemExpr
    | mapConstructor
    | nodeConstructor
    | orderedExpr
    | parenthesizedExpr
    | stringTemplate
    | StringConstructor
    | unaryLookup
    | unorderedExpr
    | varRef
    ;

processingInstructionNodeType
    : 'processing-instruction' '(' (QName | StringLiteral)? ')'
    ;

prolog
    : ((defaultNamespaceDecl | setter | namespaceDecl | import_) ';')* (
        (
            contextValueDecl
            | varDecl
            | functionDecl
            | itemTypeDecl
            | namedRecordTypeDecl
            | optionDecl
        ) ';'
    )*
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

queryBody
    : expr
    ;

quotStringLiteral
    : QuotAttrContentChar
    | EscapeQuot
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
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
    : stepExpr (('/' | '//') stepExpr)*
    ;

restrictedDynamicCall
    : (varRef | parenthesizedExpr | functionItemExpr | mapConstructor | arrayConstructor) positionalArgumentList
    ;

returnClause
    : 'return' exprSingle
    ;

schemaAttributeNodeType
    : 'schema-attribute' '(' attributeName ')'
    ;

schemaElementNodeType
    : 'schema-element' '(' elementName ')'
    ;

schemaImport
    : 'import' 'schema' schemaPrefix? uriLiteral ('at' uriLiteral ( ',' uriLiteral)*)?
    ;

schemaPrefix
    : 'namespace' NCName EQ
    | 'fixed'? 'default' 'element' 'namespace'
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

sequenceTypeUnion
    : sequenceType+
    ;

setter
    : boundarySpaceDecl
    | defaultCollationDecl
    | baseURIDecl
    | constructionDecl
    | orderingModeDecl
    | emptyOrderDecl
    | copyNamespacesDecl
    | decimalFormatDecl
    ;

simpleMapExpr
    : pathExpr ('!' pathExpr)*
    ;

simpleNodeTest
    : typeTest
    | selector
    ;

slidingWindowClause
    : 'sliding' 'window' varNameAndType 'in' exprSingle windowStartCondition? windowEndCondition
    ;

squareArrayConstructor
    : '[' (exprSingle (',' exprSingle)*)? ']'
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

switchCaseClause
    : ('case' switchCaseOperand)+ 'return' exprSingle
    ;

switchCaseOperand
    : expr
    ;

switchCases
    : switchCaseClause+ 'default' 'return' exprSingle
    ;

switchComparand
    : '(' expr? ')'
    ;

switchExpr
    : 'switch' switchComparand (switchCases | bracedSwitchCases)
    ;

textNodeType
    : 'text' '(' ')'
    ;

traceClause
    : 'trace' exprSingle
    ;

treatExpr
    : castableExpr ('treat' 'as' sequenceType)?
    ;

tryCatchExpr
    : tryClause ((catchClause+ finallyClause?) | finallyClause)
    ;

tryClause
    : 'try' enclosedExpr
    ;

tumblingWindowClause
    : 'tumbling' 'window' varNameAndType 'in' exprSingle windowStartCondition? windowEndCondition?
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
    : ('function' | 'fn') '(' typedFunctionParam* ')' 'as' sequenceType
    ;

typedMapType
    : 'map' '(' itemType ',' sequenceType ')'
    ;

typedRecordType
    : 'record' '(' (fieldDeclaration (',' fieldDeclaration)*)? ')'
    ;

typeName_
    : eqName
    ;

typeswitchCases
    : caseClause+ 'default' varName? 'return' exprSingle
    ;

typeswitchExpr
    : 'typeswitch' '(' expr ')' (typeswitchCases | bracedTypeswitchCases)
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
    : intersectExceptExpr (('union' | '|') intersectExceptExpr)*
    ;

unionNodeTest
    : '(' simpleNodeTest ('|' simpleNodeTest)* ')'
    ;

unorderedExpr
    : 'unordered' enclosedExpr
    ;

unreservedName
    : eqName
    ;

unreservedNCName
    : NCName
    ;

uriLiteral
    : StringLiteral
    ;

validateExpr
    : 'validate' (validationMode | ('type' typeName_))? '{' expr '}'
    ;

validationMode
    : 'lax'
    | 'strict'
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
    : validateExpr
    | extensionExpr
    | simpleMapExpr
    ;

varDecl
    : 'declare' annotation* 'variable' varNameAndType (
        ':=' varValue
        | 'external' (':=' varDefaultValue)?
    )
    ;

varDefaultValue
    : exprSingle
    ;

varName
    : '$' eqName
    ;

varNameAndType
    : '$' eqName typeDeclaration?
    ;

varRef
    : '$' eqName
    ;

varValue
    : exprSingle
    ;

versionDecl
    : 'xquery' (('encoding' StringLiteral) | ('version' StringLiteral ('encoding' StringLiteral)?)) ';'
    ;

whereClause
    : 'where' exprSingle
    ;

whileClause
    : 'while' exprSingle
    ;

wildcard
    : '*'
    ;

windowClause
    : 'for' (tumblingWindowClause | slidingWindowClause)
    ;

windowEndCondition
    : 'only'? 'end' windowVars ('when' exprSingle)?
    ;

windowStartCondition
    : 'start' windowVars ('when' exprSingle)?
    ;

windowVars
    : currentVar? positionalVar? previousVar? nextVar?
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



// XQuery Update Facility 1.0

insertExprTargetChoice
    : ('as' ('first' | 'last'))? 'into'
    | 'after'
    | 'before'
    ;

insertExpr
    : 'insert' ('node' | 'nodes') sourceExpr insertExprTargetChoice targetExpr
    ;

deleteExpr
    : 'delete' ('node' | 'nodes') targetExpr
    ;

replaceExpr
    : 'replace' ('value' 'of')? 'node' targetExpr 'with' exprSingle
    ;

renameExpr
    : 'rename' 'node' targetExpr 'as' newNameExpr
    ;

transformExpr
    : 'copy' '$' varName ':=' exprSingle (',' '$' varName ':=' exprSingle)* 'modify' exprSingle 'return' exprSingle
    ;

sourceExpr
    : exprSingle
    ;

targetExpr
    : exprSingle
    ;

newNameExpr
    : exprSingle
    ;
