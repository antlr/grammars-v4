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

abbreviatedStep
    :  '..'
    |  '@' nodeTest
    | simpleNodeTest
    ;

absolutePathExpr
    :  '/' relativePathExpr?
    |  '//' relativePathExpr
    ;

additiveExpr
    : multiplicativeExpr (( '+' |  '-') multiplicativeExpr)*
    ;

allowingEmpty
    : 'allowing' 'empty'
    ;

andExpr
    : comparisonExpr ( 'and' comparisonExpr)*
    ;

annotation
    : '%' EQName ('(' constant (',' constant)* ')')?
    ;

annotateddecl
    : annotation* (varDecl | contextValueDecl | functionDecl | itemTypeDecl | namedRecordTypeDecl)
    ;

// [27] Annotation: %EQName or %EQName(Literal, ...)
annotation
    :  '#' eQName ( '(' literal ( ',' literal)*  ')')?
    ;

anyArrayType
    :  'array'  '('  '*'  ')'
    ;

anyFunctionType
    : ( 'function' |  'fn')  '('  '*'  ')'
    ;

anyItemType
    :  'item'  '('  ')'
    ;

anyMapType
    :  'map'  '('  '*'  ')'
    ;

anyRecordType
    :  'record'  '('  '*'  ')'
    ;

anyXNodeType
    :  'node'  '('  ')'
    ;

aposStringLiteral
    : AposAttrContentChar
    | EscapeApos
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

argument
    : exprSingle
    | argumentPlaceholder
    ;

argumentList
    :  '(' ((positionalArguments ( ',' keywordArguments)?) | keywordArguments)?  ')'
    ;

argumentPlaceholder
    :  '?'
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
    : eQName
    ;

attributeNodeType
    :  'attribute'  '(' (nameTestUnion ( ',' typeName)?)?  ')'
    ;

// Entry point for Maven antlr4test-maven-plugin: semicolon-separated queries/modules
auxilary
    : (module_  ';'?)+ EOF
    ;

axis
    :  'ancestor'  '::'
    |  'ancestor-or-self'  '::'
    |  'attribute'  '::'
    |  'child'  '::'
    |  'descendant'  '::'
    |  'descendant-or-self'  '::'
    |  'following'  '::'
    |  'following-or-self'  '::'
    |  'following-sibling'  '::'
    |  'following-sibling-or-self'  '::'
    |  'namespace'  '::'
    |  'parent'  '::'
    |  'preceding'  '::'
    |  'preceding-or-self'  '::'
    |  'preceding-sibling'  '::'
    |  'preceding-sibling-or-self'  '::'
    |  'self'  '::'
    ;

axisStep
    : (abbreviatedStep | fullStep) (predicate | lookup)*
    ;

// [11] BaseURIDecl
baseURIDecl
    :  'declare'  'base-uri' uRILiteral
    ;

// [9] BoundarySpaceDecl
boundarySpaceDecl
    :  'declare'  'boundary-space' ( 'preserve' |  'strip')
    ;

bracedAction
    : enclosedExpr
    ;

// [71] CaseClause
caseClause
    :  'case' ( '$' eQName  'as')? sequenceTypeUnion  'return' exprSingle
    ;

castableExpr
    : castExpr ( 'castable'  'as' castTarget occurrenceIndicator?)?
    ;

castExpr
    : pipelineExpr ( 'cast'  'as' castTarget occurrenceIndicator?)?
    ;

castTarget
    : typeName
    | choiceItemType
    | enumerationType
    | typedArrayType
    | typedMapType
    | typedRecordType
    ;

// [75] CatchClause
catchClause
    :  'catch' catcherrlist enclosedExpr
    ;

catcherrlist
    : catcherror ( '|' catcherror)*
    ;

catcherror
    : eQName
    |  '*'
    ;

cDataSection
    : CDataSection
    ;

choiceItemType
    :  '(' itemType ( '|' itemType)*  ')'
    ;

commentNodeType
    :  'comment'  '('  ')'
    ;

compAttrConstructor
    :  'attribute' compNodeName enclosedExpr
    ;

compCommentConstructor
    :  'comment' enclosedExpr
    ;

compElemConstructor
    :  'element' compNodeName enclosedContentExpr
    ;

compNamespaceConstructor
    :  'namespace' compNodeNCName enclosedExpr
    ;

compPIConstructor
    :  'processing-instruction' compNodeNCName enclosedExpr
    ;

comparisonExpr
    : otherwiseExpr ((valueComp | generalComp | nodeComp) otherwiseExpr)?
    ;

compDocConstructor
    :  'document' enclosedExpr
    ;

compNodeName
    : qNameLiteral
    | OC expr CC
    ;

compNodeNCName
    : markedNCName
    | OC expr CC
    ;

compTextConstructor
    :  'text' enclosedExpr
    ;

// [83] ComputedConstructor (same as XPath 4.0)
computedConstructor
    : compDocConstructor
    | compElemConstructor
    | compAttrConstructor
    | compNamespaceConstructor
    | compTextConstructor
    | compCommentConstructor
    | compPIConstructor
    ;

constant
    : StringLiteral
    |  '-' numericLiteral
    | qNameLiteral
    | eQName  '('  ')'
    ;

// [12] ConstructionDecl
constructionDecl
    :  'declare'  'construction' ( 'strip' |  'preserve')
    ;

// [30] ContextItemDecl
contextValueDecl
    :  'declare'  'context'  'item' ( 'as' itemType)? (
        ( ':=' varDefaultValue)
        | ( 'external' ( ':=' varDefaultValue)?)
    )
    ;

contextValueRef
    : D
    ;

// [15] CopyNamespacesDecl
copyNamespacesDecl
    :  'declare'  'copy-namespaces' preserveMode  ',' inheritMode
    ;

// [58] CountClause
countClause
    :  'count'  '$' eQName
    ;

curlyArrayConstructor
    :  'array' enclosedExpr
    ;

currentVar
    :  '$' eQName
    ;

// [18] DecimalFormatDecl
decimalFormatDecl
    :  'declare' ( 'decimal-format' eQName |  'default'  'decimal-format') dFPropertyName*
    ;

// [10] DefaultCollationDecl
defaultCollationDecl
    :  'declare'  'default'  'collation' uRILiteral
    ;

// [20] DefaultNamespaceDecl
defaultNamespaceDecl
    :  'declare'  'default' ( 'element' |  'function')  'namespace' uRILiteral
    ;

// [19] DFPropertyName: eqname covers all property keywords (decimal-separator, etc.)
dFPropertyName
    : eQName EQ StringLiteral
    ;

dirAttributeList
    : dirAttributeValue*
    ;

dirAttributeValue
    : QName EQ dirattrvaluecontent
    ;

dirattrvaluecontent
    : ET_DQ_OPEN quotStringLiteral* AV_QUOT_CLOSE
    | ET_SQ_OPEN aposStringLiteral* AV_APOS_CLOSE
    ;

dirCommentConstructor
    : DirCommentContents
    ;

// DirElemContent: what appears between > and </
dirElemContent
    : dirElemConstructor
    | cDataSection
    | dirCommentConstructor
    | dirPIConstructor
    | ElementContentChar
    | PredefinedEntityRef
    | CharRef
    | LCurlyBraceEscape
    | RCurlyBraceEscape
    | OC expr CC
    ;

// [77] Direct Constructors
directConstructor
    : dirElemConstructor
    | dirCommentConstructor
    | dirPIConstructor
    ;

// DirElemConstructor: <Name attrs (/>  |  > content </Name>)
// Tokens: OPEN_TAG enters IN_ELEMENT_TAG mode; ET_SLASH_GT or ET_GT exit it.
dirElemConstructor
    : OPEN_TAG QName dirAttributeList (ET_SLASH_GT | ET_GT dirElemContent* EC_CLOSE_TAG QName CT_GT)
    ;

dirPIConstructor
    : DirPIContents
    ;

documentNodeType
    :  'document-node'  '(' (elementNodeType | schemaElementNodeType | nameTestUnion)?  ')'
    ;

dynamicNodeTest
    : enclosedExpr
    ;

elementName
    : eQName
    ;

elementNodeType
    :  'element'  '(' (nameTestUnion ( ',' typeName  '?'?)?)?  ')'
    ;

// [14] EmptyOrderDecl
emptyOrderDecl
    :  'declare'  'default'  'order'  'empty' ( 'greatest' |  'least')
    ;

enclosedContentExpr
    : enclosedExpr
    ;

enclosedExpr
    : OC expr? CC
    ;

enumerationType
    :  'enum'  '(' StringLiteral ( ',' StringLiteral)*  ')'
    ;

// ============================================================
// A.15 EQName -- keywords that are also valid names
// ============================================================

eQName
    : QName
    | URIQualifiedName
    |  'after'
    |  'allowing'
    |  'ancestor'
    |  'ancestor-or-self'
    |  'and'
    |  'array'
    |  'as'
    |  'ascending'
    |  'at'
    |  'attribute'
    |  'base-uri'
    |  'before'
    |  'boundary-space'
    |  'by'
    |  'case'
    |  'cast'
    |  'castable'
    |  'catch'
    |  'child'
    |  'collation'
    |  'comment'
    |  'construction'
    |  'context'
    |  'copy-namespaces'
    |  'count'
    |  'decimal-format'
    |  'decimal-separator'
    |  'declare'
    |  'default'
    |  'descendant'
    |  'descendant-or-self'
    |  'descending'
    |  'digit'
    |  'div'
    |  'document-node'
    |  'document'
    |  'element'
    |  'else'
    |  'empty-sequence'
    |  'empty'
    |  'encoding'
    |  'end'
    |  'enum'
    |  'eq'
    |  'every'
    |  'except'
    |  'exponent-separator'
    |  'external'
    |  'finally'
    |  'first'
    |  'fn'
    |  'following'
    |  'following-or-self'
    |  'following-sibling'
    |  'following-sibling-or-self'
    |  'follows'
    |  'follows-or-is'
    |  'for'
    |  'function'
    |  'ge'
    |  'gnode'
    |  'greatest'
    |  'group'
    |  'grouping-separator'
    |  'gt'
    |  'idiv'
    |  'if'
    |  'import'
    |  'in'
    |  'infinity'
    |  'inherit'
    |  'instance'
    |  'intersect'
    |  'is'
    |  'is-not'
    |  'item'
    |  'jnode'
    |  'key'
    |  'last'
    |  'lax'
    |  'le'
    |  'least'
    |  'let'
    |  'lt'
    |  'map'
    |  'member'
    |  'minus-sign'
    |  'mod'
    |  'module'
    |  'NaN'
    |  'namespace'
    |  'namespace-node'
    |  'ne'
    |  'next'
    |  'no-inherit'
    |  'no-preserve'
    |  'node'
    |  'of'
    |  'only'
    |  'option'
    |  'or'
    |  'order'
    |  'ordered'
    |  'ordering'
    |  'otherwise'
    |  'parent'
    |  'pattern-separator'
    |  'percent'
    |  'per-mille'
    |  'precedes'
    |  'precedes-or-is'
    |  'preceding'
    |  'preceding-or-self'
    |  'preceding-sibling'
    |  'preceding-sibling-or-self'
    |  'preserve'
    |  'previous'
    |  'processing-instruction'
    |  'record'
    |  'return'
    |  'satisfies'
    |  'schema'
    |  'schema-attribute'
    |  'schema-element'
    |  'self'
    |  'sliding'
    |  'some'
    |  'stable'
    |  'start'
    |  'strict'
    |  'strip'
    |  'switch'
    |  'text'
    |  'then'
    |  'to'
    |  'trace'
    |  'treat'
    |  'try'
    |  'tumbling'
    |  'type'
    |  'typeswitch'
    |  'union'
    |  'unordered'
    |  'validate'
    |  'value'
    |  'variable'
    |  'version'
    |  'when'
    |  'where'
    |  'while'
    |  'window'
    |  'xquery'
    |  'zero-digit'
    ;

// ============================================================
// A.4 Expressions
// ============================================================

// [39] Expr
expr
    : exprSingle ( ',' exprSingle)*
    ;

// [40] ExprSingle -- XQuery extends XPath with FLWOR, switch, typeswitch, try-catch
exprSingle
    : fLWORExpr
    | switchExpr
    | typeswitchExpr
    | tryCatchExpr
    | quantifiedExpr
    | ifExpr
    | orExpr
    ;

// "..." means extensible record (new in XQuery 4.0)
extendedFieldDeclaration
    :  '..'
    ;

// [ExtensionExpr]
extensionExpr
    : Pragma+ enclosedExpr
    ;

fieldDeclaration
    : fieldName  '?'? ( 'as' sequenceType)?
    ;

fielddeclarationlist
    : fieldDeclaration ( ',' fieldDeclaration)* ( ',' extendedFieldDeclaration)?
    | extendedFieldDeclaration
    ;

fieldName
    : QName
    | StringLiteral
    ;

// [76] FinallyClause (new in XQuery 4.0)
finallyClause
    :  'finally' enclosedExpr
    ;

// ============================================================
// A.5 FLWOR Expressions
// ============================================================

// [41] FLWORExpr
fLWORExpr
    : initialClause intermediateClause* returnClause
    ;

// [46] ForBinding
forBinding
    : forItemBinding
    | forMemberBinding
    | forEntryBinding
    ;

// [45] ForClause (multiple bindings per clause)
forClause
    :  'for' forBinding ( ',' forBinding)*
    ;

// [49] ForEntryBinding (XPath/XQuery 4.0: iterates over map entries)
forEntryBinding
    : (forEntryKeyBinding forEntryValueBinding | forEntryValueBinding) positionalVar?  'in' exprSingle
    ;

forEntryKeyBinding
    :  'key' varNameAndType
    ;

forEntryValueBinding
    :  'value' varNameAndType
    ;

// [47] ForItemBinding (AllowingEmpty is XQuery 3.0+)
forItemBinding
    : varNameAndType ( 'allowing'  'empty')? positionalVar?  'in' exprSingle
    ;

// [48] ForMemberBinding (XPath/XQuery 4.0: iterates over array members)
forMemberBinding
    :  'member' varNameAndType positionalVar?  'in' exprSingle
    ;

fullStep
    : axis nodeTest
    ;

functionBody
    : enclosedExpr
    ;

functionCall
    : { this.IsFuncCall() }? eQName argumentList
    ;

// [31] FunctionDecl
functionDecl
    :  'declare'  'function' eQName functionSignature (functionBody |  'external')
    ;

// ============================================================
// A.11 Function Item Expressions
// ============================================================

functionItemExpr
    : namedFunctionRef
    | inlineFunctionExpr
    ;

// [32] FunctionSignature
functionSignature
    :  '(' paramListWithDefaults?  ')' typeDeclaration?
    ;

functionType
    : anyFunctionType
    | typedFunctionType
    ;

generalComp
    : EQ
    |  '!='
    | LT
    |  '<='
    | GT
    |  '>='
    ;

gNodeType
    :  'gnode'  '('  ')'
    ;

// [60] GroupByClause
groupByClause
    :  'group'  'by' groupingSpec ( ',' groupingSpec)*
    ;

// [61] GroupingSpec
groupingSpec
    : (varNameAndType ( ':=' exprSingle)? | exprSingle) ( 'collation' uRILiteral)?
    ;

ifExpr
    :  'if'  '(' expr  ')' (unbracedActions | bracedAction)
    ;

// [22] Import
import_
    : schemaImport
    | moduleImport
    ;

// [17] InheritMode
inheritMode
    :  'inherit'
    |  'no-inherit'
    ;

// [42] InitialClause
initialClause
    : forClause
    | letClause
    | windowClause
    ;

inlineFunctionExpr
    : ( 'function' |  'fn') functionSignature functionBody
    ;

instanceofExpr
    : treatExpr ( 'instance'  'of' sequenceType)?
    ;

// [43] IntermediateClause
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
    : recordPutExpr (( 'intersect' |  'except') recordPutExpr)*
    ;

itemType
    : regularItemType
    | functionType
    | typeName
    | choiceItemType
    ;

// [35] ItemTypeDecl (new in XQuery 4.0: type aliases)
itemTypeDecl
    :  'declare'  'type' eQName EQ itemType
    ;

jNodeType
    :  'jnode'  '(' ( '*' | jRootSelector | QName | constant) ( ',' sequenceType)?  ')'
    ;

jRootSelector
    :  '('  ')'
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
    : eQName  ':=' argument
    ;

keywordArguments
    : keywordArgument ( ',' keywordArgument)*
    ;

letArrayBinding
    :  '$'  '[' varNameAndType  ']' typeDeclaration?  ':=' exprSingle
    ;

// [52] LetBinding variants (XPath/XQuery 4.0 destructuring)
letBinding
    : letValueBinding
    | letSequenceBinding
    | letArrayBinding
    | letMapBinding
    ;

// [51] LetClause (multiple bindings per clause)
letClause
    :  'let' letBinding ( ',' letBinding)*
    ;

letMapBinding
    :  '$' OC varNameAndType CC typeDeclaration?  ':=' exprSingle
    ;

letSequenceBinding
    :  '$'  '(' varNameAndType  ')' typeDeclaration?  ':=' exprSingle
    ;

letValueBinding
    : varNameAndType  ':=' exprSingle
    ;

// [4] LibraryModule
libraryModule
    : moduleDecl prolog
    ;

literal
    : numericLiteral
    | StringLiteral
    ;

lookup
    :  '?' keySpecifier
    ;

lookupWildcard
    :  '*'
    ;

// [3] MainModule
mainModule
    : prolog queryBody
    ;

// ============================================================
// A.13 Map / Array Constructors
// ============================================================

mapConstructor
    :  'map' OC (mapConstructorEntry ( ',' mapConstructorEntry)*)? CC
    ;

mapConstructorEntry
    : exprSingle COLON exprSingle
    ;

mappingArrowTarget
    :  '=!>' arrowTarget
    ;

mapType
    : anyMapType
    | typedMapType
    ;

markedNCName
    :  '#' QName
    ;

// A single XQuery module (library or main)
module_
    : versionDecl? (libraryModule | mainModule)
    ;

// [5] ModuleDecl
moduleDecl
    :  'module'  'namespace' NCName EQ uRILiteral  ';'
    ;

// [25] ModuleImport
moduleImport
    :  'import'  'module' ( 'namespace' NCName EQ)? uRILiteral (
         'at' uRILiteral ( ',' uRILiteral)*
    )?
    ;

multiplicativeExpr
    : unionExpr (( '*' |  '\u00D7' |  'div' |  '\u00F7' |  'idiv' |  'mod') unionExpr)*
    ;

namedFunctionRef
    : eQName  '#' IntegerLiteral
    ;

// [36] NamedRecordTypeDecl (new in XQuery 4.0)
namedRecordTypeDecl
    :  'declare'  'record' eQName EQ typedRecordType
    ;

// [21] NamespaceDecl
namespaceDecl
    :  'declare'  'namespace' NCName EQ uRILiteral
    ;

namespaceNodeType
    :  'namespace-node'  '('  ')'
    ;

nameTest
    : eQName
    | wildcard
    ;

nameTestUnion
    : nameTest
    ;

nextVar
    : eQName
    ;

// ============================================================
// A.12 Node Constructors
// ============================================================

nodeConstructor
    : directConstructor
    | computedConstructor
    ;

nodeComp
    :  'is'
    |  'is-not'
    | nodePrecedes
    | nodeFollows
    |  'precedes-or-is'
    |  'follows-or-is'
    ;

nodeFollows
    :  '>>'
    |  'follows'
    ;

nodePrecedes
    :  '<<'
    |  'precedes'
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
    :  '?'
    |  '*'
    |  '+'
    ;

// [37] OptionDecl
optionDecl
    :  'declare'  'option' eQName StringLiteral
    ;

// [62] OrderByClause
orderByClause
    : ( 'order'  'by' |  'stable'  'order'  'by') orderSpec ( ',' orderSpec)*
    ;

orderedExpr
    :  'ordered' enclosedExpr
    ;

// [13] OrderingModeDecl
orderingModeDecl
    :  'declare'  'ordering' ( 'ordered' |  'unordered')
    ;

// [64] OrderModifier
orderModifier
    : ( 'ascending' |  'descending')? ( 'empty' ( 'greatest' |  'least'))? (
         'collation' uRILiteral
    )?
    ;

// [63] OrderSpec
orderSpec
    : exprSingle orderModifier
    ;

orExpr
    : andExpr ( 'or' andExpr)*
    ;

otherwiseExpr
    : stringConcatExpr ( 'otherwise' stringConcatExpr)*
    ;

// ============================================================
// Shared helper rules
// ============================================================

paramList
    : varNameAndType ( ',' varNameAndType)*
    ;

// [33] ParamListWithDefaults (XQuery 4.0 allows default parameter values)
paramListWithDefaults
    : paramWithDefault ( ',' paramWithDefault)*
    ;

// [34] ParamWithDefault
paramWithDefault
    :  '$' eQName typeDeclaration? ( ':=' exprSingle)?
    ;

parenthesizedExpr
    :  '(' expr?  ')'
    ;

// ============================================================
// A.9 Path Expressions
// ============================================================

pathExpr
    : absolutePathExpr
    | relativePathExpr
    ;

pipelineExpr
    : arrowExpr
    ;

positionalArgumentList
    :  '(' positionalArguments?  ')'
    ;

positionalArguments
    : argument ( ',' argument)*
    ;

// [50] PositionalVar
positionalVar
    :  'at'  '$' eQName
    ;

positionalvarname
    : eQName
    ;

// ============================================================
// A.10 Postfix / Primary Expressions
// ============================================================

postfixExpr
    : primaryExpr (
        predicate
        | positionalArgumentList
        | lookup
        | ( '=?>' QName positionalArgumentList)
    )*
    ;

predicate
    :  '[' expr  ']'
    ;

predicatelist
    : predicate*
    ;

// [16] PreserveMode
preserveMode
    :  'preserve'
    |  'no-preserve'
    ;

previousVar
    : eQName
    ;

// PrimaryExpr: XQuery adds directconstructor, orderedexpr, unorderedexpr
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
    | orderedExpr
    | unorderedExpr
    ;

processingInstructionNodeType
    :  'processing-instruction'  '(' (QName | StringLiteral)?  ')'
    ;

// ============================================================
// A.2 Prolog
// ============================================================

// [6] Prolog: two phases -- setters/imports first, then annotated decls
prolog
    : (setter  ';' | defaultNamespaceDecl  ';' | namespaceDecl  ';' | import_  ';')* (
        annotateddecl  ';'
        | optionDecl  ';'
    )*
    ;

qNameLiteral
    :  '#' eQName
    ;

// ============================================================
// A.8 Expression Operators (precedence order, lowest to highest)
// ============================================================

quantifiedExpr
    : ( 'some' |  'every') quantifierBinding ( ',' quantifierBinding)*  'satisfies' exprSingle
    ;

quantifierBinding
    : varNameAndType  'in' exprSingle
    ;

// ============================================================
// A.3 Query Body
// ============================================================

queryBody
    : expr
    ;

// A file may contain multiple whitespace/semicolon-separated modules
queryList
    : module_ ( ';'* module_)*  ';'* EOF
    ;

quotStringLiteral
    : QuotAttrContentChar
    | EscapeQuot
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

rangeExpr
    : additiveExpr ( 'to' additiveExpr)?
    ;

// RecordPutExpr: new in XPath/XQuery 4.0
recordPutExpr
    : instanceofExpr ( '+:=' instanceofExpr)*
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
    : stepExpr (( '/' |  '//') stepExpr)*
    ;

restrictedDynamicCall
    : (varRef | parenthesizedExpr | functionItemExpr | mapConstructor | arrayConstructor) positionalArgumentList
    ;

// [44] ReturnClause
returnClause
    :  'return' exprSingle
    ;

schemaAttributeNodeType
    :  'schema-attribute'  '(' attributeName  ')'
    ;

schemaElementNodeType
    :  'schema-element'  '(' elementName  ')'
    ;

// [23] SchemaImport
schemaImport
    :  'import'  'schema' schemaPrefix? uRILiteral ( 'at' uRILiteral ( ',' uRILiteral)*)?
    ;

// [24] SchemaPrefix
schemaPrefix
    :  'namespace' NCName EQ
    |  'default'  'element'  'namespace'
    ;

selector
    : eQName
    | wildcard
    ;

sequenceArrowTarget
    :  '=>' arrowTarget
    ;

sequenceType
    :  'empty-sequence'  '('  ')'
    | itemType occurrenceIndicator?
    ;

// [72] SequenceTypeUnion
sequenceTypeUnion
    : sequenceType ( '|' sequenceType)*
    ;

// [8] Setter
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
    : pathExpr ( '!' pathExpr)*
    ;

simpleNodeTest
    : typeTest
    | selector
    ;

simpletypename
    : typeName
    ;

squareArrayConstructor
    :  '[' (exprSingle ( ',' exprSingle)*)?  ']'
    ;

stepExpr
    : postfixExpr
    | axisStep
    ;

stringConcatExpr
    : rangeExpr ( '||' rangeExpr)*
    ;

stringTemplate
    : StringTemplate
    ;

// [67] SwitchCaseClause
switchCaseClause
    : ( 'case' switchCaseOperand)+  'return' exprSingle
    ;

// [68] SwitchCaseOperand
switchCaseOperand
    : exprSingle
    ;

// ============================================================
// A.6 Switch / Typeswitch expressions
// ============================================================

// [65] SwitchExpr
switchExpr
    :  'switch'  '(' expr  ')' switchCaseClause+  'default'  'return' exprSingle
    ;

textNodeType
    :  'text'  '('  ')'
    ;

// TraceClause (new in XQuery 4.0): trace($label, $expr) or trace($expr)
traceClause
    :  'trace'  '(' exprSingle ( ',' exprSingle)?  ')'
    ;

treatExpr
    : castableExpr ( 'treat'  'as' sequenceType)?
    ;

// ============================================================
// A.7 Try-Catch
// ============================================================

// [73] TryCatchExpr
tryCatchExpr
    : tryClause catchClause+ finallyClause?
    ;

// [74] TryClause
tryClause
    :  'try' enclosedExpr
    ;

typedArrayType
    :  'array'  '(' sequenceType  ')'
    ;

// ============================================================
// A.14 Type Declarations and Sequence Types
// ============================================================

typeDeclaration
    :  'as' sequenceType
    ;

typedFunctionParam
    : ( '$' eQName  'as')? sequenceType
    ;

typedfunctionparamlist
    : typedFunctionParam ( ',' typedFunctionParam)*
    ;

typedFunctionType
    : ( 'function' |  'fn')  '(' typedfunctionparamlist?  ')'  'as' sequenceType
    ;

typedMapType
    :  'map'  '(' itemType  ',' sequenceType  ')'
    ;

typedRecordType
    :  'record'  '(' fielddeclarationlist  ')'
    ;

typeName
    : eQName
    ;

// [69] TypeswitchExpr
typeswitchExpr
    :  'typeswitch'  '(' expr  ')' caseClause+  'default' ( '$' eQName)?  'return' exprSingle
    ;

typeTest
    : gNodeType
    | xNodeType
    | jNodeType
    ;

unaryExpr
    : ( '-' |  '+')* valueExpr
    ;

unaryLookup
    :  '?' keySpecifier
    ;

unbracedActions
    :  'then' exprSingle  'else' exprSingle
    ;

unionExpr
    : intersectExceptExpr (( 'union' |  '|') intersectExceptExpr)*
    ;

unionNodeTest
    :  '(' simpleNodeTest ( '|' simpleNodeTest)+  ')'
    ;

unorderedExpr
    :  'unordered' enclosedExpr
    ;

uRILiteral
    : StringLiteral
    ;

// [ValidateExpr]
validateExpr
    :  'validate' validationMode? enclosedExpr
    ;

validationMode
    :  'lax'
    |  'strict'
    |  'type' typeName
    ;

valueComp
    :  'eq'
    |  'ne'
    |  'lt'
    |  'le'
    |  'gt'
    |  'ge'
    ;

// [ValueExpr] XQuery extends XPath with ValidateExpr and ExtensionExpr
valueExpr
    : validateExpr
    | extensionExpr
    | simpleMapExpr
    ;

// [28] VarDecl
varDecl
    :  'declare'  'variable'  '$' eQName typeDeclaration? (
        ( ':=' varDefaultValue)
        | ( 'external' ( ':=' varDefaultValue)?)
    )
    ;

// [29] VarDefaultValue
varDefaultValue
    : exprSingle
    ;

varNameAndType
    :  '$' eQName typeDeclaration?
    ;

varRef
    :  '$' eQName
    ;

// [2] VersionDecl
versionDecl
    :  'xquery' (
        ( 'encoding' StringLiteral)
        | ( 'version' StringLiteral ( 'encoding' StringLiteral)?)
    )  ';'
    ;

// [59] WhereClause
whereClause
    :  'where' exprSingle
    ;

// WhileClause (new in XQuery 4.0)
whileClause
    :  'while'  '(' exprSingle  ')'
    ;

wildcard
    :  '*'
    | QName  ':*'
    |  '*:' QName
    | BracedURILiteral  '*'
    ;

// [53] WindowClause
windowClause
    :  'for' ( 'tumbling' |  'sliding')  'window' varNameAndType  'in' exprSingle windowStartCondition windowEndCondition?
    ;

// [56] WindowEndCondition
windowEndCondition
    :  'only'?  'end' windowVars  'when' exprSingle
    ;

// [55] WindowStartCondition
windowStartCondition
    :  'start' windowVars  'when' exprSingle
    ;

// [57] WindowVars
windowVars
    : currentVar? ( 'at'  '$' positionalvarname)? ( 'previous'  '$' previousVar)? (
         'next'  '$' nextVar
    )?
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