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

abbreviatedstep
    :  '..'
    |  '@' nodetest
    | simplenodetest
    ;

absolutepathexpr
    :  '/' relativepathexpr?
    |  '//' relativepathexpr
    ;

additiveexpr
    : multiplicativeexpr (( '+' |  '-') multiplicativeexpr)*
    ;

andexpr
    : comparisonexpr ( 'and' comparisonexpr)*
    ;

// [26] AnnotatedDecl
annotateddecl
    : annotation* (vardecl | contextitemdecl | functiondecl | itemtypedecl | namedrecordtypedecl)
    ;

// [27] Annotation: %EQName or %EQName(Literal, ...)
annotation
    :  '#' eqname ( '(' literal ( ',' literal)*  ')')?
    ;

anyarraytype
    :  'array'  '('  '*'  ')'
    ;

anyfunctiontype
    : ( 'function' |  'fn')  '('  '*'  ')'
    ;

anyitemtype
    :  'item'  '('  ')'
    ;

anymaptype
    :  'map'  '('  '*'  ')'
    ;

anyrecordtype
    :  'record'  '('  '*'  ')'
    ;

anyxnodetype
    :  'node'  '('  ')'
    ;

aposattrcontentchar
    : AposAttrContentChar
    | EscapeApos
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

argument
    : exprsingle
    | argumentplaceholder
    ;

argumentlist
    :  '(' ((positionalarguments ( ',' keywordarguments)?) | keywordarguments)?  ')'
    ;

argumentplaceholder
    :  '?'
    ;

arrayconstructor
    : squarearrayconstructor
    | curlyarrayconstructor
    ;

arraytype
    : anyarraytype
    | typedarraytype
    ;

arrowexpr
    : unaryexpr (sequencearrowtarget | mappingarrowtarget)*
    ;

arrowtarget
    : functioncall
    | restricteddynamiccall
    ;

attributename
    : eqname
    ;

attributenodetype
    :  'attribute'  '(' (nametestunion ( ',' typename_)?)?  ')'
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

axisstep
    : (abbreviatedstep | fullstep) (predicate | lookup)*
    ;

// [11] BaseURIDecl
baseuridecl
    :  'declare'  'base-uri' uriliteral
    ;

// [9] BoundarySpaceDecl
boundaryspacedecl
    :  'declare'  'boundary-space' ( 'preserve' |  'strip')
    ;

bracedaction
    : enclosedexpr
    ;

// [71] CaseClause
caseclause
    :  'case' ( '$' eqname  'as')? sequencetypeunion  'return' exprsingle
    ;

castableexpr
    : castexpr ( 'castable'  'as' casttarget occurrenceindicator?)?
    ;

castexpr
    : pipelineexpr ( 'cast'  'as' casttarget occurrenceindicator?)?
    ;

casttarget
    : typename_
    | choiceitemtype
    | enumerationtype
    | typedarraytype
    | typedmaptype
    | typedrecordtype
    ;

// [75] CatchClause
catchclause
    :  'catch' catcherrlist enclosedexpr
    ;

catcherrlist
    : catcherror ( '|' catcherror)*
    ;

catcherror
    : eqname
    |  '*'
    ;

cdsection
    : CDataSection
    ;

choiceitemtype
    :  '(' itemtype ( '|' itemtype)*  ')'
    ;

commentnodetype
    :  'comment'  '('  ')'
    ;

compAttrconstructor
    :  'attribute' compnodename enclosedexpr
    ;

compCommentconstructor
    :  'comment' enclosedexpr
    ;

compElemconstructor
    :  'element' compnodename enclosedcontentexpr
    ;

compNSconstructor
    :  'namespace' compnodencname enclosedexpr
    ;

compPIconstructor
    :  'processing-instruction' compnodencname enclosedexpr
    ;

comparisonexpr
    : otherwiseexpr ((valuecomp | generalcomp | nodecomp) otherwiseexpr)?
    ;

compdocconstructor
    :  'document' enclosedexpr
    ;

compnodename
    : qnameliteral
    | OC expr CC
    ;

compnodencname
    : markedncname
    | OC expr CC
    ;

comptextconstructor
    :  'text' enclosedexpr
    ;

// [83] ComputedConstructor (same as XPath 4.0)
computedconstructor
    : compdocconstructor
    | compElemconstructor
    | compAttrconstructor
    | compNSconstructor
    | comptextconstructor
    | compCommentconstructor
    | compPIconstructor
    ;

constant
    : StringLiteral
    |  '-' numericliteral
    | qnameliteral
    | eqname  '('  ')'
    ;

// [12] ConstructionDecl
constructiondecl
    :  'declare'  'construction' ( 'strip' |  'preserve')
    ;

// [30] ContextItemDecl
contextitemdecl
    :  'declare'  'context'  'item' ( 'as' itemtype)? (
        ( ':=' vardefaultvalue)
        | ( 'external' ( ':=' vardefaultvalue)?)
    )
    ;

contextvalueref
    : D
    ;

// [15] CopyNamespacesDecl
copynamespaces_decl
    :  'declare'  'copy-namespaces' preservemode  ',' inheritmode
    ;

// [58] CountClause
countclause
    :  'count'  '$' eqname
    ;

curlyarrayconstructor
    :  'array' enclosedexpr
    ;

currentvar
    :  '$' eqname
    ;

// [18] DecimalFormatDecl
decimaldecl
    :  'declare' ( 'decimal-format' eqname |  'default'  'decimal-format') dfpropertyname*
    ;

// [10] DefaultCollationDecl
defaultcollationdecl
    :  'declare'  'default'  'collation' uriliteral
    ;

// [20] DefaultNamespaceDecl
defaultnamespacedecl
    :  'declare'  'default' ( 'element' |  'function')  'namespace' uriliteral
    ;

// [19] DFPropertyName: eqname covers all property keywords (decimal-separator, etc.)
dfpropertyname
    : eqname EQ StringLiteral
    ;

dirattrlist
    : dirattrvalue*
    ;

dirattrvalue
    : QName EQ dirattrvaluecontent
    ;

dirattrvaluecontent
    : ET_DQ_OPEN quotattrcontentchar* AV_QUOT_CLOSE
    | ET_SQ_OPEN aposattrcontentchar* AV_APOS_CLOSE
    ;

dircommentconstructor
    : DirCommentContents
    ;

// DirElemContent: what appears between > and </
dircontent
    : direlemconstructor
    | cdsection
    | dircommentconstructor
    | dirpiconstructor
    | ElementContentChar
    | PredefinedEntityRef
    | CharRef
    | LCurlyBraceEscape
    | RCurlyBraceEscape
    | OC expr CC
    ;

// [77] Direct Constructors
directconstructor
    : direlemconstructor
    | dircommentconstructor
    | dirpiconstructor
    ;

// DirElemConstructor: <Name attrs (/>  |  > content </Name>)
// Tokens: OPEN_TAG enters IN_ELEMENT_TAG mode; ET_SLASH_GT or ET_GT exit it.
direlemconstructor
    : OPEN_TAG QName dirattrlist (ET_SLASH_GT | ET_GT dircontent* EC_CLOSE_TAG QName CT_GT)
    ;

dirpiconstructor
    : DirPIContents
    ;

documentnodetype
    :  'document-node'  '(' (elementnodetype | schemaelementnodetype | nametestunion)?  ')'
    ;

dynamicnodetest
    : enclosedexpr
    ;

elementname
    : eqname
    ;

elementnodetype
    :  'element'  '(' (nametestunion ( ',' typename_  '?'?)?)?  ')'
    ;

// [14] EmptyOrderDecl
emptyorderdecl
    :  'declare'  'default'  'order'  'empty' ( 'greatest' |  'least')
    ;

enclosedcontentexpr
    : enclosedexpr
    ;

enclosedexpr
    : OC expr? CC
    ;

enumerationtype
    :  'enum'  '(' StringLiteral ( ',' StringLiteral)*  ')'
    ;

// ============================================================
// A.15 EQName -- keywords that are also valid names
// ============================================================

eqname
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
    : exprsingle ( ',' exprsingle)*
    ;

// [40] ExprSingle -- XQuery extends XPath with FLWOR, switch, typeswitch, try-catch
exprsingle
    : flworexpr
    | switchexpr
    | typeswitchexpr
    | trycatchexpr
    | quantifiedexpr
    | ifexpr
    | orexpr
    ;

// "..." means extensible record (new in XQuery 4.0)
extendedfielddeclaration
    :  '..'
    ;

// [ExtensionExpr]
extensionexpr
    : Pragma+ enclosedexpr
    ;

fielddeclaration
    : fieldname  '?'? ( 'as' sequencetype)?
    ;

fielddeclarationlist
    : fielddeclaration ( ',' fielddeclaration)* ( ',' extendedfielddeclaration)?
    | extendedfielddeclaration
    ;

fieldname
    : QName
    | StringLiteral
    ;

// [76] FinallyClause (new in XQuery 4.0)
finallyclause
    :  'finally' enclosedexpr
    ;

// ============================================================
// A.5 FLWOR Expressions
// ============================================================

// [41] FLWORExpr
flworexpr
    : initialclause intermediateclause* returnclause
    ;

// [46] ForBinding
forbinding
    : foritembinding
    | formemberbinding
    | forentrybinding
    ;

// [45] ForClause (multiple bindings per clause)
forclause
    :  'for' forbinding ( ',' forbinding)*
    ;

// [49] ForEntryBinding (XPath/XQuery 4.0: iterates over map entries)
forentrybinding
    : (forentrykeybinding forentryvaluebinding | forentryvaluebinding) positionalvar?  'in' exprsingle
    ;

forentrykeybinding
    :  'key' varnameandtype
    ;

forentryvaluebinding
    :  'value' varnameandtype
    ;

// [47] ForItemBinding (AllowingEmpty is XQuery 3.0+)
foritembinding
    : varnameandtype ( 'allowing'  'empty')? positionalvar?  'in' exprsingle
    ;

// [48] ForMemberBinding (XPath/XQuery 4.0: iterates over array members)
formemberbinding
    :  'member' varnameandtype positionalvar?  'in' exprsingle
    ;

fullstep
    : axis nodetest
    ;

functionbody
    : enclosedexpr
    ;

functioncall
    : { this.IsFuncCall() }? eqname argumentlist
    ;

// [31] FunctionDecl
functiondecl
    :  'declare'  'function' eqname functionsignature (functionbody |  'external')
    ;

// ============================================================
// A.11 Function Item Expressions
// ============================================================

functionitemexpr
    : namedfunctionref
    | inlinefunctionexpr
    ;

// [32] FunctionSignature
functionsignature
    :  '(' paramlistwithdefaults?  ')' typedeclaration?
    ;

functiontype
    : anyfunctiontype
    | typedfunctiontype
    ;

generalcomp
    : EQ
    |  '!='
    | LT
    |  '<='
    | GT
    |  '>='
    ;

gnodetype
    :  'gnode'  '('  ')'
    ;

// [60] GroupByClause
groupbyclause
    :  'group'  'by' groupingspec ( ',' groupingspec)*
    ;

// [61] GroupingSpec
groupingspec
    : (varnameandtype ( ':=' exprsingle)? | exprsingle) ( 'collation' uriliteral)?
    ;

ifexpr
    :  'if'  '(' expr  ')' (unbracedactions | bracedaction)
    ;

// [22] Import
import_
    : schemaimport
    | moduleimport
    ;

// [17] InheritMode
inheritmode
    :  'inherit'
    |  'no-inherit'
    ;

// [42] InitialClause
initialclause
    : forclause
    | letclause
    | windowclause
    ;

inlinefunctionexpr
    : ( 'function' |  'fn') functionsignature functionbody
    ;

instanceofexpr
    : treatexpr ( 'instance'  'of' sequencetype)?
    ;

// [43] IntermediateClause
intermediateclause
    : initialclause
    | whereclause
    | groupbyclause
    | orderbyclause
    | countclause
    | whileclause
    | traceclause
    ;

intersectexceptexpr
    : recordputexpr (( 'intersect' |  'except') recordputexpr)*
    ;

itemtype
    : regularitemtype
    | functiontype
    | typename_
    | choiceitemtype
    ;

// [35] ItemTypeDecl (new in XQuery 4.0: type aliases)
itemtypedecl
    :  'declare'  'type' eqname EQ itemtype
    ;

jnodetype
    :  'jnode'  '(' ( '*' | jrootselector | QName | constant) ( ',' sequencetype)?  ')'
    ;

jrootselector
    :  '('  ')'
    ;

keyspecifier
    : QName
    | literal
    | contextvalueref
    | varref
    | parenthesizedexpr
    | lookupwildcard
    ;

keywordargument
    : eqname  ':=' argument
    ;

keywordarguments
    : keywordargument ( ',' keywordargument)*
    ;

letarraybinding
    :  '$'  '[' varnameandtype  ']' typedeclaration?  ':=' exprsingle
    ;

// [52] LetBinding variants (XPath/XQuery 4.0 destructuring)
letbinding
    : letvaluebinding
    | letsequencebinding
    | letarraybinding
    | letmapbinding
    ;

// [51] LetClause (multiple bindings per clause)
letclause
    :  'let' letbinding ( ',' letbinding)*
    ;

letmapbinding
    :  '$' OC varnameandtype CC typedeclaration?  ':=' exprsingle
    ;

letsequencebinding
    :  '$'  '(' varnameandtype  ')' typedeclaration?  ':=' exprsingle
    ;

letvaluebinding
    : varnameandtype  ':=' exprsingle
    ;

// [4] LibraryModule
librarymodule
    : moduledecl prolog
    ;

literal
    : numericliteral
    | StringLiteral
    ;

lookup
    :  '?' keyspecifier
    ;

lookupwildcard
    :  '*'
    ;

// [3] MainModule
mainmodule
    : prolog querybody
    ;

// ============================================================
// A.13 Map / Array Constructors
// ============================================================

mapconstructor
    :  'map' OC (mapconstructorentry ( ',' mapconstructorentry)*)? CC
    ;

mapconstructorentry
    : exprsingle COLON exprsingle
    ;

mappingarrowtarget
    :  '=!>' arrowtarget
    ;

maptype
    : anymaptype
    | typedmaptype
    ;

markedncname
    :  '#' QName
    ;

// A single XQuery module (library or main)
module_
    : versiondecl? (librarymodule | mainmodule)
    ;

// [5] ModuleDecl
moduledecl
    :  'module'  'namespace' NCName EQ uriliteral  ';'
    ;

// [25] ModuleImport
moduleimport
    :  'import'  'module' ( 'namespace' NCName EQ)? uriliteral (
         'at' uriliteral ( ',' uriliteral)*
    )?
    ;

multiplicativeexpr
    : unionexpr (( '*' |  '\u00D7' |  'div' |  '\u00F7' |  'idiv' |  'mod') unionexpr)*
    ;

namedfunctionref
    : eqname  '#' IntegerLiteral
    ;

// [36] NamedRecordTypeDecl (new in XQuery 4.0)
namedrecordtypedecl
    :  'declare'  'record' eqname EQ typedrecordtype
    ;

// [21] NamespaceDecl
namespacedecl
    :  'declare'  'namespace' NCName EQ uriliteral
    ;

namespacenodetype
    :  'namespace-node'  '('  ')'
    ;

nametest
    : eqname
    | wildcard
    ;

nametestunion
    : nametest
    ;

nextvar
    : eqname
    ;

// ============================================================
// A.12 Node Constructors
// ============================================================

nodeConstructor
    : directconstructor
    | computedconstructor
    ;

nodecomp
    :  'is'
    |  'is-not'
    | nodeprecedes
    | nodefollows
    |  'precedes-or-is'
    |  'follows-or-is'
    ;

nodefollows
    :  '>>'
    |  'follows'
    ;

nodeprecedes
    :  '<<'
    |  'precedes'
    ;

nodetest
    : unionnodetest
    | simplenodetest
    | dynamicnodetest
    ;

numericliteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

occurrenceindicator
    :  '?'
    |  '*'
    |  '+'
    ;

// [37] OptionDecl
optiondecl
    :  'declare'  'option' eqname StringLiteral
    ;

// [62] OrderByClause
orderbyclause
    : ( 'order'  'by' |  'stable'  'order'  'by') orderspec ( ',' orderspec)*
    ;

orderedexpr
    :  'ordered' enclosedexpr
    ;

// [13] OrderingModeDecl
orderingmodedecl
    :  'declare'  'ordering' ( 'ordered' |  'unordered')
    ;

// [64] OrderModifier
ordermodifier
    : ( 'ascending' |  'descending')? ( 'empty' ( 'greatest' |  'least'))? (
         'collation' uriliteral
    )?
    ;

// [63] OrderSpec
orderspec
    : exprsingle ordermodifier
    ;

orexpr
    : andexpr ( 'or' andexpr)*
    ;

otherwiseexpr
    : stringconcatexpr ( 'otherwise' stringconcatexpr)*
    ;

// ============================================================
// Shared helper rules
// ============================================================

paramlist
    : varnameandtype ( ',' varnameandtype)*
    ;

// [33] ParamListWithDefaults (XQuery 4.0 allows default parameter values)
paramlistwithdefaults
    : paramwithdefault ( ',' paramwithdefault)*
    ;

// [34] ParamWithDefault
paramwithdefault
    :  '$' eqname typedeclaration? ( ':=' exprsingle)?
    ;

parenthesizedexpr
    :  '(' expr?  ')'
    ;

// ============================================================
// A.9 Path Expressions
// ============================================================

pathexpr
    : absolutepathexpr
    | relativepathexpr
    ;

pipelineexpr
    : arrowexpr
    ;

positionalargumentlist
    :  '(' positionalarguments?  ')'
    ;

positionalarguments
    : argument ( ',' argument)*
    ;

// [50] PositionalVar
positionalvar
    :  'at'  '$' eqname
    ;

positionalvarname
    : eqname
    ;

// ============================================================
// A.10 Postfix / Primary Expressions
// ============================================================

postfixexpr
    : primaryexpr (
        predicate
        | positionalargumentlist
        | lookup
        | ( '=?>' QName positionalargumentlist)
    )*
    ;

predicate
    :  '[' expr  ']'
    ;

predicatelist
    : predicate*
    ;

// [16] PreserveMode
preservemode
    :  'preserve'
    |  'no-preserve'
    ;

previousvar
    : eqname
    ;

// PrimaryExpr: XQuery adds directconstructor, orderedexpr, unorderedexpr
primaryexpr
    : literal
    | varref
    | parenthesizedexpr
    | contextvalueref
    | functioncall
    | nodeConstructor
    | functionitemexpr
    | mapconstructor
    | arrayconstructor
    | stringtemplate
    | unarylookup
    | orderedexpr
    | unorderedexpr
    ;

processinginstructionnodetype
    :  'processing-instruction'  '(' (QName | StringLiteral)?  ')'
    ;

// ============================================================
// A.2 Prolog
// ============================================================

// [6] Prolog: two phases -- setters/imports first, then annotated decls
prolog
    : (setter  ';' | defaultnamespacedecl  ';' | namespacedecl  ';' | import_  ';')* (
        annotateddecl  ';'
        | optiondecl  ';'
    )*
    ;

qnameliteral
    :  '#' eqname
    ;

// ============================================================
// A.8 Expression Operators (precedence order, lowest to highest)
// ============================================================

quantifiedexpr
    : ( 'some' |  'every') quantifierbinding ( ',' quantifierbinding)*  'satisfies' exprsingle
    ;

quantifierbinding
    : varnameandtype  'in' exprsingle
    ;

// ============================================================
// A.3 Query Body
// ============================================================

querybody
    : expr
    ;

// A file may contain multiple whitespace/semicolon-separated modules
querylist
    : module_ ( ';'* module_)*  ';'* EOF
    ;

quotattrcontentchar
    : QuotAttrContentChar
    | EscapeQuot
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

rangeexpr
    : additiveexpr ( 'to' additiveexpr)?
    ;

// RecordPutExpr: new in XPath/XQuery 4.0
recordputexpr
    : instanceofexpr ( '+:=' instanceofexpr)*
    ;

recordtype
    : anyrecordtype
    | typedrecordtype
    ;

regularitemtype
    : anyitemtype
    | xnodetype
    | gnodetype
    | jnodetype
    | maptype
    | arraytype
    | recordtype
    | enumerationtype
    ;

relativepathexpr
    : stepexpr (( '/' |  '//') stepexpr)*
    ;

restricteddynamiccall
    : (varref | parenthesizedexpr | functionitemexpr | mapconstructor | arrayconstructor) positionalargumentlist
    ;

// [44] ReturnClause
returnclause
    :  'return' exprsingle
    ;

schemaattributenodetype
    :  'schema-attribute'  '(' attributename  ')'
    ;

schemaelementnodetype
    :  'schema-element'  '(' elementname  ')'
    ;

// [23] SchemaImport
schemaimport
    :  'import'  'schema' schemaprefix? uriliteral ( 'at' uriliteral ( ',' uriliteral)*)?
    ;

// [24] SchemaPrefix
schemaprefix
    :  'namespace' NCName EQ
    |  'default'  'element'  'namespace'
    ;

selector
    : eqname
    | wildcard
    ;

sequencearrowtarget
    :  '=>' arrowtarget
    ;

sequencetype
    :  'empty-sequence'  '('  ')'
    | itemtype occurrenceindicator?
    ;

// [72] SequenceTypeUnion
sequencetypeunion
    : sequencetype ( '|' sequencetype)*
    ;

// [8] Setter
setter
    : boundaryspacedecl
    | defaultcollationdecl
    | baseuridecl
    | constructiondecl
    | orderingmodedecl
    | emptyorderdecl
    | copynamespaces_decl
    | decimaldecl
    ;

simplemapexpr
    : pathexpr ( '!' pathexpr)*
    ;

simplenodetest
    : typetest
    | selector
    ;

simpletypename
    : typename_
    ;

squarearrayconstructor
    :  '[' (exprsingle ( ',' exprsingle)*)?  ']'
    ;

stepexpr
    : postfixexpr
    | axisstep
    ;

stringconcatexpr
    : rangeexpr ( '||' rangeexpr)*
    ;

stringtemplate
    : StringTemplate
    ;

// [67] SwitchCaseClause
switchcaseclause
    : ( 'case' switchcaseoperand)+  'return' exprsingle
    ;

// [68] SwitchCaseOperand
switchcaseoperand
    : exprsingle
    ;

// ============================================================
// A.6 Switch / Typeswitch expressions
// ============================================================

// [65] SwitchExpr
switchexpr
    :  'switch'  '(' expr  ')' switchcaseclause+  'default'  'return' exprsingle
    ;

textnodetype
    :  'text'  '('  ')'
    ;

// TraceClause (new in XQuery 4.0): trace($label, $expr) or trace($expr)
traceclause
    :  'trace'  '(' exprsingle ( ',' exprsingle)?  ')'
    ;

treatexpr
    : castableexpr ( 'treat'  'as' sequencetype)?
    ;

// ============================================================
// A.7 Try-Catch
// ============================================================

// [73] TryCatchExpr
trycatchexpr
    : tryclause catchclause+ finallyclause?
    ;

// [74] TryClause
tryclause
    :  'try' enclosedexpr
    ;

typedarraytype
    :  'array'  '(' sequencetype  ')'
    ;

// ============================================================
// A.14 Type Declarations and Sequence Types
// ============================================================

typedeclaration
    :  'as' sequencetype
    ;

typedfunctionparam
    : ( '$' eqname  'as')? sequencetype
    ;

typedfunctionparamlist
    : typedfunctionparam ( ',' typedfunctionparam)*
    ;

typedfunctiontype
    : ( 'function' |  'fn')  '(' typedfunctionparamlist?  ')'  'as' sequencetype
    ;

typedmaptype
    :  'map'  '(' itemtype  ',' sequencetype  ')'
    ;

typedrecordtype
    :  'record'  '(' fielddeclarationlist  ')'
    ;

typename_
    : eqname
    ;

// [69] TypeswitchExpr
typeswitchexpr
    :  'typeswitch'  '(' expr  ')' caseclause+  'default' ( '$' eqname)?  'return' exprsingle
    ;

typetest
    : gnodetype
    | xnodetype
    | jnodetype
    ;

unaryexpr
    : ( '-' |  '+')* valueexpr
    ;

unarylookup
    :  '?' keyspecifier
    ;

unbracedactions
    :  'then' exprsingle  'else' exprsingle
    ;

unionexpr
    : intersectexceptexpr (( 'union' |  '|') intersectexceptexpr)*
    ;

unionnodetest
    :  '(' simplenodetest ( '|' simplenodetest)+  ')'
    ;

unorderedexpr
    :  'unordered' enclosedexpr
    ;

uriliteral
    : StringLiteral
    ;

// [ValidateExpr]
validateexpr
    :  'validate' validationmode? enclosedexpr
    ;

validationmode
    :  'lax'
    |  'strict'
    |  'type' typename_
    ;

valuecomp
    :  'eq'
    |  'ne'
    |  'lt'
    |  'le'
    |  'gt'
    |  'ge'
    ;

// [ValueExpr] XQuery extends XPath with ValidateExpr and ExtensionExpr
valueexpr
    : validateexpr
    | extensionexpr
    | simplemapexpr
    ;

// [28] VarDecl
vardecl
    :  'declare'  'variable'  '$' eqname typedeclaration? (
        ( ':=' vardefaultvalue)
        | ( 'external' ( ':=' vardefaultvalue)?)
    )
    ;

// [29] VarDefaultValue
vardefaultvalue
    : exprsingle
    ;

varnameandtype
    :  '$' eqname typedeclaration?
    ;

varref
    :  '$' eqname
    ;

// [2] VersionDecl
versiondecl
    :  'xquery' (
        ( 'encoding' StringLiteral)
        | ( 'version' StringLiteral ( 'encoding' StringLiteral)?)
    )  ';'
    ;

// [59] WhereClause
whereclause
    :  'where' exprsingle
    ;

// WhileClause (new in XQuery 4.0)
whileclause
    :  'while'  '(' exprsingle  ')'
    ;

wildcard
    :  '*'
    | QName  ':*'
    |  '*:' QName
    | BracedURILiteral  '*'
    ;

// [53] WindowClause
windowclause
    :  'for' ( 'tumbling' |  'sliding')  'window' varnameandtype  'in' exprsingle windowstartcondition windowendcondition?
    ;

// [56] WindowEndCondition
windowendcondition
    :  'only'?  'end' windowvars  'when' exprsingle
    ;

// [55] WindowStartCondition
windowstartcondition
    :  'start' windowvars  'when' exprsingle
    ;

// [57] WindowVars
windowvars
    : currentvar? ( 'at'  '$' positionalvarname)? ( 'previous'  '$' previousvar)? (
         'next'  '$' nextvar
    )?
    ;

xnodetype
    : documentnodetype
    | elementnodetype
    | attributenodetype
    | schemaelementnodetype
    | schemaattributenodetype
    | processinginstructionnodetype
    | commentnodetype
    | textnodetype
    | namespacenodetype
    | anyxnodetype
    ;