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
    : DD
    | AT nodetest
    | simplenodetest
    ;

absolutepathexpr
    : SLASH relativepathexpr?
    | SS relativepathexpr
    ;

additiveexpr
    : multiplicativeexpr ((PLUS | MINUS) multiplicativeexpr)*
    ;

andexpr
    : comparisonexpr (KW_AND comparisonexpr)*
    ;

// [26] AnnotatedDecl
annotateddecl
    : annotation* (vardecl | contextitemdecl | functiondecl | itemtypedecl | namedrecordtypedecl)
    ;

// [27] Annotation: %EQName or %EQName(Literal, ...)
annotation
    : POUND eqname (OP literal (COMMA literal)* CP)?
    ;

anyarraytype
    : KW_ARRAY OP STAR CP
    ;

anyfunctiontype
    : (KW_FUNCTION | KW_FN) OP STAR CP
    ;

anyitemtype
    : KW_ITEM OP CP
    ;

anymaptype
    : KW_MAP OP STAR CP
    ;

anyrecordtype
    : KW_RECORD OP STAR CP
    ;

anyxnodetype
    : KW_NODE OP CP
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
    : OP ((positionalarguments (COMMA keywordarguments)?) | keywordarguments)? CP
    ;

argumentplaceholder
    : QM
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
    : KW_ATTRIBUTE OP (nametestunion (COMMA typename_)?)? CP
    ;

// Entry point for Maven antlr4test-maven-plugin: semicolon-separated queries/modules
auxilary
    : (module_ SEMI?)+ EOF
    ;

axis
    : KW_ANCESTOR COLONCOLON
    | KW_ANCESTOR_OR_SELF COLONCOLON
    | KW_ATTRIBUTE COLONCOLON
    | KW_CHILD COLONCOLON
    | KW_DESCENDANT COLONCOLON
    | KW_DESCENDANT_OR_SELF COLONCOLON
    | KW_FOLLOWING COLONCOLON
    | KW_FOLLOWING_OR_SELF COLONCOLON
    | KW_FOLLOWING_SIBLING COLONCOLON
    | KW_FOLLOWING_SIBLING_OR_SELF COLONCOLON
    | KW_NAMESPACE COLONCOLON
    | KW_PARENT COLONCOLON
    | KW_PRECEDING COLONCOLON
    | KW_PRECEDING_OR_SELF COLONCOLON
    | KW_PRECEDING_SIBLING COLONCOLON
    | KW_PRECEDING_SIBLING_OR_SELF COLONCOLON
    | KW_SELF COLONCOLON
    ;

axisstep
    : (abbreviatedstep | fullstep) (predicate | lookup)*
    ;

// [11] BaseURIDecl
baseuridecl
    : KW_DECLARE KW_BASE_URI uriliteral
    ;

// [9] BoundarySpaceDecl
boundaryspacedecl
    : KW_DECLARE KW_BOUNDARY_SPACE (KW_PRESERVE | KW_STRIP)
    ;

bracedaction
    : enclosedexpr
    ;

// [71] CaseClause
caseclause
    : KW_CASE (DOLLAR eqname KW_AS)? sequencetypeunion KW_RETURN exprsingle
    ;

castableexpr
    : castexpr (KW_CASTABLE KW_AS casttarget occurrenceindicator?)?
    ;

castexpr
    : pipelineexpr (KW_CAST KW_AS casttarget occurrenceindicator?)?
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
    : KW_CATCH catcherrlist enclosedexpr
    ;

catcherrlist
    : catcherror (P catcherror)*
    ;

catcherror
    : eqname
    | STAR
    ;

cdsection
    : CDataSection
    ;

choiceitemtype
    : OP itemtype (P itemtype)* CP
    ;

commentnodetype
    : KW_COMMENT OP CP
    ;

compAttrconstructor
    : KW_ATTRIBUTE compnodename enclosedexpr
    ;

compCommentconstructor
    : KW_COMMENT enclosedexpr
    ;

compElemconstructor
    : KW_ELEMENT compnodename enclosedcontentexpr
    ;

compNSconstructor
    : KW_NAMESPACE compnodencname enclosedexpr
    ;

compPIconstructor
    : KW_PROCESSING_INSTRUCTION compnodencname enclosedexpr
    ;

comparisonexpr
    : otherwiseexpr ((valuecomp | generalcomp | nodecomp) otherwiseexpr)?
    ;

compdocconstructor
    : KW_DOCUMENT enclosedexpr
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
    : KW_TEXT enclosedexpr
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
    | MINUS numericliteral
    | qnameliteral
    | eqname OP CP
    ;

// [12] ConstructionDecl
constructiondecl
    : KW_DECLARE KW_CONSTRUCTION (KW_STRIP | KW_PRESERVE)
    ;

// [30] ContextItemDecl
contextitemdecl
    : KW_DECLARE KW_CONTEXT KW_ITEM (KW_AS itemtype)? (
        (CEQ vardefaultvalue)
        | (KW_EXTERNAL (CEQ vardefaultvalue)?)
    )
    ;

contextvalueref
    : D
    ;

// [15] CopyNamespacesDecl
copynamespaces_decl
    : KW_DECLARE KW_COPY_NAMESPACES preservemode COMMA inheritmode
    ;

// [58] CountClause
countclause
    : KW_COUNT DOLLAR eqname
    ;

curlyarrayconstructor
    : KW_ARRAY enclosedexpr
    ;

currentvar
    : DOLLAR eqname
    ;

// [18] DecimalFormatDecl
decimaldecl
    : KW_DECLARE (KW_DECIMAL_FORMAT eqname | KW_DEFAULT KW_DECIMAL_FORMAT) dfpropertyname*
    ;

// [10] DefaultCollationDecl
defaultcollationdecl
    : KW_DECLARE KW_DEFAULT KW_COLLATION uriliteral
    ;

// [20] DefaultNamespaceDecl
defaultnamespacedecl
    : KW_DECLARE KW_DEFAULT (KW_ELEMENT | KW_FUNCTION) KW_NAMESPACE uriliteral
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
    : KW_DOCUMENT_NODE OP (elementnodetype | schemaelementnodetype | nametestunion)? CP
    ;

dynamicnodetest
    : enclosedexpr
    ;

elementname
    : eqname
    ;

elementnodetype
    : KW_ELEMENT OP (nametestunion (COMMA typename_ QM?)?)? CP
    ;

// [14] EmptyOrderDecl
emptyorderdecl
    : KW_DECLARE KW_DEFAULT KW_ORDER KW_EMPTY (KW_GREATEST | KW_LEAST)
    ;

enclosedcontentexpr
    : enclosedexpr
    ;

enclosedexpr
    : OC expr? CC
    ;

enumerationtype
    : KW_ENUM OP StringLiteral (COMMA StringLiteral)* CP
    ;

// ============================================================
// A.15 EQName -- keywords that are also valid names
// ============================================================

eqname
    : QName
    | URIQualifiedName
    | KW_AFTER
    | KW_ALLOWING
    | KW_ANCESTOR
    | KW_ANCESTOR_OR_SELF
    | KW_AND
    | KW_ARRAY
    | KW_AS
    | KW_ASCENDING
    | KW_AT
    | KW_ATTRIBUTE
    | KW_BASE_URI
    | KW_BEFORE
    | KW_BOUNDARY_SPACE
    | KW_BY
    | KW_CASE
    | KW_CAST
    | KW_CASTABLE
    | KW_CATCH
    | KW_CHILD
    | KW_COLLATION
    | KW_COMMENT
    | KW_CONSTRUCTION
    | KW_CONTEXT
    | KW_COPY_NAMESPACES
    | KW_COUNT
    | KW_DECIMAL_FORMAT
    | KW_DECIMAL_SEPARATOR
    | KW_DECLARE
    | KW_DEFAULT
    | KW_DESCENDANT
    | KW_DESCENDANT_OR_SELF
    | KW_DESCENDING
    | KW_DIGIT
    | KW_DIV
    | KW_DOCUMENT_NODE
    | KW_DOCUMENT
    | KW_ELEMENT
    | KW_ELSE
    | KW_EMPTY_SEQUENCE
    | KW_EMPTY
    | KW_ENCODING
    | KW_END
    | KW_ENUM
    | KW_EQ
    | KW_EVERY
    | KW_EXCEPT
    | KW_EXPONENT_SEPARATOR
    | KW_EXTERNAL
    | KW_FINALLY
    | KW_FIRST
    | KW_FN
    | KW_FOLLOWING
    | KW_FOLLOWING_OR_SELF
    | KW_FOLLOWING_SIBLING
    | KW_FOLLOWING_SIBLING_OR_SELF
    | KW_FOLLOWS
    | KW_FOLLOWS_OR_IS
    | KW_FOR
    | KW_FUNCTION
    | KW_GE
    | KW_GNODE
    | KW_GREATEST
    | KW_GROUP
    | KW_GROUPING_SEPARATOR
    | KW_GT
    | KW_IDIV
    | KW_IF
    | KW_IMPORT
    | KW_IN
    | KW_INFINITY
    | KW_INHERIT
    | KW_INSTANCE
    | KW_INTERSECT
    | KW_IS
    | KW_IS_NOT
    | KW_ITEM
    | KW_JNODE
    | KW_KEY
    | KW_LAST
    | KW_LAX
    | KW_LE
    | KW_LEAST
    | KW_LET
    | KW_LT
    | KW_MAP
    | KW_MEMBER
    | KW_MINUS_SIGN
    | KW_MOD
    | KW_MODULE
    | KW_NAN
    | KW_NAMESPACE
    | KW_NAMESPACE_NODE
    | KW_NE
    | KW_NEXT
    | KW_NO_INHERIT
    | KW_NO_PRESERVE
    | KW_NODE
    | KW_OF
    | KW_ONLY
    | KW_OPTION
    | KW_OR
    | KW_ORDER
    | KW_ORDERED
    | KW_ORDERING
    | KW_OTHERWISE
    | KW_PARENT
    | KW_PATTERN_SEPARATOR
    | KW_PERCENT
    | KW_PER_MILLE
    | KW_PRECEDES
    | KW_PRECEDES_OR_IS
    | KW_PRECEDING
    | KW_PRECEDING_OR_SELF
    | KW_PRECEDING_SIBLING
    | KW_PRECEDING_SIBLING_OR_SELF
    | KW_PRESERVE
    | KW_PREVIOUS
    | KW_PROCESSING_INSTRUCTION
    | KW_RECORD
    | KW_RETURN
    | KW_SATISFIES
    | KW_SCHEMA
    | KW_SCHEMA_ATTRIBUTE
    | KW_SCHEMA_ELEMENT
    | KW_SELF
    | KW_SLIDING
    | KW_SOME
    | KW_STABLE
    | KW_START
    | KW_STRICT
    | KW_STRIP
    | KW_SWITCH
    | KW_TEXT
    | KW_THEN
    | KW_TO
    | KW_TRACE
    | KW_TREAT
    | KW_TRY
    | KW_TUMBLING
    | KW_TYPE
    | KW_TYPESWITCH
    | KW_UNION
    | KW_UNORDERED
    | KW_VALIDATE
    | KW_VALUE
    | KW_VARIABLE
    | KW_VERSION
    | KW_WHEN
    | KW_WHERE
    | KW_WHILE
    | KW_WINDOW
    | KW_XQUERY
    | KW_ZERO_DIGIT
    ;

// ============================================================
// A.4 Expressions
// ============================================================

// [39] Expr
expr
    : exprsingle (COMMA exprsingle)*
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
    : DD
    ;

// [ExtensionExpr]
extensionexpr
    : Pragma+ enclosedexpr
    ;

fielddeclaration
    : fieldname QM? (KW_AS sequencetype)?
    ;

fielddeclarationlist
    : fielddeclaration (COMMA fielddeclaration)* (COMMA extendedfielddeclaration)?
    | extendedfielddeclaration
    ;

fieldname
    : QName
    | StringLiteral
    ;

// [76] FinallyClause (new in XQuery 4.0)
finallyclause
    : KW_FINALLY enclosedexpr
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
    : KW_FOR forbinding (COMMA forbinding)*
    ;

// [49] ForEntryBinding (XPath/XQuery 4.0: iterates over map entries)
forentrybinding
    : (forentrykeybinding forentryvaluebinding | forentryvaluebinding) positionalvar? KW_IN exprsingle
    ;

forentrykeybinding
    : KW_KEY varnameandtype
    ;

forentryvaluebinding
    : KW_VALUE varnameandtype
    ;

// [47] ForItemBinding (AllowingEmpty is XQuery 3.0+)
foritembinding
    : varnameandtype (KW_ALLOWING KW_EMPTY)? positionalvar? KW_IN exprsingle
    ;

// [48] ForMemberBinding (XPath/XQuery 4.0: iterates over array members)
formemberbinding
    : KW_MEMBER varnameandtype positionalvar? KW_IN exprsingle
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
    : KW_DECLARE KW_FUNCTION eqname functionsignature (functionbody | KW_EXTERNAL)
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
    : OP paramlistwithdefaults? CP typedeclaration?
    ;

functiontype
    : anyfunctiontype
    | typedfunctiontype
    ;

generalcomp
    : EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    ;

gnodetype
    : KW_GNODE OP CP
    ;

// [60] GroupByClause
groupbyclause
    : KW_GROUP KW_BY groupingspec (COMMA groupingspec)*
    ;

// [61] GroupingSpec
groupingspec
    : (varnameandtype (CEQ exprsingle)? | exprsingle) (KW_COLLATION uriliteral)?
    ;

ifexpr
    : KW_IF OP expr CP (unbracedactions | bracedaction)
    ;

// [22] Import
import_
    : schemaimport
    | moduleimport
    ;

// [17] InheritMode
inheritmode
    : KW_INHERIT
    | KW_NO_INHERIT
    ;

// [42] InitialClause
initialclause
    : forclause
    | letclause
    | windowclause
    ;

inlinefunctionexpr
    : (KW_FUNCTION | KW_FN) functionsignature functionbody
    ;

instanceofexpr
    : treatexpr (KW_INSTANCE KW_OF sequencetype)?
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
    : recordputexpr ((KW_INTERSECT | KW_EXCEPT) recordputexpr)*
    ;

itemtype
    : regularitemtype
    | functiontype
    | typename_
    | choiceitemtype
    ;

// [35] ItemTypeDecl (new in XQuery 4.0: type aliases)
itemtypedecl
    : KW_DECLARE KW_TYPE eqname EQ itemtype
    ;

jnodetype
    : KW_JNODE OP (STAR | jrootselector | QName | constant) (COMMA sequencetype)? CP
    ;

jrootselector
    : OP CP
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
    : eqname CEQ argument
    ;

keywordarguments
    : keywordargument (COMMA keywordargument)*
    ;

letarraybinding
    : DOLLAR OB varnameandtype CB typedeclaration? CEQ exprsingle
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
    : KW_LET letbinding (COMMA letbinding)*
    ;

letmapbinding
    : DOLLAR OC varnameandtype CC typedeclaration? CEQ exprsingle
    ;

letsequencebinding
    : DOLLAR OP varnameandtype CP typedeclaration? CEQ exprsingle
    ;

letvaluebinding
    : varnameandtype CEQ exprsingle
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
    : QM keyspecifier
    ;

lookupwildcard
    : STAR
    ;

// [3] MainModule
mainmodule
    : prolog querybody
    ;

// ============================================================
// A.13 Map / Array Constructors
// ============================================================

mapconstructor
    : KW_MAP OC (mapconstructorentry (COMMA mapconstructorentry)*)? CC
    ;

mapconstructorentry
    : exprsingle COLON exprsingle
    ;

mappingarrowtarget
    : MAPPING_ARROW arrowtarget
    ;

maptype
    : anymaptype
    | typedmaptype
    ;

markedncname
    : POUND QName
    ;

// A single XQuery module (library or main)
module_
    : versiondecl? (librarymodule | mainmodule)
    ;

// [5] ModuleDecl
moduledecl
    : KW_MODULE KW_NAMESPACE NCName EQ uriliteral SEMI
    ;

// [25] ModuleImport
moduleimport
    : KW_IMPORT KW_MODULE (KW_NAMESPACE NCName EQ)? uriliteral (
        KW_AT uriliteral (COMMA uriliteral)*
    )?
    ;

multiplicativeexpr
    : unionexpr ((STAR | TIMES_SIGN | KW_DIV | DIV_SIGN | KW_IDIV | KW_MOD) unionexpr)*
    ;

namedfunctionref
    : eqname POUND IntegerLiteral
    ;

// [36] NamedRecordTypeDecl (new in XQuery 4.0)
namedrecordtypedecl
    : KW_DECLARE KW_RECORD eqname EQ typedrecordtype
    ;

// [21] NamespaceDecl
namespacedecl
    : KW_DECLARE KW_NAMESPACE NCName EQ uriliteral
    ;

namespacenodetype
    : KW_NAMESPACE_NODE OP CP
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
    : KW_IS
    | KW_IS_NOT
    | nodeprecedes
    | nodefollows
    | KW_PRECEDES_OR_IS
    | KW_FOLLOWS_OR_IS
    ;

nodefollows
    : GG
    | KW_FOLLOWS
    ;

nodeprecedes
    : LL
    | KW_PRECEDES
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
    : QM
    | STAR
    | PLUS
    ;

// [37] OptionDecl
optiondecl
    : KW_DECLARE KW_OPTION eqname StringLiteral
    ;

// [62] OrderByClause
orderbyclause
    : (KW_ORDER KW_BY | KW_STABLE KW_ORDER KW_BY) orderspec (COMMA orderspec)*
    ;

orderedexpr
    : KW_ORDERED enclosedexpr
    ;

// [13] OrderingModeDecl
orderingmodedecl
    : KW_DECLARE KW_ORDERING (KW_ORDERED | KW_UNORDERED)
    ;

// [64] OrderModifier
ordermodifier
    : (KW_ASCENDING | KW_DESCENDING)? (KW_EMPTY (KW_GREATEST | KW_LEAST))? (
        KW_COLLATION uriliteral
    )?
    ;

// [63] OrderSpec
orderspec
    : exprsingle ordermodifier
    ;

orexpr
    : andexpr (KW_OR andexpr)*
    ;

otherwiseexpr
    : stringconcatexpr (KW_OTHERWISE stringconcatexpr)*
    ;

// ============================================================
// Shared helper rules
// ============================================================

paramlist
    : varnameandtype (COMMA varnameandtype)*
    ;

// [33] ParamListWithDefaults (XQuery 4.0 allows default parameter values)
paramlistwithdefaults
    : paramwithdefault (COMMA paramwithdefault)*
    ;

// [34] ParamWithDefault
paramwithdefault
    : DOLLAR eqname typedeclaration? (CEQ exprsingle)?
    ;

parenthesizedexpr
    : OP expr? CP
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
    : OP positionalarguments? CP
    ;

positionalarguments
    : argument (COMMA argument)*
    ;

// [50] PositionalVar
positionalvar
    : KW_AT DOLLAR eqname
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
        | (METHOD_ARROW QName positionalargumentlist)
    )*
    ;

predicate
    : OB expr CB
    ;

predicatelist
    : predicate*
    ;

// [16] PreserveMode
preservemode
    : KW_PRESERVE
    | KW_NO_PRESERVE
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
    : KW_PROCESSING_INSTRUCTION OP (QName | StringLiteral)? CP
    ;

// ============================================================
// A.2 Prolog
// ============================================================

// [6] Prolog: two phases -- setters/imports first, then annotated decls
prolog
    : (setter SEMI | defaultnamespacedecl SEMI | namespacedecl SEMI | import_ SEMI)* (
        annotateddecl SEMI
        | optiondecl SEMI
    )*
    ;

qnameliteral
    : POUND eqname
    ;

// ============================================================
// A.8 Expression Operators (precedence order, lowest to highest)
// ============================================================

quantifiedexpr
    : (KW_SOME | KW_EVERY) quantifierbinding (COMMA quantifierbinding)* KW_SATISFIES exprsingle
    ;

quantifierbinding
    : varnameandtype KW_IN exprsingle
    ;

// ============================================================
// A.3 Query Body
// ============================================================

querybody
    : expr
    ;

// A file may contain multiple whitespace/semicolon-separated modules
querylist
    : module_ (SEMI* module_)* SEMI* EOF
    ;

quotattrcontentchar
    : QuotAttrContentChar
    | EscapeQuot
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

rangeexpr
    : additiveexpr (KW_TO additiveexpr)?
    ;

// RecordPutExpr: new in XPath/XQuery 4.0
recordputexpr
    : instanceofexpr (PLUS_CEQ instanceofexpr)*
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
    : stepexpr ((SLASH | SS) stepexpr)*
    ;

restricteddynamiccall
    : (varref | parenthesizedexpr | functionitemexpr | mapconstructor | arrayconstructor) positionalargumentlist
    ;

// [44] ReturnClause
returnclause
    : KW_RETURN exprsingle
    ;

schemaattributenodetype
    : KW_SCHEMA_ATTRIBUTE OP attributename CP
    ;

schemaelementnodetype
    : KW_SCHEMA_ELEMENT OP elementname CP
    ;

// [23] SchemaImport
schemaimport
    : KW_IMPORT KW_SCHEMA schemaprefix? uriliteral (KW_AT uriliteral (COMMA uriliteral)*)?
    ;

// [24] SchemaPrefix
schemaprefix
    : KW_NAMESPACE NCName EQ
    | KW_DEFAULT KW_ELEMENT KW_NAMESPACE
    ;

selector
    : eqname
    | wildcard
    ;

sequencearrowtarget
    : EG arrowtarget
    ;

sequencetype
    : KW_EMPTY_SEQUENCE OP CP
    | itemtype occurrenceindicator?
    ;

// [72] SequenceTypeUnion
sequencetypeunion
    : sequencetype (P sequencetype)*
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
    : pathexpr (BANG pathexpr)*
    ;

simplenodetest
    : typetest
    | selector
    ;

simpletypename
    : typename_
    ;

squarearrayconstructor
    : OB (exprsingle (COMMA exprsingle)*)? CB
    ;

stepexpr
    : postfixexpr
    | axisstep
    ;

stringconcatexpr
    : rangeexpr (PP rangeexpr)*
    ;

stringtemplate
    : StringTemplate
    ;

// [67] SwitchCaseClause
switchcaseclause
    : (KW_CASE switchcaseoperand)+ KW_RETURN exprsingle
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
    : KW_SWITCH OP expr CP switchcaseclause+ KW_DEFAULT KW_RETURN exprsingle
    ;

textnodetype
    : KW_TEXT OP CP
    ;

// TraceClause (new in XQuery 4.0): trace($label, $expr) or trace($expr)
traceclause
    : KW_TRACE OP exprsingle (COMMA exprsingle)? CP
    ;

treatexpr
    : castableexpr (KW_TREAT KW_AS sequencetype)?
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
    : KW_TRY enclosedexpr
    ;

typedarraytype
    : KW_ARRAY OP sequencetype CP
    ;

// ============================================================
// A.14 Type Declarations and Sequence Types
// ============================================================

typedeclaration
    : KW_AS sequencetype
    ;

typedfunctionparam
    : (DOLLAR eqname KW_AS)? sequencetype
    ;

typedfunctionparamlist
    : typedfunctionparam (COMMA typedfunctionparam)*
    ;

typedfunctiontype
    : (KW_FUNCTION | KW_FN) OP typedfunctionparamlist? CP KW_AS sequencetype
    ;

typedmaptype
    : KW_MAP OP itemtype COMMA sequencetype CP
    ;

typedrecordtype
    : KW_RECORD OP fielddeclarationlist CP
    ;

typename_
    : eqname
    ;

// [69] TypeswitchExpr
typeswitchexpr
    : KW_TYPESWITCH OP expr CP caseclause+ KW_DEFAULT (DOLLAR eqname)? KW_RETURN exprsingle
    ;

typetest
    : gnodetype
    | xnodetype
    | jnodetype
    ;

unaryexpr
    : (MINUS | PLUS)* valueexpr
    ;

unarylookup
    : QM keyspecifier
    ;

unbracedactions
    : KW_THEN exprsingle KW_ELSE exprsingle
    ;

unionexpr
    : intersectexceptexpr ((KW_UNION | P) intersectexceptexpr)*
    ;

unionnodetest
    : OP simplenodetest (P simplenodetest)+ CP
    ;

unorderedexpr
    : KW_UNORDERED enclosedexpr
    ;

uriliteral
    : StringLiteral
    ;

// [ValidateExpr]
validateexpr
    : KW_VALIDATE validationmode? enclosedexpr
    ;

validationmode
    : KW_LAX
    | KW_STRICT
    | KW_TYPE typename_
    ;

valuecomp
    : KW_EQ
    | KW_NE
    | KW_LT
    | KW_LE
    | KW_GT
    | KW_GE
    ;

// [ValueExpr] XQuery extends XPath with ValidateExpr and ExtensionExpr
valueexpr
    : validateexpr
    | extensionexpr
    | simplemapexpr
    ;

// [28] VarDecl
vardecl
    : KW_DECLARE KW_VARIABLE DOLLAR eqname typedeclaration? (
        (CEQ vardefaultvalue)
        | (KW_EXTERNAL (CEQ vardefaultvalue)?)
    )
    ;

// [29] VarDefaultValue
vardefaultvalue
    : exprsingle
    ;

varnameandtype
    : DOLLAR eqname typedeclaration?
    ;

varref
    : DOLLAR eqname
    ;

// [2] VersionDecl
versiondecl
    : KW_XQUERY (
        (KW_ENCODING StringLiteral)
        | (KW_VERSION StringLiteral (KW_ENCODING StringLiteral)?)
    ) SEMI
    ;

// [59] WhereClause
whereclause
    : KW_WHERE exprsingle
    ;

// WhileClause (new in XQuery 4.0)
whileclause
    : KW_WHILE OP exprsingle CP
    ;

wildcard
    : STAR
    | QName CS
    | SC QName
    | BracedURILiteral STAR
    ;

// [53] WindowClause
windowclause
    : KW_FOR (KW_TUMBLING | KW_SLIDING) KW_WINDOW varnameandtype KW_IN exprsingle windowstartcondition windowendcondition?
    ;

// [56] WindowEndCondition
windowendcondition
    : KW_ONLY? KW_END windowvars KW_WHEN exprsingle
    ;

// [55] WindowStartCondition
windowstartcondition
    : KW_START windowvars KW_WHEN exprsingle
    ;

// [57] WindowVars
windowvars
    : currentvar? (KW_AT DOLLAR positionalvarname)? (KW_PREVIOUS DOLLAR previousvar)? (
        KW_NEXT DOLLAR nextvar
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