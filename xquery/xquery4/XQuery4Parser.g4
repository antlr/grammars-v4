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

// ============================================================
// A.1 Top-level entry points
// ============================================================

// A file may contain multiple whitespace/semicolon-separated modules
querylist
    : module_ (SEMI* module_)* SEMI* EOF
    ;

// A single XQuery module (library or main)
module_
    : versiondecl? (librarymodule | mainmodule)
    ;

// [2] VersionDecl
versiondecl
    : KW_XQUERY (
        (KW_ENCODING StringLiteral)
        | (KW_VERSION StringLiteral (KW_ENCODING StringLiteral)?)
    ) SEMI
    ;

// [3] MainModule
mainmodule
    : prolog querybody
    ;

// [4] LibraryModule
librarymodule
    : moduledecl prolog
    ;

// [5] ModuleDecl
moduledecl
    : KW_MODULE KW_NAMESPACE NCName EQ uriliteral SEMI
    ;

// ============================================================
// A.2 Prolog
// ============================================================

// [6] Prolog: two phases -- setters/imports first, then annotated decls
prolog
    : (setter SEMI | defaultnamespacedecl SEMI | namespacedecl SEMI | import_ SEMI)*
      (annotateddecl SEMI | optiondecl SEMI)*
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

// [9] BoundarySpaceDecl
boundaryspacedecl
    : KW_DECLARE KW_BOUNDARY_SPACE (KW_PRESERVE | KW_STRIP)
    ;

// [10] DefaultCollationDecl
defaultcollationdecl
    : KW_DECLARE KW_DEFAULT KW_COLLATION uriliteral
    ;

// [11] BaseURIDecl
baseuridecl
    : KW_DECLARE KW_BASE_URI uriliteral
    ;

// [12] ConstructionDecl
constructiondecl
    : KW_DECLARE KW_CONSTRUCTION (KW_STRIP | KW_PRESERVE)
    ;

// [13] OrderingModeDecl
orderingmodedecl
    : KW_DECLARE KW_ORDERING (KW_ORDERED | KW_UNORDERED)
    ;

// [14] EmptyOrderDecl
emptyorderdecl
    : KW_DECLARE KW_DEFAULT KW_ORDER KW_EMPTY (KW_GREATEST | KW_LEAST)
    ;

// [15] CopyNamespacesDecl
copynamespaces_decl
    : KW_DECLARE KW_COPY_NAMESPACES preservemode COMMA inheritmode
    ;

// [16] PreserveMode
preservemode
    : KW_PRESERVE | KW_NO_PRESERVE
    ;

// [17] InheritMode
inheritmode
    : KW_INHERIT | KW_NO_INHERIT
    ;

// [18] DecimalFormatDecl
decimaldecl
    : KW_DECLARE (KW_DECIMAL_FORMAT eqname | KW_DEFAULT KW_DECIMAL_FORMAT) dfpropertyname*
    ;

// [19] DFPropertyName: eqname covers all property keywords (decimal-separator, etc.)
dfpropertyname
    : eqname EQ StringLiteral
    ;

// [20] DefaultNamespaceDecl
defaultnamespacedecl
    : KW_DECLARE KW_DEFAULT (KW_ELEMENT | KW_FUNCTION) KW_NAMESPACE uriliteral
    ;

// [21] NamespaceDecl
namespacedecl
    : KW_DECLARE KW_NAMESPACE NCName EQ uriliteral
    ;

// [22] Import
import_
    : schemaimport
    | moduleimport
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

// [25] ModuleImport
moduleimport
    : KW_IMPORT KW_MODULE (KW_NAMESPACE NCName EQ)? uriliteral (KW_AT uriliteral (COMMA uriliteral)*)?
    ;

// [26] AnnotatedDecl
annotateddecl
    : annotation* (vardecl | contextitemdecl | functiondecl | itemtypedecl | namedrecordtypedecl)
    ;

// [27] Annotation: %EQName or %EQName(Literal, ...)
annotation
    : POUND eqname (OP literal (COMMA literal)* CP)?
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

// [30] ContextItemDecl
contextitemdecl
    : KW_DECLARE KW_CONTEXT KW_ITEM (KW_AS itemtype)? (
        (CEQ vardefaultvalue)
        | (KW_EXTERNAL (CEQ vardefaultvalue)?)
    )
    ;

// [31] FunctionDecl
functiondecl
    : KW_DECLARE KW_FUNCTION eqname functionsignature (functionbody | KW_EXTERNAL)
    ;

// [32] FunctionSignature
functionsignature
    : OP paramlistwithdefaults? CP typedeclaration?
    ;

// [33] ParamListWithDefaults (XQuery 4.0 allows default parameter values)
paramlistwithdefaults
    : paramwithdefault (COMMA paramwithdefault)*
    ;

// [34] ParamWithDefault
paramwithdefault
    : DOLLAR eqname typedeclaration? (CEQ exprsingle)?
    ;

// [35] ItemTypeDecl (new in XQuery 4.0: type aliases)
itemtypedecl
    : KW_DECLARE KW_TYPE eqname EQ itemtype
    ;

// [36] NamedRecordTypeDecl (new in XQuery 4.0)
namedrecordtypedecl
    : KW_DECLARE KW_RECORD eqname EQ typedrecordtype
    ;

// [37] OptionDecl
optiondecl
    : KW_DECLARE KW_OPTION eqname StringLiteral
    ;

// ============================================================
// A.3 Query Body
// ============================================================

querybody
    : expr
    ;

uriliteral
    : StringLiteral
    ;

// ============================================================
// Shared helper rules
// ============================================================

paramlist
    : varnameandtype (COMMA varnameandtype)*
    ;

varnameandtype
    : DOLLAR eqname typedeclaration?
    ;

functionbody
    : enclosedexpr
    ;

enclosedexpr
    : OC expr? CC
    ;

enclosedcontentexpr
    : enclosedexpr
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

// ============================================================
// A.5 FLWOR Expressions
// ============================================================

// [41] FLWORExpr
flworexpr
    : initialclause intermediateclause* returnclause
    ;

// [42] InitialClause
initialclause
    : forclause
    | letclause
    | windowclause
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

// [44] ReturnClause
returnclause
    : KW_RETURN exprsingle
    ;

// [45] ForClause (multiple bindings per clause)
forclause
    : KW_FOR forbinding (COMMA forbinding)*
    ;

// [46] ForBinding
forbinding
    : foritembinding
    | formemberbinding
    | forentrybinding
    ;

// [47] ForItemBinding (AllowingEmpty is XQuery 3.0+)
foritembinding
    : varnameandtype (KW_ALLOWING KW_EMPTY)? positionalvar? KW_IN exprsingle
    ;

// [48] ForMemberBinding (XPath/XQuery 4.0: iterates over array members)
formemberbinding
    : KW_MEMBER varnameandtype positionalvar? KW_IN exprsingle
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

// [50] PositionalVar
positionalvar
    : KW_AT DOLLAR eqname
    ;

// [51] LetClause (multiple bindings per clause)
letclause
    : KW_LET letbinding (COMMA letbinding)*
    ;

// [52] LetBinding variants (XPath/XQuery 4.0 destructuring)
letbinding
    : letvaluebinding
    | letsequencebinding
    | letarraybinding
    | letmapbinding
    ;

letvaluebinding
    : varnameandtype CEQ exprsingle
    ;

letsequencebinding
    : DOLLAR OP varnameandtype CP typedeclaration? CEQ exprsingle
    ;

letarraybinding
    : DOLLAR OB varnameandtype CB typedeclaration? CEQ exprsingle
    ;

letmapbinding
    : DOLLAR OC varnameandtype CC typedeclaration? CEQ exprsingle
    ;

// [53] WindowClause
windowclause
    : KW_FOR (KW_TUMBLING | KW_SLIDING) KW_WINDOW varnameandtype KW_IN exprsingle
      windowstartcondition windowendcondition?
    ;

// [55] WindowStartCondition
windowstartcondition
    : KW_START windowvars KW_WHEN exprsingle
    ;

// [56] WindowEndCondition
windowendcondition
    : KW_ONLY? KW_END windowvars KW_WHEN exprsingle
    ;

// [57] WindowVars
windowvars
    : currentvar? (KW_AT DOLLAR positionalvarname)? (KW_PREVIOUS DOLLAR previousvar)? (KW_NEXT DOLLAR nextvar)?
    ;

currentvar
    : DOLLAR eqname
    ;

positionalvarname
    : eqname
    ;

previousvar
    : eqname
    ;

nextvar
    : eqname
    ;

// [58] CountClause
countclause
    : KW_COUNT DOLLAR eqname
    ;

// [59] WhereClause
whereclause
    : KW_WHERE exprsingle
    ;

// [60] GroupByClause
groupbyclause
    : KW_GROUP KW_BY groupingspec (COMMA groupingspec)*
    ;

// [61] GroupingSpec
groupingspec
    : (varnameandtype (CEQ exprsingle)? | exprsingle) (KW_COLLATION uriliteral)?
    ;

// [62] OrderByClause
orderbyclause
    : (KW_ORDER KW_BY | KW_STABLE KW_ORDER KW_BY) orderspec (COMMA orderspec)*
    ;

// [63] OrderSpec
orderspec
    : exprsingle ordermodifier
    ;

// [64] OrderModifier
ordermodifier
    : (KW_ASCENDING | KW_DESCENDING)?
      (KW_EMPTY (KW_GREATEST | KW_LEAST))?
      (KW_COLLATION uriliteral)?
    ;

// WhileClause (new in XQuery 4.0)
whileclause
    : KW_WHILE OP exprsingle CP
    ;

// TraceClause (new in XQuery 4.0): trace($label, $expr) or trace($expr)
traceclause
    : KW_TRACE OP exprsingle (COMMA exprsingle)? CP
    ;

// ============================================================
// A.6 Switch / Typeswitch expressions
// ============================================================

// [65] SwitchExpr
switchexpr
    : KW_SWITCH OP expr CP switchcaseclause+ KW_DEFAULT KW_RETURN exprsingle
    ;

// [67] SwitchCaseClause
switchcaseclause
    : (KW_CASE switchcaseoperand)+ KW_RETURN exprsingle
    ;

// [68] SwitchCaseOperand
switchcaseoperand
    : exprsingle
    ;

// [69] TypeswitchExpr
typeswitchexpr
    : KW_TYPESWITCH OP expr CP caseclause+ KW_DEFAULT (DOLLAR eqname)? KW_RETURN exprsingle
    ;

// [71] CaseClause
caseclause
    : KW_CASE (DOLLAR eqname KW_AS)? sequencetypeunion KW_RETURN exprsingle
    ;

// [72] SequenceTypeUnion
sequencetypeunion
    : sequencetype (P sequencetype)*
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

// [76] FinallyClause (new in XQuery 4.0)
finallyclause
    : KW_FINALLY enclosedexpr
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

ifexpr
    : KW_IF OP expr CP (unbracedactions | bracedaction)
    ;

unbracedactions
    : KW_THEN exprsingle KW_ELSE exprsingle
    ;

bracedaction
    : enclosedexpr
    ;

orexpr
    : andexpr (KW_OR andexpr)*
    ;

andexpr
    : comparisonexpr (KW_AND comparisonexpr)*
    ;

comparisonexpr
    : otherwiseexpr ((valuecomp | generalcomp | nodecomp) otherwiseexpr)?
    ;

otherwiseexpr
    : stringconcatexpr (KW_OTHERWISE stringconcatexpr)*
    ;

stringconcatexpr
    : rangeexpr (PP rangeexpr)*
    ;

rangeexpr
    : additiveexpr (KW_TO additiveexpr)?
    ;

additiveexpr
    : multiplicativeexpr ((PLUS | MINUS) multiplicativeexpr)*
    ;

multiplicativeexpr
    : unionexpr ((STAR | TIMES_SIGN | KW_DIV | DIV_SIGN | KW_IDIV | KW_MOD) unionexpr)*
    ;

unionexpr
    : intersectexceptexpr ((KW_UNION | P) intersectexceptexpr)*
    ;

intersectexceptexpr
    : recordputexpr ((KW_INTERSECT | KW_EXCEPT) recordputexpr)*
    ;

// RecordPutExpr: new in XPath/XQuery 4.0
recordputexpr
    : instanceofexpr (PLUS_CEQ instanceofexpr)*
    ;

instanceofexpr
    : treatexpr (KW_INSTANCE KW_OF sequencetype)?
    ;

treatexpr
    : castableexpr (KW_TREAT KW_AS sequencetype)?
    ;

castableexpr
    : castexpr (KW_CASTABLE KW_AS casttarget occurrenceindicator?)?
    ;

castexpr
    : pipelineexpr (KW_CAST KW_AS casttarget occurrenceindicator?)?
    ;

pipelineexpr
    : arrowexpr
    ;

arrowexpr
    : unaryexpr (sequencearrowtarget | mappingarrowtarget)*
    ;

sequencearrowtarget
    : EG arrowtarget
    ;

mappingarrowtarget
    : MAPPING_ARROW arrowtarget
    ;

arrowtarget
    : functioncall
    | restricteddynamiccall
    ;

restricteddynamiccall
    : (varref | parenthesizedexpr | functionitemexpr | mapconstructor | arrayconstructor) positionalargumentlist
    ;

unaryexpr
    : (MINUS | PLUS)* valueexpr
    ;

// [ValueExpr] XQuery extends XPath with ValidateExpr and ExtensionExpr
valueexpr
    : validateexpr
    | extensionexpr
    | simplemapexpr
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

// [ExtensionExpr]
extensionexpr
    : Pragma+ enclosedexpr
    ;

generalcomp
    : EQ | NE | LT | LE | GT | GE
    ;

valuecomp
    : KW_EQ | KW_NE | KW_LT | KW_LE | KW_GT | KW_GE
    ;

nodecomp
    : KW_IS
    | KW_IS_NOT
    | nodeprecedes
    | nodefollows
    | KW_PRECEDES_OR_IS
    | KW_FOLLOWS_OR_IS
    ;

nodeprecedes
    : LL | KW_PRECEDES
    ;

nodefollows
    : GG | KW_FOLLOWS
    ;

simplemapexpr
    : pathexpr (BANG pathexpr)*
    ;

// ============================================================
// A.9 Path Expressions
// ============================================================

pathexpr
    : absolutepathexpr
    | relativepathexpr
    ;

absolutepathexpr
    : SLASH relativepathexpr?
    | SS relativepathexpr
    ;

relativepathexpr
    : stepexpr ((SLASH | SS) stepexpr)*
    ;

stepexpr
    : postfixexpr
    | axisstep
    ;

axisstep
    : (abbreviatedstep | fullstep) (predicate | lookup)*
    ;

abbreviatedstep
    : DD
    | AT nodetest
    | simplenodetest
    ;

fullstep
    : axis nodetest
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

nodetest
    : unionnodetest
    | simplenodetest
    | dynamicnodetest
    ;

unionnodetest
    : OP simplenodetest (P simplenodetest)+ CP
    ;

simplenodetest
    : typetest
    | selector
    ;

typetest
    : gnodetype
    | xnodetype
    | jnodetype
    ;

selector
    : eqname
    | wildcard
    ;

dynamicnodetest
    : enclosedexpr
    ;

// ============================================================
// A.10 Postfix / Primary Expressions
// ============================================================

postfixexpr
    : primaryexpr (predicate | positionalargumentlist | lookup | (METHOD_ARROW QName positionalargumentlist))*
    ;

positionalargumentlist
    : OP positionalarguments? CP
    ;

positionalarguments
    : argument (COMMA argument)*
    ;

predicatelist
    : predicate*
    ;

predicate
    : OB expr CB
    ;

lookup
    : QM keyspecifier
    ;

keyspecifier
    : QName
    | literal
    | contextvalueref
    | varref
    | parenthesizedexpr
    | lookupwildcard
    ;

lookupwildcard
    : STAR
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

orderedexpr
    : KW_ORDERED enclosedexpr
    ;

unorderedexpr
    : KW_UNORDERED enclosedexpr
    ;

literal
    : numericliteral
    | StringLiteral
    ;

numericliteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

varref
    : DOLLAR eqname
    ;

parenthesizedexpr
    : OP expr? CP
    ;

contextvalueref
    : D
    ;

functioncall
    : { this.IsFuncCall() }? eqname argumentlist
    ;

argumentlist
    : OP ((positionalarguments (COMMA keywordarguments)?) | keywordarguments)? CP
    ;

keywordarguments
    : keywordargument (COMMA keywordargument)*
    ;

keywordargument
    : eqname CEQ argument
    ;

argument
    : exprsingle
    | argumentplaceholder
    ;

argumentplaceholder
    : QM
    ;

// ============================================================
// A.11 Function Item Expressions
// ============================================================

functionitemexpr
    : namedfunctionref
    | inlinefunctionexpr
    ;

namedfunctionref
    : eqname POUND IntegerLiteral
    ;

inlinefunctionexpr
    : (KW_FUNCTION | KW_FN) functionsignature functionbody
    ;

// ============================================================
// A.12 Node Constructors
// ============================================================

nodeConstructor
    : directconstructor
    | computedconstructor
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
    : OPEN_TAG QName dirattrlist (
        ET_SLASH_GT
        | ET_GT dircontent* EC_CLOSE_TAG QName CT_GT
    )
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

quotattrcontentchar
    : QuotAttrContentChar
    | EscapeQuot
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
    ;

aposattrcontentchar
    : AposAttrContentChar
    | EscapeApos
    | PredefinedEntityRef
    | CharRef
    | OC expr CC
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

cdsection
    : CDataSection
    ;

dircommentconstructor
    : DirCommentContents
    ;

dirpiconstructor
    : DirPIContents
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

compdocconstructor
    : KW_DOCUMENT enclosedexpr
    ;

compElemconstructor
    : KW_ELEMENT compnodename enclosedcontentexpr
    ;

compAttrconstructor
    : KW_ATTRIBUTE compnodename enclosedexpr
    ;

compNSconstructor
    : KW_NAMESPACE compnodencname enclosedexpr
    ;

comptextconstructor
    : KW_TEXT enclosedexpr
    ;

compCommentconstructor
    : KW_COMMENT enclosedexpr
    ;

compPIconstructor
    : KW_PROCESSING_INSTRUCTION compnodencname enclosedexpr
    ;

compnodename
    : qnameliteral
    | OC expr CC
    ;

compnodencname
    : markedncname
    | OC expr CC
    ;

markedncname
    : POUND QName
    ;

qnameliteral
    : POUND eqname
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

arrayconstructor
    : squarearrayconstructor
    | curlyarrayconstructor
    ;

squarearrayconstructor
    : OB (exprsingle (COMMA exprsingle)*)? CB
    ;

curlyarrayconstructor
    : KW_ARRAY enclosedexpr
    ;

stringtemplate
    : StringTemplate
    ;

unarylookup
    : QM keyspecifier
    ;

// ============================================================
// A.14 Type Declarations and Sequence Types
// ============================================================

typedeclaration
    : KW_AS sequencetype
    ;

sequencetype
    : KW_EMPTY_SEQUENCE OP CP
    | itemtype occurrenceindicator?
    ;

occurrenceindicator
    : QM | STAR | PLUS
    ;

itemtype
    : regularitemtype
    | functiontype
    | typename_
    | choiceitemtype
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

anyitemtype
    : KW_ITEM OP CP
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

anyxnodetype
    : KW_NODE OP CP
    ;

documentnodetype
    : KW_DOCUMENT_NODE OP (elementnodetype | schemaelementnodetype | nametestunion)? CP
    ;

elementnodetype
    : KW_ELEMENT OP (nametestunion (COMMA typename_ QM?)?)? CP
    ;

attributenodetype
    : KW_ATTRIBUTE OP (nametestunion (COMMA typename_)?)? CP
    ;

schemaelementnodetype
    : KW_SCHEMA_ELEMENT OP elementname CP
    ;

schemaattributenodetype
    : KW_SCHEMA_ATTRIBUTE OP attributename CP
    ;

processinginstructionnodetype
    : KW_PROCESSING_INSTRUCTION OP (QName | StringLiteral)? CP
    ;

commentnodetype
    : KW_COMMENT OP CP
    ;

textnodetype
    : KW_TEXT OP CP
    ;

namespacenodetype
    : KW_NAMESPACE_NODE OP CP
    ;

nametestunion
    : nametest
    ;

nametest
    : eqname
    | wildcard
    ;

gnodetype
    : KW_GNODE OP CP
    ;

jnodetype
    : KW_JNODE OP (STAR | jrootselector | QName | constant) (COMMA sequencetype)? CP
    ;

jrootselector
    : OP CP
    ;

constant
    : StringLiteral
    | MINUS numericliteral
    | qnameliteral
    | eqname OP CP
    ;

maptype
    : anymaptype
    | typedmaptype
    ;

anymaptype
    : KW_MAP OP STAR CP
    ;

typedmaptype
    : KW_MAP OP itemtype COMMA sequencetype CP
    ;

arraytype
    : anyarraytype
    | typedarraytype
    ;

anyarraytype
    : KW_ARRAY OP STAR CP
    ;

typedarraytype
    : KW_ARRAY OP sequencetype CP
    ;

recordtype
    : anyrecordtype
    | typedrecordtype
    ;

anyrecordtype
    : KW_RECORD OP STAR CP
    ;

typedrecordtype
    : KW_RECORD OP fielddeclarationlist CP
    ;

fielddeclarationlist
    : fielddeclaration (COMMA fielddeclaration)* (COMMA extendedfielddeclaration)?
    | extendedfielddeclaration
    ;

fielddeclaration
    : fieldname QM? (KW_AS sequencetype)?
    ;

// "..." means extensible record (new in XQuery 4.0)
extendedfielddeclaration
    : DD
    ;

fieldname
    : QName
    | StringLiteral
    ;

enumerationtype
    : KW_ENUM OP StringLiteral (COMMA StringLiteral)* CP
    ;

functiontype
    : anyfunctiontype
    | typedfunctiontype
    ;

anyfunctiontype
    : (KW_FUNCTION | KW_FN) OP STAR CP
    ;

typedfunctiontype
    : (KW_FUNCTION | KW_FN) OP typedfunctionparamlist? CP KW_AS sequencetype
    ;

typedfunctionparamlist
    : typedfunctionparam (COMMA typedfunctionparam)*
    ;

typedfunctionparam
    : (DOLLAR eqname KW_AS)? sequencetype
    ;

casttarget
    : typename_
    | choiceitemtype
    | enumerationtype
    | typedarraytype
    | typedmaptype
    | typedrecordtype
    ;

choiceitemtype
    : OP itemtype (P itemtype)* CP
    ;

typename_
    : eqname
    ;

simpletypename
    : typename_
    ;

wildcard
    : STAR
    | QName CS
    | SC QName
    | BracedURILiteral STAR
    ;

attributename
    : eqname
    ;

elementname
    : eqname
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
// Entry points for testing
// ============================================================

// Entry point for Maven antlr4test-maven-plugin: semicolon-separated queries/modules
auxilary
    : (module_ SEMI?)+ EOF
    ;
