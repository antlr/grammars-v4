/*
BSD License
Copyright (c) 2020, Evgeniy Slobodkin
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

parser grammar HaskellParser;

options { tokenVocab=HaskellLexer; }

module :  OCURLY? semi* pragmas? semi* (module_content | body) CCURLY? semi? EOF;

module_content
    :
    'module' modid exports? where_module
    ;

where_module
    :
    'where' module_body
    ;

module_body
    :
    open_ body close semi*
    ;

pragmas
    :
    pragma+
    ;

pragma
    :
    language_pragma
    | options_ghc
    | simple_options
    ;

language_pragma
    :
    '{-#' 'LANGUAGE'  extension_ (',' extension_)* '#-}' semi?
    ;

options_ghc
    :
    '{-#' 'OPTIONS_GHC' ('-' (varid | conid))* '#-}' semi?
    ;

simple_options
    :
    '{-#' 'OPTIONS' ('-' (varid | conid))* '#-}' semi?
    ;

extension_
    :
    CONID
    ;

body
    :
    (impdecls topdecls)
    | impdecls
    | topdecls
    ;

impdecls
    :
    (impdecl | NEWLINE | semi)+
    ;

exports
    :
    '(' (exprt (',' exprt)*)? ','? ')'
    ;

exprt
    :
    qvar
    | ( qtycon ( ('(' '..' ')') | ('(' (cname (',' cname)*)? ')') )? )
    | ( qtycls ( ('(' '..' ')') | ('(' (qvar (',' qvar)*)? ')') )? )
    | ( 'module' modid )
    ;

impdecl
    :
    'import' 'qualified'? modid ('as' modid)? impspec? semi+
    ;

impspec
    :
    ('(' (himport (',' himport)* ','?)? ')')
    | ( 'hiding' '(' (himport (',' himport)* ','?)? ')' )
    ;

himport
    :
    var_
    | ( tycon ( ('(' '..' ')') | ('(' (cname (',' cname)*)? ')') )? )
    | ( tycls ( ('(' '..' ')') | ('(' sig_vars? ')') )? )
    ;

cname
    :
    var_ | con
    ;

// -------------------------------------------
// Fixity Declarations

fixity: 'infix' | 'infixl' | 'infixr';

ops : op (',' op)*;

// -------------------------------------------
// Top-Level Declarations
topdecls : (topdecl semi+| NEWLINE | semi)+;

topdecl
    :
    cl_decl
    | ty_decl
    // Check KindSignatures
    | standalone_kind_sig
    | inst_decl
    | standalone_deriving
    | role_annot
    | ('default' '(' comma_types? ')' )
    | ('foreign' fdecl)
    | ('{-#' 'DEPRECATED' deprecations? '#-}')
    | ('{-#' 'WARNING' warnings? '#-}')
    | ('{-#' 'RULES' rules? '#-}')
    | annotation
    | decl_no_th
    // -- Template Haskell Extension
    //  The $(..) form is one possible form of infixexp
    //  but we treat an arbitrary expression just as if
    //  it had a $(..) wrapped around it
    | infixexp
    ;

// Type classes
//
cl_decl
    :
    'class' tycl_hdr fds? where_cls?
    ;

// Type declarations (toplevel)
//
ty_decl
    :
    // ordinary type synonyms
    'type' type_ '=' ktypedoc
    // type family declarations
    | 'type' 'family' type_ opt_tyfam_kind_sig? opt_injective_info? where_type_family?
    // ordinary data type or newtype declaration
    | 'data' capi_ctype? tycl_hdr constrs derivings?
    | 'newtype' capi_ctype? tycl_hdr constrs derivings?
    // ordinary GADT declaration
    | 'data' capi_ctype? tycl_hdr opt_kind_sig? gadt_constrlist? derivings?
    | 'newtype' capi_ctype? tycl_hdr opt_kind_sig? gadt_constrlist? derivings?
    // data/newtype family
    | 'data' 'family' type_ opt_datafam_kind_sig?
    ;

// standalone kind signature

standalone_kind_sig
    :
    'type' sks_vars '::' ktypedoc
    ;

// See also: sig_vars
sks_vars
    :
    oqtycon (',' oqtycon)*
    ;

inst_decl
    :
    ('instance' overlap_pragma? inst_type where_inst?)
    | ('type' 'instance' ty_fam_inst_eqn)
    // 'constrs' in the end of this rules in GHC
    // This parser no use docs
    | ('data' 'instance' capi_ctype? tycl_hdr_inst derivings?)
    | ('newtype' 'instance' capi_ctype? tycl_hdr_inst derivings?)
    // For GADT
    | ('data' 'instance' capi_ctype? tycl_hdr_inst opt_kind_sig? gadt_constrlist? derivings?)
    | ('newtype' 'instance' capi_ctype? tycl_hdr_inst opt_kind_sig? gadt_constrlist? derivings?)
    ;

overlap_pragma
    :
      '{-#' 'OVERLAPPABLE' '#-}'
    | '{-#' 'OVERLAPPING' '#-}'
    | '{-#' 'OVERLAPS' '#-}'
    | '{-#' 'INCOHERENT' '#-}'
    ;


deriv_strategy_no_via
    :
      'stock'
    | 'anyclass'
    | 'newtype'
    ;

deriv_strategy_via
    :
    'via' ktype
    ;

deriv_standalone_strategy
    :
      'stock'
    | 'anyclass'
    | 'newtype'
    | deriv_strategy_via
    ;

// Injective type families

opt_injective_info
    :
    '|' injectivity_cond
    ;

injectivity_cond
    :
    // but in GHC new tyvarid rule
    tyvarid '->' inj_varids
    ;

inj_varids
    :
    tyvarid+
    ;

// Closed type families

where_type_family
    :
    'where' ty_fam_inst_eqn_list
    ;

ty_fam_inst_eqn_list
    :
    (open_ ty_fam_inst_eqns? close)
    | ('{' '..' '}')
    | (open_ '..' close)
    ;

ty_fam_inst_eqns
    :
    ty_fam_inst_eqn (semi+ ty_fam_inst_eqn)* semi*
    ;

ty_fam_inst_eqn
    :
    'forall' tv_bndrs? '.' type_ '=' ktype
    | type_ '=' ktype
    ;

//  Associated type family declarations

//  * They have a different syntax than on the toplevel (no family special
//    identifier).

//  * They also need to be separate from instances; otherwise, data family
//    declarations without a kind signature cause parsing conflicts with empty
//    data declarations.

at_decl_cls
    :
    ('data' 'family'? type_ opt_datafam_kind_sig?)
    | ('type' 'family'? type_ opt_at_kind_inj_sig?)
    | ('type' 'instance'? ty_fam_inst_eqn)
    ;

// Associated type instances
//
at_decl_inst
    :
    // type instance declarations, with optional 'instance' keyword
    ('type' 'instance'? ty_fam_inst_eqn)
    // data/newtype instance declaration, with optional 'instance' keyword
    | ('data' 'instance'? capi_ctype? tycl_hdr_inst constrs derivings?)
    | ('newtype' 'instance'? capi_ctype? tycl_hdr_inst constrs derivings?)
    // GADT instance declaration, with optional 'instance' keyword
    | ('data' 'instance'? capi_ctype? tycl_hdr_inst opt_kind_sig? gadt_constrlist? derivings?)
    | ('newtype' 'instance'? capi_ctype? tycl_hdr_inst opt_kind_sig? gadt_constrlist? derivings?)
    ;

// Family result/return kind signatures

opt_kind_sig
    :
    '::' kind
    ;

opt_datafam_kind_sig
    :
    '::' kind
    ;

opt_tyfam_kind_sig
    :
    ('::' kind)
    | ('=' tv_bndr)
    ;

opt_at_kind_inj_sig
    :
    ('::' kind)
    | ('=' tv_bndr_no_braces '|' injectivity_cond)
    ;

tycl_hdr
    :
    (tycl_context '=>' type_)
    | type_
    ;

tycl_hdr_inst
    :
    ('forall' tv_bndrs? '.' tycl_context '=>' type_)
    | ('forall' tv_bndrs? '.' type_)
    | (tycl_context '=>' type_)
    | type_
    ;

capi_ctype
    :
    ('{-#' 'CTYPE' STRING STRING '#-}')
    | ('{-#' 'CTYPE' STRING '#-}')
    ;

// -------------------------------------------
// Stand-alone deriving

standalone_deriving
    :
    'deriving' deriv_standalone_strategy? 'instance' overlap_pragma? inst_type
    ;

// -------------------------------------------
// Role annotations

role_annot
    :
    'type' 'role' oqtycon roles?
    ;

roles
    :
    role+
    ;

role
    :
    varid | '_'
    ;


// -------------------------------------------
// Pattern synonyms
pattern_synonym_decl
    :
    ('pattern' pattern_synonym_lhs '=' pat)
    | ('pattern' pattern_synonym_lhs '<-' pat where_decls?)
    ;

pattern_synonym_lhs
    :
    (con vars_?)
    | (varid conop varid)
    | (con '{' cvars '}')
    ;

vars_
    :
    varid+
    ;

cvars
    :
    var_ (',' var_)*
    ;

where_decls
    :
    'where' open_ decls? close
    ;

pattern_synonym_sig
    :
    'pattern' con_list '::' sigtypedoc
    ;

// -------------------------------------------
// Nested declaration

// Declaration in class bodies

decl_cls
    :
    at_decl_cls
    | decl
    | 'default' infixexp '::' sigtypedoc
    ;

decls_cls
    :
    decl_cls (semi+ decl_cls)* semi*
    ;

decllist_cls
    :
    open_ decls_cls? close
    ;

// Class body
//
where_cls
    :
    'where' decllist_cls
    ;

// Declarations in instance bodies
//
decl_inst
    :
    at_decl_inst
    | decl
    ;

decls_inst
    :
    decl_inst (semi+ decl_inst)* semi*
    ;

decllist_inst
    :
    open_ decls_inst? close
    ;

// Instance body
//
where_inst
    :
    'where' decllist_inst
    ;

// Declarations in binding groups other than classes and instances
//
decls
    :
    decl (semi+ decl)* semi*
    ;

decllist
    :
    open_ decls? close
    ;

// Binding groups other than those of class and instance declarations
//
binds
    :
    decllist
    | (open_ dbinds? close)
    ;

wherebinds
    :
    'where' binds
    ;


// -------------------------------------------
// Transformation Rules

rules
    :
    pragma_rule (semi pragma_rule)* semi?
    ;

pragma_rule
    :
    pstring rule_activation? rule_foralls? infixexp '=' exp
    ;

rule_activation_marker
    :
    '~' | varsym
    ;

rule_activation
    :
    ('[' integer ']')
    | ('[' rule_activation_marker integer ']')
    | ('[' rule_activation_marker ']')
    ;

rule_foralls
    :
    ('forall' rule_vars? '.' ('forall' rule_vars? '.')?)
    ;

rule_vars
    :
    rule_var+
    ;

rule_var
    :
    varid
    | ('(' varid '::' ctype ')')
    ;

// -------------------------------------------
// Warnings and deprecations (c.f. rules)

warnings
    :
    pragma_warning (semi pragma_warning)* semi?
    ;

pragma_warning
    :
    namelist strings
    ;

deprecations
    :
    pragma_deprecation (semi pragma_deprecation)* semi?
    ;

pragma_deprecation
    :
    namelist strings
    ;

strings
    :
    pstring
    | ('[' stringlist? ']')
    ;

stringlist
    :
    pstring (',' pstring)*
    ;

// -------------------------------------------
// Annotations

annotation
    :
      ('{-#' 'ANN' name_var aexp '#-}')
    | ('{-#' 'ANN' tycon aexp '#-}')
    | ('{-#' 'ANN' 'module' aexp '#-}')
    ;

// -------------------------------------------
// Foreign import and export declarations

fdecl
    :
    ('import' callconv safety? fspec)
    | ('export' callconv fspec)
    ;

callconv
    :
    'ccall' | 'stdcall' | 'cplusplus' | 'javascript'
    ;

safety : 'unsafe' | 'safe' | 'interruptible';

fspec
    :
    pstring? var_ '::' sigtypedoc
    ;

// -------------------------------------------
// Type signatures

opt_sig : '::' sigtype;

opt_tyconsig : '::' gtycon;

sigtype
    :
    ctype
    ;

sigtypedoc
    :
    ctypedoc
    ;

sig_vars
    :
    var_ (',' var_)*
    ;

sigtypes1
    :
    sigtype (',' sigtype)*
    ;

// -------------------------------------------
// Types

unpackedness
    :
      ('{-#' 'UNPACK'   '#-}')
    | ('{-#' 'NOUNPACK' '#-}')
    ;

forall_vis_flag
    :
    '.'
    | '->'
    ;

// A ktype/ktypedoc is a ctype/ctypedoc, possibly with a kind annotation
ktype
    :
    ctype
    | (ctype '::' kind)
    ;

ktypedoc
    :
    ctypedoc
    | ctypedoc '::' kind
    ;

// A ctype is a for-all type
ctype
    :
    'forall' tv_bndrs? forall_vis_flag ctype
    | btype '=>' ctype
    | var_ '::' type_ // not sure about this rule
    | type_
    ;

// -- Note [ctype and ctypedoc]
// -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// -- It would have been nice to simplify the grammar by unifying `ctype` and
// -- ctypedoc` into one production, allowing comments on types everywhere (and
// -- rejecting them after parsing, where necessary).  This is however not possible
// -- since it leads to ambiguity. The reason is the support for comments on record
// -- fields:
// --         data R = R { field :: Int -- ^ comment on the field }
// -- If we allow comments on types here, it's not clear if the comment applies
// -- to 'field' or to 'Int'. So we must use `ctype` to describe the type.


ctypedoc
    :
    'forall' tv_bndrs? forall_vis_flag ctypedoc
    | tycl_context '=>' ctypedoc
    | var_ '::' type_
    | typedoc
    ;

// In GHC this rule is context
tycl_context
    :
    btype
    ;

// constr_context rule

// {- Note [GADT decl discards annotations]
// ~~~~~~~~~~~~~~~~~~~~~
// The type production for

//     btype `->`         ctypedoc
//     btype docprev `->` ctypedoc

// add the AnnRarrow annotation twice, in different places.

// This is because if the type is processed as usual, it belongs on the annotations
// for the type as a whole.

// But if the type is passed to mkGadtDecl, it discards the top level SrcSpan, and
// the top-level annotation will be disconnected. Hence for this specific case it
// is connected to the first type too.
// -}

constr_context
    :
    constr_btype
    ;

// {- Note [GADT decl discards annotations]
// ~~~~~~~~~~~~~~~~~~~~~
// The type production for

//     btype `->`         ctypedoc
//     btype docprev `->` ctypedoc

// add the AnnRarrow annotation twice, in different places.

// This is because if the type is processed as usual, it belongs on the annotations
// for the type as a whole.

// But if the type is passed to mkGadtDecl, it discards the top level SrcSpan, and
// the top-level annotation will be disconnected. Hence for this specific case it
// is connected to the first type too.
// -}

type_
    :
    btype
    | btype '->' ctype
    ;

typedoc
    :
    btype
    | btype '->' ctypedoc
    ;

constr_btype
    :
    constr_tyapps
    ;

constr_tyapps
    :
    constr_tyapp+
    ;

constr_tyapp
    :
    tyapp
    ;

btype
    :
    tyapps
    ;

tyapps
    :
    tyapp+
    ;

tyapp
    :
    atype
    | ('@' atype)
    | qtyconop
    | tyvarop
    | ('\'' qconop)
    | ('\'' varop)
    | unpackedness
    ;

atype
    :
    ntgtycon
    | tyvar
    | '*'
    | ('~' atype)
    | ('!' atype)
    | ('{' fielddecls? '}')
    | ('(' ')')
    | ('(' ktype ',' comma_types ')')
    | ('(#' '#)')
    | ('(#' comma_types '#)')
    | ('(#' bar_types2 '#)')
    | ('[' ktype ']')
    | ('(' ktype ')')
    | quasiquote
    | splice_untyped
    | ('\'' qcon_nowiredlist)
    | ('\'' '(' ktype ',' comma_types ')')
    | ('\'' '[' comma_types? ']')
    | ('\'' var_)
    // Two or more [ty, ty, ty] must be a promoted list type, just as
    // if you had written '[ty, ty, ty]
    // (One means a list type, zero means the list type constructor,
    // so you have to quote those.)
    | ('[' ktype ',' comma_types ']')
    | integer
    | pstring
    | '_'
    ;

inst_type
    :
    sigtype
    ;

deriv_types
    :
    ktypedoc (',' ktypedoc)*
    ;

comma_types
    :
    ktype (',' ktype)*
    ;

bar_types2
    :
    ktype '|' ktype ('|' ktype)*
    ;

tv_bndrs
    :
    tv_bndr+
    ;

tv_bndr
    :
    tv_bndr_no_braces
    | ('{' tyvar '}')
    | ('{' tyvar '::' kind '}')
    ;

tv_bndr_no_braces
    :
    tyvar
    | ('(' tyvar '::' kind ')')
    ;

fds
    :
    '|' fds1
    ;

fds1
    :
    fd (',' fd)*
    ;

fd
    :
    varids0? '->' varids0?
    ;

varids0
    :
    tyvar+
    ;


// -------------------------------------------
// Kinds

kind
    :
    ctype
    ;

// {- Note [Promotion]
//    ~~~~~~~~~~~~~~~~

// - Syntax of promoted qualified names
// We write 'Nat.Zero instead of Nat.'Zero when dealing with qualified
// names. Moreover ticks are only allowed in types, not in kinds, for a
// few reasons:
//   1. we don't need quotes since we cannot define names in kinds
//   2. if one day we merge types and kinds, tick would mean look in DataName
//   3. we don't have a kind namespace anyway

// - Name resolution
// When the user write Zero instead of 'Zero in types, we parse it a
// HsTyVar ("Zero", TcClsName) instead of HsTyVar ("Zero", DataName). We
// deal with this in the renamer. If a HsTyVar ("Zero", TcClsName) is not
// bounded in the type level, then we look for it in the term level (we
// change its namespace to DataName, see Note [Demotion] in GHC.Types.Names.OccName).
// And both become a HsTyVar ("Zero", DataName) after the renamer.

// -}

// -------------------------------------------
// Datatype declarations

gadt_constrlist
    :
    'where' open_ gadt_constrs? semi* close
    ;

gadt_constrs
    :
    gadt_constr_with_doc (semi gadt_constr_with_doc)*
    ;

// We allow the following forms:
//      C :: Eq a => a -> T a
//      C :: forall a. Eq a => !a -> T a
//      D { x,y :: a } :: T a
//      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr_with_doc
    :
    gadt_constr
    ;

gadt_constr
    :
    con_list '::' sigtypedoc
    ;


// {- Note [Difference in parsing GADT and data constructors]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GADT constructors have simpler syntax than usual data constructors:
// in GADTs, types cannot occur to the left of '::', so they cannot be mixed
// with constructor names (see Note [Parsing data constructors is hard]).

// Due to simplified syntax, GADT constructor names (left-hand side of '::')
// use simpler grammar production than usual data constructor names. As a
// consequence, GADT constructor names are restricted (names like '(*)' are
// allowed in usual data constructors, but not in GADTs).
// -}

// NOT AS IN GHC
// constrs
//     :
//     constr ('|' constr)*
//     ;

constrs
    :
    '=' constrs1
    ;

constrs1
    :
    constr ('|' constr)*
    ;

// {- Note [Constr variations of non-terminals]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// In record declarations we assume that 'ctype' used to parse the type will not
// consume the trailing docprev:

//   data R = R { field :: Int -- ^ comment on the field }

// In 'R' we expect the comment to apply to the entire field, not to 'Int'. The
// same issue is detailed in Note [ctype and ctypedoc].

// So, we do not want 'ctype'  to consume 'docprev', therefore
//     we do not want 'btype'  to consume 'docprev', therefore
//     we do not want 'tyapps' to consume 'docprev'.

// At the same time, when parsing a 'constr', we do want to consume 'docprev':

//   data T = C Int  -- ^ comment on Int
//              Bool -- ^ comment on Bool

// So, we do want 'constr_stuff' to consume 'docprev'.

// The problem arises because the clauses in 'constr' have the following
// structure:

//   (a)  context '=>' constr_stuff   (e.g.  data T a = Ord a => C a)
//   (b)               constr_stuff   (e.g.  data T a =          C a)

// and to avoid a reduce/reduce conflict, 'context' and 'constr_stuff' must be
// compatible. And for 'context' to be compatible with 'constr_stuff', it must
// consume 'docprev'.

// So, we want 'context'  to consume 'docprev', therefore
//     we want 'btype'    to consume 'docprev', therefore
//     we want 'tyapps'   to consume 'docprev'.

// Our requirements end up conflicting: for parsing record types, we want 'tyapps'
// to leave 'docprev' alone, but for parsing constructors, we want it to consume
// 'docprev'.

// As the result, we maintain two parallel hierarchies of non-terminals that
// either consume 'docprev' or not:

//   tyapps      constr_tyapps
//   btype       constr_btype
//   context     constr_context
//   ...

// They must be kept identical except for their treatment of 'docprev'.

// -}

// constr
//     :
//     (con ('!'? atype)*)
//     | ((btype | ('!' atype)) conop (btype | ('!' atype)))
//     | (con '{' fielddecls? '}')
//     ;

constr
    :
    forall? (constr_context '=>')? constr_stuff
    ;

forall
    :
    'forall' tv_bndrs? '.'
    ;

constr_stuff
    :
    constr_tyapps
    ;

fielddecls
    :
    fielddecl (',' fielddecl)*
    ;

fielddecl
    :
    sig_vars '::' ctype
    ;

// A list of one or more deriving clauses at the end of a datatype
derivings
    :
    deriving+
    ;

// The outer Located is just to allow the caller to
// know the rightmost extremity of the 'deriving' clause
deriving
    :
    ('deriving' deriv_clause_types)
    | ('deriving' deriv_strategy_no_via deriv_clause_types)
    | ('deriving' deriv_clause_types deriv_strategy_via)
    ;

deriv_clause_types
    :
    qtycon
    | '(' ')'
    | '(' deriv_types ')'
    ;

// -------------------------------------------
// Value definitions (CHECK!!!)

// {- Note [Declaration/signature overlap]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// There's an awkward overlap with a type signature.  Consider
//         f :: Int -> Int = ...rhs...
//    Then we can't tell whether it's a type signature or a value
//    definition with a result signature until we see the '='.
//    So we have to inline enough to postpone reductions until we know.
// -}

// {-
//   ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
//   instead of qvar, we get another shift/reduce-conflict. Consider the
//   following programs:

//      { (^^) :: Int->Int ; }          Type signature; only var allowed

//      { (^^) :: Int->Int = ... ; }    Value defn with result signature;
//                                      qvar allowed (because of instance decls)

//   We can't tell whether to reduce var to qvar until after we've read the signatures.
// -}

decl_no_th
    :
    sigdecl
    | (infixexp opt_sig? rhs)
    | pattern_synonym_decl
    // | docdecl
    | semi+
    ;

decl
    :
    decl_no_th

    // Why do we only allow naked declaration splices in top-level
    // declarations and not here? Short answer: because readFail009
    // fails terribly with a panic in cvBindsAndSigs otherwise.
    | splice_exp
    | semi+
    ;

rhs
    :
    ('=' exp wherebinds?)
    | (gdrhs wherebinds?);

gdrhs
    :
    gdrh+
    ;

gdrh
    :
    '|' guards '=' exp
    ;

sigdecl
    :
    (infixexp '::' sigtypedoc)
    | (var_ ',' sig_vars '::' sigtypedoc)
    | (fixity integer? ops)
    | (pattern_synonym_sig)
    | ('{-#' 'COMPLETE' con_list opt_tyconsig? '#-}')
    | ('{-#' 'INLINE' activation? qvar '#-}')
    | ('{-#' 'SCC' qvar pstring? '#-}')
    | ('{-#' 'SPECIALISE' activation? qvar '::' sigtypes1 '#-}')
    | ('{-#' 'SPECIALISE_INLINE' activation? qvar '::' sigtypes1 '#-}')
    | ('{-#' 'SPECIALISE' 'instance' inst_type '#-}')
    | ('{-#' 'MINIMAL' '#-}' name_boolformula_opt? '#-}')
    |(semi+)
    ;

activation
    :
    ('[' integer ']')
    | ('[' rule_activation_marker integer ']')
    ;

// -------------------------------------------
// Expressions

th_quasiquote
    :
    '[' varid '|'
    ;

th_qquasiquote
    :
    '[' qvarid '|'
    ;

quasiquote
    :
    th_quasiquote
    | th_qquasiquote
    ;

exp
    :
    (infixexp '::' sigtype)
    | (infixexp '-<' exp)
    | (infixexp '>-' exp)
    | (infixexp '-<<' exp)
    | (infixexp '>>-' exp)
    | infixexp
    ;

infixexp
    :
    exp10 (qop exp10p)*
    ;

exp10p
    :
    exp10
    ;

exp10
    :
    '-'? fexp
    ;

fexp
    :
    aexp+ ('@' atype)?
    ;

aexp
    :
    (qvar '@' aexp)
    | ('~' aexp)
    | ('!' aexp)
    | ('\\' apats '->' exp)
    | ('let' decllist 'in' exp)
    | (LCASE alts)
    | ('if' exp semi? 'then' exp semi? 'else' exp)
    | ('if' ifgdpats)
    | ('case' exp 'of' alts)
    | ('do' stmtlist)
    | ('mdo' stmtlist)
    | aexp1
    ;

aexp1
    :
    aexp2 ('{' fbinds? '}')*
    ;

aexp2
    :
    qvar
    | qcon
    | varid
    | literal
    | pstring
    | integer
    | pfloat
    // N.B.: sections get parsed by these next two productions.
    // This allows you to write, e.g., '(+ 3, 4 -)', which isn't
    // correct Haskell (you'd have to write '((+ 3), (4 -))')
    // but the less cluttered version fell out of having texps.
    | ('(' texp ')')
    | ('(' tup_exprs ')')
    | ('(#' texp '#)')
    | ('(#' tup_exprs '#)')
    | ('[' list_ ']')
    | '_'
    // Template Haskell
    | splice_untyped
    | splice_typed
    | ('\'' qvar)
    | ('\'' qcon)
    | ('\'\'' tyvar)
    | ('\'\'' gtycon)
    | '\'\''
    | '[|' exp '|]'
    | '[||' exp '||]'
    | '[t|' ktype '|]'
    | '[p|' infixexp '|]'
    | '[d|' cvtopbody '|]'
    | quasiquote
    | (AopenParen aexp cmdargs? AopenParen)
    ;

splice_exp
    :
    splice_typed
    | splice_untyped
    ;

splice_untyped
    :
    '$' aexp
    ;

splice_typed
    :
    '$$' aexp
    ;

cmdargs
    :
    acmd+
    ;

acmd
    :
    aexp
    ;

cvtopbody
    :
    open_ cvtopdecls0? close
    ;

cvtopdecls0
    :
    topdecls semi*
    ;

// -------------------------------------------
// Tuple expressions

texp
    :
    exp
    | (infixexp qop)
    | (qopm infixexp)
    | (exp '->' texp)
    ;

tup_exprs
    :
    (texp commas_tup_tail)
    | (texp bars)
    | (commas tup_tail?)
    | (bars texp bars?)
    ;

commas_tup_tail
    :
    commas tup_tail?
    ;

tup_tail
    :
    texp commas_tup_tail
    | texp
    ;

// -------------------------------------------
// List expressions

list_
    :
    texp
    | lexps
    | texp '..'
    | texp ',' exp '..'
    | texp '..' exp
    | texp ',' exp '..' exp
    | texp '|' flattenedpquals
    ;

lexps
    :
    texp ',' texp (',' texp)*
    ;


// -------------------------------------------
// List Comprehensions

flattenedpquals
    :
    pquals
    ;

pquals
    :
    squals ('|' squals)*
    ;

squals
    :
    transformqual (',' transformqual)*
    | transformqual (',' qual)*
    | qual (',' transformqual)*
    | qual (',' qual)*
    ;

transformqual
    :
    'then' exp
    | 'then' exp 'by' exp
    | 'then' 'group' 'using' exp
    | 'then' 'group' 'by' exp 'using' exp
    ;

// Note that 'group' is a special_id, which means that you can enable
// TransformListComp while still using Data.List.group. However, this
// introduces a shift/reduce conflict. Happy chooses to resolve the conflict
// in by choosing the "group by" variant, which is what we want.



// -------------------------------------------
// Guards (Different from GHC)

guards
    :
    guard_ (',' guard_)*
    ;

guard_
    :
    pat '<-' infixexp
    | 'let' decllist
    | infixexp
    ;

// -------------------------------------------
// Case alternatives

alts
    :
    (open_ (alt semi*)+ close)
    | (open_ close)
    ;

alt : pat alt_rhs ;

alt_rhs
    :
    ralt wherebinds?
    ;

ralt
    :
    ('->' exp)
    | gdpats
    ;


gdpats
    :
    gdpat+
    ;

// In ghc parser on GitLab second rule is 'gdpats close'
// Unclearly, is there possible errors with this implemmentation
ifgdpats
    :
    '{' gdpats '}'
    | gdpats
    ;

gdpat
    :
    '|' guards '->' exp
    ;

pat
    :
    exp
    ;

bindpat
    :
    exp
    ;

apat
    :
    aexp
    ;

apats
    :
    apat+
    ;

fpat
    :
    qvar '=' pat
    ;

// -------------------------------------------
// Statement sequences

stmtlist
    :
    open_ stmts? close
    ;

stmts
    :
    stmt (semi+ stmt)* semi*
    ;

stmt
    : qual
    | ('rec' stmtlist)
    | semi+
    ;


qual
    :
    bindpat '<-' exp
    | exp
    | 'let' binds
    ;

// -------------------------------------------
// Record Field Update/Construction

fbinds
    :
    (fbind (',' fbind)*)
    | ('..')
    ;

// In GHC 'texp', not 'exp'

// 1) RHS is a 'texp', allowing view patterns (#6038)
// and, incidentally, sections.  Eg
// f (R { x = show -> s }) = ...
//
// 2) In the punning case, use a place-holder
// The renamer fills in the final value
fbind
    :
    (qvar '=' exp)
    | qvar
    ;

// -------------------------------------------
// Implicit Parameter Bindings

dbinds
    :
    dbind (semi+ dbind) semi*
    ;

dbind
    :
    varid '=' exp
    ;

// -------------------------------------------

// Warnings and deprecations

name_boolformula_opt
    :
    name_boolformula_and ('|' name_boolformula_and)*
    ;

name_boolformula_and
    :
    name_boolformula_and_list
    ;

name_boolformula_and_list
    :
    name_boolformula_atom (',' name_boolformula_atom)*
    ;

name_boolformula_atom
    :
    ('(' name_boolformula_opt ')')
    | name_var
    ;

namelist
    :
    name_var (',' name_var)*
    ;

name_var
    :
    var_ | con
    ;

// -------------------------------------------
// Data constructors
// There are two different productions here as lifted list constructors
// are parsed differently.

qcon_nowiredlist : gen_qcon | sysdcon_nolist;

qcon  : gen_qcon | sysdcon;

gen_qcon : qconid | ( '(' qconsym ')' );

con    : conid   | ( '(' consym ')' ) | sysdcon;

con_list : con (',' con)*;

sysdcon_nolist
    :
    ('(' ')')
    | ('(' commas ')')
    | ('(#' '#)')
    | ('(#' commas '#)')
    ;

sysdcon
    :
    sysdcon_nolist
    | ('[' ']')
    ;

conop  : consym  | ('`' conid '`')	 ;

qconop : gconsym | ('`' qconid '`')	 ;

gconsym: ':'  	 | qconsym			 ;

// -------------------------------------------
// Type constructors (Be careful!!!)

gtycon
    :
    ntgtycon
    | ('('  ')')
    | ('(#' '#)')
    ;

ntgtycon
    :
    oqtycon
    | ('(' commas ')')
    | ('(#' commas '#)')
    | ('(' '->' ')')
    | ('[' ']')
    ;

oqtycon
    :
    qtycon
    | ('(' qtyconsym ')')
    ;

// {- Note [Type constructors in export list]
// ~~~~~~~~~~~~~~~~~~~~~
// Mixing type constructors and data constructors in export lists introduces
// ambiguity in grammar: e.g. (*) may be both a type constructor and a function.

// -XExplicitNamespaces allows to disambiguate by explicitly prefixing type
// constructors with 'type' keyword.

// This ambiguity causes reduce/reduce conflicts in parser, which are always
// resolved in favour of data constructors. To get rid of conflicts we demand
// that ambiguous type constructors (those, which are formed by the same
// productions as variable constructors) are always prefixed with 'type' keyword.
// Unambiguous type constructors may occur both with or without 'type' keyword.

// Note that in the parser we still parse data constructors as type
// constructors. As such, they still end up in the type constructor namespace
// until after renaming when we resolve the proper namespace for each exported
// child.
// -}

qtyconop: qtyconsym | ('`' qtycon '`');

qtycon : (modid '.')? tycon;

tycon : conid;

qtyconsym:qconsym | qvarsym | tyconsym;

tyconsym: consym | varsym | ':' | '-' | '.';

// -------------------------------------------
// Operators

op     : varop   | conop           ;

varop  : varsym  | ('`' varid '`') ;

qop    : qvarop  | qconop		   ;

qopm   : qvaropm | qconop | hole_op;

hole_op: '`' '_' '`';

qvarop : qvarsym | ('`' qvarid '`');

qvaropm: qvarsym_no_minus | ('`' qvarid '`');

// -------------------------------------------
// Type variables

tyvar : varid;

tyvarop: '`' tyvarid '`';

// Expand this rule later
// In GHC:
// tyvarid : VARID | special_id | 'unsafe'
//         | 'safe' | 'interruptible';

tyvarid : varid | special_id | 'unsafe' | 'safe' | 'interruptible';

tycls   : conid;

qtycls  : (modid '.')? tycls;

// -------------------------------------------
// Variables

var_	   : varid   | ( '(' varsym ')' );

qvar   : qvarid  | ( '(' qvarsym ')');

// We've inlined qvarsym here so that the decision about
// whether it's a qvar or a var can be postponed until
// *after* we see the close paren
qvarid : (modid '.')? varid;

// Note that 'role' and 'family' get lexed separately regardless of
// the use of extensions. However, because they are listed here,
// this is OK and they can be used as normal varids.
varid : (VARID | special_id) '#'*;

qvarsym: (modid '.')? varsym;

qvarsym_no_minus
    : varsym_no_minus
    | qvarsym
    ;

varsym : varsym_no_minus | '-';

varsym_no_minus : ascSymbol+;

// These special_ids are treated as keywords in various places,
// but as ordinary ids elsewhere.   'special_id' collects all these
// except 'unsafe', 'interruptible', 'forall', 'family', 'role', 'stock', and
// 'anyclass', whose treatment differs depending on context
special_id
    : 'as'
    | 'qualified'
    | 'hiding'
    | 'export'
    | 'stdcall'
    | 'ccall'
    | 'capi'
    | 'javascript'
    | 'stock'
    | 'anyclass'
    | 'via'
    ;

// -------------------------------------------
// Data constructors

qconid : (modid '.')? conid;

conid : CONID '#'*;

qconsym: (modid '.')? consym;

consym : ':' ascSymbol*;

// -------------------------------------------
// Literals

literal : integer | pfloat | pchar | pstring;

// -------------------------------------------
// Layout

open_ : VOCURLY | OCURLY;
close : VCCURLY | CCURLY;
semi : ';' | SEMI;

// -------------------------------------------
// Miscellaneous (mostly renamings)

modid : (conid '.')* conid;

commas: ','+;

bars : '|'+;

// -------------------------------------------

special : '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}';

symbol: ascSymbol;
ascSymbol: '!' | '#' | '$' | '%' | '&' | '*' | '+'
        | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '~' | ':' ;

integer
    :
    DECIMAL
    | OCTAL
    | HEXADECIMAL
    ;


pfloat : FLOAT;
pchar  : CHAR;
pstring: STRING;