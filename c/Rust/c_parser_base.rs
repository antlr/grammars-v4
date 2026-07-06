#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeClassification {
    Global,
    Block,
    Function,
    Variable,
    TypeSpecifier,
    StorageClassSpecifier,
    TypeQualifier,
    FunctionSpecifier,
    AlignmentSpecifier,
    AtomicTypeSpecifier,
    EnumSpecifier,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub classification: HashSet<TypeClassification>,
    pub predefined: bool,
    pub members: HashMap<String, Symbol>,
}

impl Symbol {
    fn new_kw(name: &str, classes: &[TypeClassification]) -> Self {
        Symbol {
            name: name.to_owned(),
            classification: classes.iter().cloned().collect(),
            predefined: true,
            members: HashMap::new(),
        }
    }

    fn new_scope(name: &str, class: TypeClassification) -> Self {
        Symbol {
            name: name.to_owned(),
            classification: std::iter::once(class).collect(),
            predefined: true,
            members: HashMap::new(),
        }
    }
}

pub struct SymbolTable {
    scope_stack: Vec<Symbol>,
    block_counter: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        use TypeClassification::*;
        let mut st = SymbolTable {
            scope_stack: Vec::new(),
            block_counter: 0,
        };
        st.scope_stack.push(Symbol::new_scope("global", Global));

        let kws: &[(&str, &[TypeClassification])] = &[
            ("auto",         &[StorageClassSpecifier]),
            ("constexpr",    &[StorageClassSpecifier]),
            ("extern",       &[StorageClassSpecifier]),
            ("register",     &[StorageClassSpecifier]),
            ("static",       &[StorageClassSpecifier]),
            ("thread_local", &[StorageClassSpecifier]),
            ("_Thread_local",&[StorageClassSpecifier]),
            ("typedef",      &[StorageClassSpecifier]),
            ("enum",         &[EnumSpecifier]),
            ("struct",       &[StorageClassSpecifier]),
            ("union",        &[StorageClassSpecifier]),
            ("const",        &[TypeQualifier]),
            ("restrict",     &[TypeQualifier]),
            ("__restrict__", &[TypeQualifier]),
            ("__restrict",   &[TypeQualifier]),
            ("volatile",     &[TypeQualifier]),
            ("__volatile__", &[TypeQualifier]),
            ("_Atomic",      &[TypeQualifier, AtomicTypeSpecifier]),
            ("void",         &[TypeSpecifier]),
            ("char",         &[TypeSpecifier]),
            ("short",        &[TypeSpecifier]),
            ("int",          &[TypeSpecifier]),
            ("long",         &[TypeSpecifier]),
            ("float",        &[TypeSpecifier]),
            ("double",       &[TypeSpecifier]),
            ("signed",       &[TypeSpecifier]),
            ("__signed__",   &[TypeSpecifier]),
            ("unsigned",     &[TypeSpecifier]),
            ("_BitInt",      &[TypeSpecifier]),
            ("bool",         &[TypeSpecifier]),
            ("_Bool",        &[TypeSpecifier]),
            ("_Complex",     &[TypeSpecifier]),
            ("_Decimal32",   &[TypeSpecifier]),
            ("_Decimal64",   &[TypeSpecifier]),
            ("_Decimal128",  &[TypeSpecifier]),
            ("__m128",       &[TypeSpecifier]),
            ("__m128d",      &[TypeSpecifier]),
            ("__m128i",      &[TypeSpecifier]),
            ("__extension__",&[TypeSpecifier]),
            ("__builtin_va_list",                     &[TypeSpecifier]),
            ("__builtin_has_attribute",               &[TypeSpecifier]),
            ("__builtin_speculation_safe_value",      &[TypeSpecifier]),
            ("__builtin_types_compatible_p",          &[TypeSpecifier]),
            ("__builtin_choose_expr",                 &[TypeSpecifier]),
            ("__builtin_tgmath",                      &[TypeSpecifier]),
            ("__builtin_constant_p",                  &[TypeSpecifier]),
            ("__builtin_is_constant_evaluated",       &[TypeSpecifier]),
            ("__builtin_bit_cast",                    &[TypeSpecifier]),
            ("__builtin_expect",                      &[TypeSpecifier]),
            ("__builtin_expect_with_probability",     &[TypeSpecifier]),
            ("__builtin_trap",                        &[TypeSpecifier]),
            ("__builtin_assoc_barrier",               &[TypeSpecifier]),
            ("__builtin_assume_aligned",              &[TypeSpecifier]),
            ("__builtin_LINE",                        &[TypeSpecifier]),
            ("__builtin_FUNCTION",                    &[TypeSpecifier]),
            ("__builtin_FILE",                        &[TypeSpecifier]),
            ("__builtin___clear_cache",               &[TypeSpecifier]),
            ("__builtin_prefetch",                    &[Function]),
            ("__builtin_classify_type",               &[TypeSpecifier]),
            ("__builtin_extend_pointer",              &[TypeSpecifier]),
            ("__builtin_goacc_parlevel_id",           &[TypeSpecifier]),
            ("__builtin_goacc_parlevel_size",         &[TypeSpecifier]),
            ("inline",       &[FunctionSpecifier]),
            ("_Noreturn",    &[FunctionSpecifier]),
            ("__inline__",   &[FunctionSpecifier]),
            ("__inline",     &[TypeSpecifier]),
            ("__cdecl",      &[FunctionSpecifier]),
            ("__clrcall",    &[FunctionSpecifier]),
            ("__stdcall",    &[FunctionSpecifier]),
            ("__fastcall",   &[FunctionSpecifier]),
            ("__thiscall",   &[FunctionSpecifier]),
            ("__vectorcall", &[FunctionSpecifier]),
            ("_purecall",              &[TypeSpecifier]),
            ("_purecall_handler",      &[TypeSpecifier]),
            ("_onexit_t",              &[TypeSpecifier]),
            ("_locale_t",              &[TypeSpecifier]),
            ("_invalid_parameter_handler", &[TypeSpecifier]),
            ("__int8",   &[TypeSpecifier]),
            ("__int16",  &[TypeSpecifier]),
            ("__int32",  &[TypeSpecifier]),
            ("__int64",  &[TypeSpecifier]),
            ("__int128", &[TypeSpecifier]),
            ("_Float16",   &[TypeSpecifier]),
            ("_Float32",   &[TypeSpecifier]),
            ("_Float64",   &[TypeSpecifier]),
            ("_Float128",  &[TypeSpecifier]),
            ("_Float16x",  &[TypeSpecifier]),
            ("_Float32x",  &[TypeSpecifier]),
            ("_Float64x",  &[TypeSpecifier]),
            ("_Float128x", &[TypeSpecifier]),
            ("__v8hf",   &[TypeSpecifier]),
            ("__bf16",   &[TypeSpecifier]),
            ("__v16bf",  &[TypeSpecifier]),
            ("__declspec",   &[FunctionSpecifier]),
            ("__attribute__",&[FunctionSpecifier]),
            ("alignas",  &[AlignmentSpecifier]),
            ("_Alignas", &[AlignmentSpecifier]),
            ("align",    &[AlignmentSpecifier]),
        ];

        for (name, classes) in kws {
            let sym = Symbol::new_kw(name, classes);
            if let Some(scope) = st.scope_stack.last_mut() {
                scope.members.insert(sym.name.clone(), sym);
            }
        }
        st
    }

    pub fn define(&mut self, symbol: Symbol) -> bool {
        if let Some(current) = self.scope_stack.last_mut() {
            if current.members.contains_key(&symbol.name) {
                return false;
            }
            current.members.insert(symbol.name.clone(), symbol);
            true
        } else {
            false
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(sym) = scope.members.get(name) {
                return Some(sym);
            }
        }
        None
    }

    pub fn push_block_scope(&mut self) {
        self.block_counter += 1;
        let scope = Symbol {
            name: format!("block{}", self.block_counter),
            classification: std::iter::once(TypeClassification::Block).collect(),
            predefined: true,
            members: HashMap::new(),
        };
        self.scope_stack.push(scope);
    }

    pub fn pop_block_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    pub fn is_in_block_scope(&self) -> bool {
        self.scope_stack.len() > 1
    }
}

thread_local! {
    static SYMBOL_TABLE: RefCell<SymbolTable> = RefCell::new(SymbolTable::new());
}

fn with_st<F, R>(f: F) -> R
where
    F: FnOnce(&SymbolTable) -> R,
{
    SYMBOL_TABLE.with(|st| f(&st.borrow()))
}

fn with_st_mut<F, R>(f: F) -> R
where
    F: FnOnce(&mut SymbolTable) -> R,
{
    SYMBOL_TABLE.with(|st| f(&mut st.borrow_mut()))
}

// ── Actions ──────────────────────────────────────────────────────────────────

pub fn enter_scope() {
    with_st_mut(|st| st.push_block_scope());
}

pub fn exit_scope() {
    with_st_mut(|st| st.pop_block_scope());
}

/// Registers typedef declarations into the symbol table.
///
/// `tokens` is a reverse-ordered snapshot of recently-consumed tokens
/// collected by the grammar action via `recog.input.lt(-k)`.
///
/// Two call sites:
///   • `declaration` rule (after `;`): tokens[0].0 == SEMI.
///     Forward-scan to find `typedef` and identifier names at paren depth 0.
///   • `declarator` rule (after direct identifier): tokens[0].0 == IDENT.
///     Scan further back to find `typedef` before the previous `;`.
pub fn enter_declaration(tokens: Vec<(i32, String)>) {
    // Token type constants from the generated CParser
    const TYPEDEF: i32 = 68;
    const SEMI: i32 = 116;
    const IDENT: i32 = 134;
    const LPAREN: i32 = 88;
    const RPAREN: i32 = 89;
    const LBRACE: i32 = 92;
    const RBRACE: i32 = 93;
    const LBRACKET: i32 = 90;
    const RBRACKET: i32 = 91;
    const STRUCT: i32 = 65;
    const UNION: i32 = 71;
    const ENUM_TT: i32 = 45;

    if tokens.is_empty() {
        return;
    }
    let first_tt = tokens[0].0;

    if first_tt == SEMI {
        // From the `declaration` rule: tokens is [';', prev, prev-1, ...].
        // Re-scan forward (reverse the slice) to locate typedef and names.
        let is_typedef = tokens.iter().any(|(tt, _)| *tt == TYPEDEF);
        if !is_typedef {
            return;
        }
        let mut depth: i32 = 0;
        let mut prev_tt: i32 = -1;
        for (tt, text) in tokens.iter().rev() {
            let tt = *tt;
            match tt {
                LPAREN | LBRACE | LBRACKET => depth += 1,
                RPAREN | RBRACE | RBRACKET => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                IDENT if depth == 0 => {
                    // Skip struct/union/enum tag names (e.g. the `node` in `struct node`)
                    if prev_tt != STRUCT && prev_tt != UNION && prev_tt != ENUM_TT {
                        define_typedef(text);
                    }
                }
                _ => {}
            }
            prev_tt = tt;
        }
    } else if first_tt == IDENT {
        // From the `declarator` rule with a direct identifier.
        // tokens[0].1 is the declared name; scan further back for `typedef`.
        // Tokens are in reverse chronological order; RBRACE/LBRACE track brace depth
        // so that semicolons inside struct bodies don't stop the scan prematurely.
        let name = tokens[0].1.clone();
        let mut is_typedef = false;
        let mut depth: i32 = 0;
        for (tt, _) in &tokens[1..] {
            match *tt {
                RBRACE => depth += 1,
                LBRACE => { if depth > 0 { depth -= 1; } }
                SEMI if depth == 0 => break, // previous declaration boundary
                TYPEDEF if depth == 0 => { is_typedef = true; break; }
                _ => {}
            }
        }
        if is_typedef {
            define_typedef(&name);
        } else if with_st(|st| st.is_in_block_scope()) {
            // Only register as a variable in block scope.  Global-scope identifiers
            // (function parameters, top-level declarations) must not shadow typedefs
            // that other translation-unit files may declare with the same name.
            define_variable(&name);
        }
    }
    // Other first_tt values (e.g. RPAREN for outer function-suffix declarators): skip.
}

fn define_typedef(name: &str) {
    with_st_mut(|st| {
        let sym = Symbol {
            name: name.to_owned(),
            classification: std::iter::once(TypeClassification::TypeSpecifier).collect(),
            predefined: false,
            members: HashMap::new(),
        };
        st.define(sym);
    });
}

fn define_variable(name: &str) {
    with_st_mut(|st| {
        let sym = Symbol {
            name: name.to_owned(),
            classification: std::iter::once(TypeClassification::Variable).collect(),
            predefined: false,
            members: HashMap::new(),
        };
        st.define(sym);
    });
}

/// No-op stub.
pub fn lookup_symbol() {}

/// No-op stub.
pub fn output_symbol_table() {}

// ── Internal helpers ─────────────────────────────────────────────────────────

fn classify(text: &str) -> HashSet<TypeClassification> {
    with_st(|st| {
        st.resolve(text)
            .map(|s| s.classification.clone())
            .unwrap_or_default()
    })
}

// ── Predicate implementations ─────────────────────────────────────────────────

pub fn is_typedef_name(text: &str) -> bool {
    let class = classify(text);
    if class.is_empty() {
        return false;
    }
    !class.contains(&TypeClassification::Variable)
        && !class.contains(&TypeClassification::Function)
}

pub fn is_alignment_specifier(text: &str) -> bool {
    classify(text).contains(&TypeClassification::AlignmentSpecifier)
}

pub fn is_atomic_type_specifier(text: &str) -> bool {
    classify(text).contains(&TypeClassification::AtomicTypeSpecifier)
}

pub fn is_storage_class_specifier(text: &str) -> bool {
    classify(text).contains(&TypeClassification::StorageClassSpecifier)
}

pub fn is_function_specifier(text: &str) -> bool {
    classify(text).contains(&TypeClassification::FunctionSpecifier)
}

pub fn is_type_qualifier(text: &str) -> bool {
    classify(text).contains(&TypeClassification::TypeQualifier)
}

pub fn is_enum_specifier(text: &str) -> bool {
    text == "enum"
}

pub fn is_struct_or_union_specifier(text: &str) -> bool {
    matches!(text, "struct" | "union")
}

pub fn is_typeof_specifier(text: &str) -> bool {
    matches!(
        text,
        "typeof" | "__typeof__" | "__typeof" | "typeof_unqual" | "__typeof_unqual__"
    )
}

pub fn is_type_specifier(text: &str) -> bool {
    let class = classify(text);
    if class.contains(&TypeClassification::TypeSpecifier) {
        return true;
    }
    if is_atomic_type_specifier(text) {
        return true;
    }
    if is_struct_or_union_specifier(text) {
        return true;
    }
    if is_enum_specifier(text) {
        return true;
    }
    if is_typedef_name(text) {
        return true;
    }
    if is_typeof_specifier(text) {
        return true;
    }
    false
}

pub fn is_type_specifier_qualifier(text: &str) -> bool {
    is_type_specifier(text) || is_type_qualifier(text) || is_alignment_specifier(text)
}

pub fn is_attribute_specifier(text: &str) -> bool {
    text == "["
}

pub fn is_static_assert_declaration(text: &str) -> bool {
    matches!(text, "static_assert" | "_Static_assert")
}

fn prev_tokens_have_type_specifier(prev_tokens: &[(i32, String)]) -> bool {
    for (_, text) in prev_tokens {
        let sym = with_st(|st| st.resolve(text).cloned());
        if let Some(s) = sym {
            if s.classification.contains(&TypeClassification::TypeSpecifier)
                || s.classification.contains(&TypeClassification::AtomicTypeSpecifier)
            {
                return true;
            }
        }
        // struct/union/enum/typeof keywords are type specifiers but may not be
        // classified as TypeSpecifier in the predefined table.
        if is_enum_specifier(text) || is_typeof_specifier(text) || is_struct_or_union_specifier(text) {
            return true;
        }
    }
    false
}

/// Returns true if `text` can begin a declaration specifier at this point.
///
/// Predefined type keywords (`int`, `long`, `void`, …) are always accepted.
/// User-defined typedef names are only accepted as a type specifier if no type
/// specifier has been seen yet in the current declaration (checked via
/// `prev_tokens`). This prevents a typedef name used as a variable name (e.g.
/// `i64` in `sqlite3_int64 i64;`) from being consumed as a second type specifier.
pub fn is_declaration_specifier(text: &str, prev_tokens: &[(i32, String)]) -> bool {
    // Storage class, type qualifiers, function/alignment specifiers: unconditional.
    // Note: __attribute__ is classified as FunctionSpecifier and must be allowed here
    // so that `void __attribute__((cdecl)) func(...)` parses correctly after GCC
    // expands __cdecl → __attribute__((__cdecl__)) during preprocessing.
    if is_storage_class_specifier(text)
        || is_type_qualifier(text)
        || is_function_specifier(text)
        || is_alignment_specifier(text)
    {
        return true;
    }
    // Type specifiers: distinguish predefined keywords from user-defined typedefs.
    let sym = with_st(|st| st.resolve(text).cloned());
    if let Some(ref s) = sym {
        let is_ts = s.classification.contains(&TypeClassification::TypeSpecifier)
            || s.classification.contains(&TypeClassification::AtomicTypeSpecifier);
        if is_ts {
            if s.predefined {
                // Predefined keyword (int, long, __int64, …): always a type specifier.
                return true;
            } else {
                // User-defined typedef: only if no type specifier has been seen yet.
                return !prev_tokens_have_type_specifier(prev_tokens);
            }
        }
    }
    // Structural type specifiers matched by text.
    if is_enum_specifier(text) || is_typeof_specifier(text) {
        return true;
    }
    false
}

pub fn is_declaration(text: &str) -> bool {
    // At the start of a new block item there is no prior type specifier context.
    is_declaration_specifier(text, &[])
        || is_attribute_specifier(text)
        || is_static_assert_declaration(text)
}

pub fn is_statement(t1_text: &str, t2_text: &str) -> bool {
    // A labelled statement starts with `Identifier ':'`.
    if t2_text == ":" && !is_declaration_specifier(t1_text, &[]) {
        return true;
    }
    !is_declaration(t1_text)
}

pub fn is_null_struct_declaration_list_extension() -> bool {
    true
}

pub fn is_init_declarator_list(text: &str, prev_tokens: &[(i32, String)]) -> bool {
    let class = classify(text);
    if class.is_empty() {
        return true; // unknown identifier → declarator
    }
    if text == "__attribute__" {
        return false;
    }
    if class.contains(&TypeClassification::TypeQualifier) {
        return false;
    }
    if class.contains(&TypeClassification::TypeSpecifier) {
        let sym = with_st(|st| st.resolve(text).cloned());
        if let Some(s) = sym {
            if !s.predefined {
                // User-defined typedef used as variable name: allowed if a type
                // specifier was already seen (e.g. `i64` in `sqlite3_int64 i64;`).
                return prev_tokens_have_type_specifier(prev_tokens);
            }
        }
        return false;
    }
    true
}

fn is_type_name(text: &str) -> bool {
    is_type_specifier_qualifier(text)
}

pub fn is_something_of_typename(t1_text: &str, t2_text: &str, t3_text: &str) -> bool {
    let is_op = matches!(
        t1_text,
        "sizeof"
            | "alignof"
            | "_Alignof"
            | "__alignof__"
            | "__alignof"
            | "_Maxof"
            | "_Minof"
            | "_Countof"
    );
    if !is_op {
        return false;
    }
    if t2_text != "(" {
        return false;
    }
    is_type_name(t3_text)
}

pub fn is_cast(t1_text: &str, t2_text: &str) -> bool {
    if t1_text != "(" {
        return false;
    }
    with_st(|st| match st.resolve(t2_text) {
        None => false,
        Some(sym) => {
            if sym.predefined {
                // Predefined keyword: cast if it is a type-like classification.
                sym.classification.iter().any(|c| {
                    matches!(
                        c,
                        TypeClassification::TypeSpecifier
                            | TypeClassification::TypeQualifier
                            | TypeClassification::AtomicTypeSpecifier
                            | TypeClassification::StorageClassSpecifier
                    )
                })
            } else {
                // User-defined: cast only for typedefs (TypeSpecifier).
                sym.classification.contains(&TypeClassification::TypeSpecifier)
            }
        }
    })
}

/// Run the C preprocessor on `source_name` and return the preprocessed text.
/// Mirrors the default `gcc -std=c2x -E -C` behaviour of CLexerBase.cs.
/// Falls back to reading the raw file if gcc is unavailable or fails.
pub fn preprocess_input(source_name: &str) -> String {
    // If the source is not a .c file write it to a temp file so gcc accepts it.
    let actual_source = if source_name.ends_with(".c") {
        source_name.to_owned()
    } else {
        let content = std::fs::read_to_string(source_name).unwrap_or_default();
        std::fs::write("stdin.c", content.as_bytes()).ok();
        "stdin.c".to_owned()
    };
    let output_name = format!("{}.p", source_name);
    match std::process::Command::new("gcc")
        .args(["-std=c2x", "-E", "-C", &actual_source])
        .output()
    {
        Ok(out) if !out.stdout.is_empty() => {
            let text = String::from_utf8_lossy(&out.stdout).into_owned();
            std::fs::write(&output_name, &text).ok();
            text
        }
        _ => std::fs::read_to_string(source_name).unwrap_or_default(),
    }
}
