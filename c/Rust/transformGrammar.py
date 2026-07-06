"""Transforms C grammar files for the Rust (antlr4rust) target.

antlr4rust ignores the superClass option, so predicate and action method calls
on `this` must be replaced with inline Rust expressions that use `recog` (the
parser reference available in rule bodies and sempred functions) and a
thread-local symbol table provided by `crate::c_parser_base`.

Two-phase operation (called twice by build.sh):
  Phase 1 (before ANTLR): .g4 files still contain `this.` → transform them.
  Phase 2 (after ANTLR) : .g4 files no longer contain `this.`
                           → fix codegen bugs, copy c_parser_base.rs, patch main.rs.
"""

import re
import shutil
import sys
from glob import glob
from pathlib import Path

# ---------------------------------------------------------------------------
# Inline Rust snippets replacing `this.Method()` calls
# ---------------------------------------------------------------------------

# Helper: get token text at position k via recog.input.lt(k)
def _lt(k: int) -> str:
    return (
        f"recog.input.lt({k})"
        f".map(|t| t.get_text().to_owned())"
        f".unwrap_or_default()"
    )

_T1 = f"let __t1 = {_lt(1)}; "
_T2 = f"let __t2 = {_lt(2)}; "
_T3 = f"let __t3 = {_lt(3)}; "

_IS_TYPEDEF_NAME = (
    _T1 + "crate::c_parser_base::is_typedef_name(&__t1)"
)

_BT_SCAN = (
    "let mut __bt: Vec<(i32,String)> = Vec::new();"
    " let mut __bk: isize = 1;"
    " loop {"
    " let (__btt,__btx) = recog.input.lt(-__bk)"
    ".map(|t| (t.get_token_type(), t.get_text().to_owned()))"
    ".unwrap_or((-1,String::new()));"
    " if __btt <= 0 { break; }"
    " __bt.push((__btt, __btx));"
    " if __btt == 116 || __btt == 92 || __btt == 93 || __btt == 88 || __btt == 117 || __bk >= 200 { break; }"
    " __bk += 1; } "
)

_IS_DECLARATION_SPECIFIER = (
    _T1 + _BT_SCAN + "crate::c_parser_base::is_declaration_specifier(&__t1, &__bt)"
)

_IS_TYPE_SPECIFIER_QUALIFIER = (
    _T1 + "crate::c_parser_base::is_type_specifier_qualifier(&__t1)"
)

_IS_NULL_STRUCT = "crate::c_parser_base::is_null_struct_declaration_list_extension()"

_IS_INIT_DECLARATOR_LIST = (
    _T1 + _BT_SCAN + "crate::c_parser_base::is_init_declarator_list(&__t1, &__bt)"
)

_IS_STATEMENT = (
    _T1 + _T2 + "crate::c_parser_base::is_statement(&__t1, &__t2)"
)

_IS_DECLARATION = (
    _T1 + "crate::c_parser_base::is_declaration(&__t1)"
)

_IS_SOMETHING_OF_TYPENAME = (
    _T1 + _T2 + _T3
    + "crate::c_parser_base::is_something_of_typename(&__t1, &__t2, &__t3)"
)

_NOT_IS_SOMETHING_OF_TYPENAME = (
    _T1 + _T2 + _T3
    + "!crate::c_parser_base::is_something_of_typename(&__t1, &__t2, &__t3)"
)

_IS_CAST = (
    _T1 + _T2 + "crate::c_parser_base::is_cast(&__t1, &__t2)"
)

# ---------------------------------------------------------------------------
# Grammar transformations
# ---------------------------------------------------------------------------

# Each entry: (pattern, replacement) applied to every .g4 line.
# Order matters: negated forms before positive forms.
_PREDICATE_REPLACEMENTS = [
    # Negated IsSomethingOfTypeName
    (r"\{!this\.IsSomethingOfTypeName\(\)\}\?",
     "{" + _NOT_IS_SOMETHING_OF_TYPENAME + "}?"),
    # Positive IsSomethingOfTypeName
    (r"\{this\.IsSomethingOfTypeName\(\)\}\?",
     "{" + _IS_SOMETHING_OF_TYPENAME + "}?"),
    # IsCast
    (r"\{this\.IsCast\(\)\}\?",
     "{" + _IS_CAST + "}?"),
    # IsInitDeclaratorList
    (r"\{this\.IsInitDeclaratorList\(\)\}\?",
     "{" + _IS_INIT_DECLARATOR_LIST + "}?"),
    # IsDeclarationSpecifier (may have leading space: `{ this.` or `{this.`)
    (r"\{[ ]*this\.IsDeclarationSpecifier\(\)[ ]*\}\?",
     "{" + _IS_DECLARATION_SPECIFIER + "}?"),
    # IsNullStructDeclarationListExtension
    (r"\{this\.IsNullStructDeclarationListExtension\(\)\}\?",
     "{" + _IS_NULL_STRUCT + "}?"),
    # IsTypeSpecifierQualifier
    (r"\{this\.IsTypeSpecifierQualifier\(\)\}\?",
     "{" + _IS_TYPE_SPECIFIER_QUALIFIER + "}?"),
    # IsTypedefName
    (r"\{this\.IsTypedefName\(\)\}\?",
     "{" + _IS_TYPEDEF_NAME + "}?"),
    # IsStatement
    (r"\{this\.IsStatement\(\)\}\?",
     "{" + _IS_STATEMENT + "}?"),
    # IsDeclaration
    (r"\{this\.IsDeclaration\(\)\}\?",
     "{" + _IS_DECLARATION + "}?"),
]

_ACTION_REPLACEMENTS = [
    # Actions (no trailing `?`)
    (r"\{this\.OutputSymbolTable\(\);\}",
     "{crate::c_parser_base::output_symbol_table();}"),
    (r"\{this\.LookupSymbol\(\);\}",
     "{crate::c_parser_base::lookup_symbol();}"),
    (r"\{this\.EnterDeclaration\(\);\}",
     "{ let mut __etoks: Vec<(i32,String)> = Vec::new();"
     " let mut __ek: isize = 1;"
     " let mut __edepth: i32 = 0;"
     " loop {"
     " let (__ett,__etx) = recog.input.lt(-__ek)"
     ".map(|t| (t.get_token_type(), t.get_text().to_owned()))"
     ".unwrap_or((-1,String::new()));"
     " if __ett <= 0 { break; }"
     " __etoks.push((__ett, __etx));"
     " if __ett == 93 { __edepth += 1; }"        # 93 = RBRACE: entering block going backwards
     " else if __ett == 92 && __edepth > 0 { __edepth -= 1; }"  # 92 = LBRACE: leaving
     " if __edepth == 0 && (__ett == 116 || __ek >= 500) { break; }"  # SEMI only at depth 0
     " __ek += 1; }"
     " crate::c_parser_base::enter_declaration(__etoks); }"),
    (r"\{this\.EnterScope\(\);\}",
     "{crate::c_parser_base::enter_scope();}"),
    (r"\{this\.ExitScope\(\);\}",
     "{crate::c_parser_base::exit_scope();}"),
]

ALL_REPLACEMENTS = _PREDICATE_REPLACEMENTS + _ACTION_REPLACEMENTS


def needs_transform() -> bool:
    """Return True if any .g4 file still contains un-transformed `this.` calls."""
    for f in glob("./*.g4"):
        with open(f, encoding="utf-8") as fp:
            content = fp.read()
            # Skip lines that are commented out
            for line in content.splitlines():
                stripped = line.lstrip()
                if stripped.startswith("//"):
                    continue
                if "this." in line:
                    return True
    return False


def transform_grammar(file_path: str) -> None:
    print(f"Transforming {file_path}")
    src = Path(file_path)
    if not src.is_file():
        print(f"  Not found: {file_path}", file=sys.stderr)
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", encoding="utf-8") as inp, \
         open(file_path, "w", encoding="utf-8") as out:
        for line in inp:
            # Skip commented-out lines
            stripped = line.lstrip()
            if stripped.startswith("//"):
                out.write(line)
                continue
            for pattern, replacement in ALL_REPLACEMENTS:
                line = re.sub(pattern, replacement, line)
            out.write(line)
    print(f"  Done.")


def fix_generated_rust() -> None:
    """Phase-2 fixes applied after ANTLR has generated the Rust source files."""

    # 1. Fix antlr4rust codegen bugs in generated cparser.rs
    fn = Path("src/gen/cparser.rs")
    if fn.exists():
        content = fn.read_text(encoding="utf-8")
        fixed = content
        # Bug: doubled parser context name
        fixed = fixed.replace("CParserParserContext", "CParserContext")
        # Bug: into_owned() not available on &str; antlr4rust get_text() returns &str
        fixed = fixed.replace(".into_owned()", ".to_owned()")
        if fixed != content:
            fn.write_text(fixed, encoding="utf-8")
            print(f"Fixed codegen issues in {fn}")

    # 2. Copy c_parser_base.rs into src/
    # trgen copies all Rust/ files to Generated-Rust/, so check here first.
    candidates = [
        Path("c_parser_base.rs"),                          # copied by trgen alongside script
        Path(__file__).resolve().parent / ".." / "Rust" / "c_parser_base.rs",
    ]
    src_base = None
    for c in candidates:
        if c.is_file():
            src_base = c.resolve()
            break
    dst_base = Path("src") / "c_parser_base.rs"
    if src_base and not dst_base.exists():
        shutil.copy(src_base, dst_base)
        print(f"Copied {src_base} -> {dst_base}")
    elif not src_base:
        print("WARNING: c_parser_base.rs not found; src/c_parser_base.rs must be "
              "created manually.", file=sys.stderr)

    # 3. Add `mod c_parser_base;` to src/main.rs (once), just before `mod r#gen;`
    main_rs = Path("src/main.rs")
    if main_rs.exists():
        content = main_rs.read_text(encoding="utf-8")
        if "mod c_parser_base" not in content:
            lines = content.splitlines(keepends=True)
            insert_at = len(lines)
            for i, ln in enumerate(lines):
                if ln.startswith("mod r#gen"):
                    insert_at = i
                    break
            lines.insert(insert_at, "mod c_parser_base;\n")
            main_rs.write_text("".join(lines), encoding="utf-8")
            print("Added `mod c_parser_base;` to src/main.rs")

    # 4. Replace direct file read with a gcc preprocessing call.
    # Mirrors the default behaviour of CLexerBase.cs (gcc -std=c2x -E -C).
    if main_rs.exists():
        content = main_rs.read_text(encoding="utf-8")
        if "preprocess_input" not in content:
            old_read = (
                "    let my_string_result = fs::read_to_string(input_name);\n"
                "    let input = my_string_result.unwrap(); // Panics if Err\n"
            )
            new_read = (
                "    let input = crate::c_parser_base::preprocess_input(input_name);\n"
            )
            if old_read in content:
                content = content.replace("use std::fs;\n", "")
                content = content.replace(old_read, new_read)
                main_rs.write_text(content, encoding="utf-8")
                print("Patched src/main.rs to use preprocess_input")


def main() -> None:
    if needs_transform():
        for f in glob("./*.g4"):
            transform_grammar(f)
    # Always run post-generation fixes: copies c_parser_base.rs into src/,
    # patches mod declarations and preprocess_input into main.rs.
    # In Phase 1 the generated cparser.rs does not exist yet so that step is
    # silently skipped; in Phase 2 the full fixup runs.  When build.sh only
    # invokes this script once (trash 1.1.1) the Phase-1 call is sufficient.
    fix_generated_rust()


if __name__ == "__main__":
    main()
