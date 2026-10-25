"""Transforms the Java grammar files for the Rust target.

antlr4rust ignores the superClass option, so predicate method calls are
inlined directly using `recog` (the parser reference available in both
rule bodies and sempred functions).
"""
import sys
import re
import shutil
from glob import glob
from pathlib import Path

# Inline Rust for IsNotIdentifierAssign():
#   Returns true unless LA(1) is an identifier-like token AND LA(2) is '='
_IS_NOT_IDENTIFIER_ASSIGN = (
    "(!matches!(recog.input.la(1), "
    "JavaParser_IDENTIFIER | JavaParser_MODULE | JavaParser_OPEN | "
    "JavaParser_REQUIRES | JavaParser_EXPORTS | JavaParser_OPENS | "
    "JavaParser_TO | JavaParser_USES | JavaParser_PROVIDES | "
    "JavaParser_WHEN | JavaParser_WITH | JavaParser_TRANSITIVE | "
    "JavaParser_YIELD | JavaParser_SEALED | JavaParser_PERMITS | "
    "JavaParser_RECORD | JavaParser_VAR) "
    "|| recog.input.la(2) != JavaParser_ASSIGN)"
)

# Inline Rust for DoLastRecordComponent():
#   Returns true unless a non-last recordComponent has ELLIPSIS.
#   In rule body predicates, _localctx is Some(Rc<RecordComponentListContextAll>).
#   In sempred functions, _localctx is Option<&RecordComponentListContextAll>.
#   as_deref() normalises both to Option<&RecordComponentListContextAll>.
#   NOTE: no '_ lifetime — ANTLR4 lexes ' as a literal-token delimiter.
_DO_LAST_RECORD_COMPONENT = (
    "_localctx.as_deref()"
    ".map(|ctx| { let rcs = ctx.recordComponent_all(); let count = rcs.len();"
    " (0..count).all(|i| rcs[i].ELLIPSIS().is_none() || i + 1 == count) })"
    ".unwrap_or(true)"
)


def needs_transform():
    """True if any .g4 file still contains the predicate method calls."""
    for f in glob("./*.g4"):
        with open(f, encoding="utf-8") as fp:
            if "this." in fp.read():
                return True
    return False


def fix_generated_rust():
    """Fix antlr4rust codegen bug: JavaParserParserContext -> JavaParserContext."""
    fn = Path("src/gen/javaparser.rs")
    if not fn.exists():
        return
    content = fn.read_text(encoding="utf-8")
    fixed = content.replace("JavaParserParserContext", "JavaParserContext")
    if fixed != content:
        fn.write_text(fixed, encoding="utf-8")
        print(f"Fixed JavaParserParserContext -> JavaParserContext in {fn}")


def main():
    if needs_transform():
        for file in glob("./*.g4"):
            transform_grammar(file)
    else:
        fix_generated_rust()


def transform_grammar(file_path):
    print("Altering " + file_path)
    if not Path(file_path).is_file():
        print(f"Could not find file: {file_path}")
        sys.exit(1)
    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", "r", encoding="utf-8") as input_file:
        with open(file_path, "w", encoding="utf-8") as output_file:
            for line in input_file:
                line = re.sub(
                    r"this\.IsNotIdentifierAssign\(\)",
                    _IS_NOT_IDENTIFIER_ASSIGN,
                    line,
                )
                line = re.sub(
                    r"this\.DoLastRecordComponent\(\)",
                    _DO_LAST_RECORD_COMPONENT,
                    line,
                )
                output_file.write(line)
    print("Writing ...")


if __name__ == "__main__":
    main()
