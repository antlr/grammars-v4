"""Adapt the generated REx lexer for antlr4rust (safe before/after generation).

Rust does not implement superClass. Inline LexerBase.Check1's lookahead
predicate using the BaseLexer reference supplied to generated sempred functions.
"""
from pathlib import Path
import re


def remove_superclass(match):
    body = re.sub(r"\bsuperClass\s*=\s*LexerBase\s*;", "", match.group(1))
    return "options {" + body + "}" if body.strip() else ""

for path in Path(".").glob("rexLexer.g4"):
    text = path.read_text(encoding="utf-8")
    text = re.sub(r"\boptions\s*\{([^{}]*)\}", remove_superclass, text)
    text = text.replace("this.Check1()", "recog.input.as_mut().unwrap().la(1) != 58")
    path.write_text(text, encoding="utf-8")
