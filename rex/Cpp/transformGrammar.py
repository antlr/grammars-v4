"""Adapt the generated copy of the REx lexer for Cpp (idempotent)."""
from pathlib import Path

for path in Path(".").glob("rexLexer.g4"):
    text = path.read_text(encoding="utf-8")
    text = text.replace("this.Check1()", "this->Check1()")
    header = "@header {#include \"LexerBase.h\"}"
    text = text.replace("// Insert @header for lexer.", header)
    path.write_text(text, encoding="utf-8")
