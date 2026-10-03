"""Adapt the generated copy of the REx lexer for Python3 (idempotent)."""
from pathlib import Path

for path in Path(".").glob("rexLexer.g4"):
    text = path.read_text(encoding="utf-8")
    text = text.replace("this.Check1()", "self.Check1()")
    path.write_text(text, encoding="utf-8")
