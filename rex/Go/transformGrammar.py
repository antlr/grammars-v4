"""Adapt the generated copy of the REx lexer for Go (idempotent)."""
from pathlib import Path

for path in list(Path(".").glob("rexLexer.g4")) + list(Path("parser").glob("rexLexer.g4")):
    text = path.read_text(encoding="utf-8")
    text = text.replace("this.Check1()", "p.Check1()")
    path.write_text(text, encoding="utf-8")
