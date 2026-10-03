"""Adapt the generated copy of the REx lexer for Antlr4ng (idempotent)."""
from pathlib import Path

for path in Path(".").glob("rexLexer.g4"):
    text = path.read_text(encoding="utf-8")
    header = "@header {import LexerBase from \"./LexerBase.js\";}"
    text = text.replace("// Insert @header for lexer.", header)
    path.write_text(text, encoding="utf-8")
