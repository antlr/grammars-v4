"""Adapt the generated copy of the REx lexer for Antlr4ng (idempotent)."""
from pathlib import Path
import re

for path in Path(".").glob("rexLexer.g4"):
    text = path.read_text(encoding="utf-8")
    header = "@header {import LexerBase from \"./LexerBase.js\";}"
    if header not in text:
        text = text.replace("lexer grammar rexLexer;", "lexer grammar rexLexer;\n\n" + header)
    path.write_text(text, encoding="utf-8")

# The generated method must not shadow Parser.context.
path = Path("rexParser.g4")
text = path.read_text(encoding="utf-8")
path.write_text(re.sub(r"\bcontext\b", "context_", text), encoding="utf-8")
