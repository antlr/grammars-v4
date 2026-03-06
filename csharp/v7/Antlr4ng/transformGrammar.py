"""Add @header import blocks to the CSharp grammar files for the Antlr4ng target."""
import re
import shutil
from glob import glob
from pathlib import Path

HEADERS = {
    "CSharpLexer.g4":
        '@header { import { CSharpLexerBase } from "./CSharpLexerBase.js"; }\n',
    "CSharpParser.g4":
        '@header { import { CSharpParserBase } from "./CSharpParserBase.js"; }\n',
}

def transform(file_path: str) -> None:
    header = HEADERS.get(Path(file_path).name)
    if header is None:
        return
    print(f"Transforming {file_path}")
    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", "r", encoding="utf-8") as f:
        text = f.read()
    # Insert @header before the first 'options {' block
    text = re.sub(r"(options\s*\{)", header + r"\1", text, count=1)
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(text)

for g4 in glob("./*.g4"):
    transform(g4)
