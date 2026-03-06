"""Transform grammar files for the C++ ANTLR4 target.

Two fixes are needed:
1. Add @header sections so the generated .h files include the base class headers.
   Without this, CSharpLexer.h and CSharpParser.h fail to compile because
   CSharpLexerBase/CSharpParserBase are used as base classes but never declared.
2. Replace 'this.' with 'this->' in grammar action/predicate blocks.
   The ANTLR4 C++ generator copies actions verbatim, so Java/C# style 'this.X()'
   must become 'this->X()' for valid C++.
"""
import shutil
from glob import glob
from pathlib import Path


LEXER_OPTIONS_BLOCK = "options {\n    superClass = CSharpLexerBase;\n}"
LEXER_OPTIONS_WITH_HEADER = (
    "options {\n    superClass = CSharpLexerBase;\n}\n"
    "\n@header {\n#include \"CSharpLexerBase.h\"\n}"
)

PARSER_OPTIONS_BLOCK = "options {\n    tokenVocab = CSharpLexer;\n    superClass = CSharpParserBase;\n}"
PARSER_OPTIONS_WITH_HEADER = (
    "options {\n    tokenVocab = CSharpLexer;\n    superClass = CSharpParserBase;\n}\n"
    "\n@header {\n#include \"CSharpParserBase.h\"\n}"
)


def transform_lexer(text: str) -> str:
    text = text.replace(LEXER_OPTIONS_BLOCK, LEXER_OPTIONS_WITH_HEADER)
    text = text.replace("this.", "this->")
    return text


def transform_parser(text: str) -> str:
    text = text.replace(PARSER_OPTIONS_BLOCK, PARSER_OPTIONS_WITH_HEADER)
    text = text.replace("this.", "this->")
    return text


TRANSFORMS = {
    "CSharpLexer.g4":  transform_lexer,
    "CSharpParser.g4": transform_parser,
}


def transform(file_path: str) -> None:
    fn = Path(file_path).name
    transform_fn = TRANSFORMS.get(fn)
    if transform_fn is None:
        return
    print(f"Transforming {file_path}")
    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", "r", encoding="utf-8") as f:
        text = f.read()
    text = transform_fn(text)
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(text)


for g4 in glob("./*.g4"):
    transform(g4)
