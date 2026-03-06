"""Replace 'this.' with Go receiver variables in grammar action/predicate blocks.

ANTLR4's Go code generator copies grammar actions verbatim, so 'this.X()'
from Java/C# style grammar actions must be replaced with the receiver variable.
In Go generated code:
  - Lexer *Action methods use receiver 'l *CSharpLexer'
  - Lexer *Sempred methods use receiver 'p *CSharpLexer'
  - Parser *Sempred methods use receiver 'p *CSharpParser'

Strategy:
  - 'this.Is' (predicate calls) -> 'p.'  in both lexer and parser grammars
  - 'this.' (everything else, action calls) -> 'l.' in lexer grammar, 'p.' in parser grammar
"""
import shutil
from glob import glob
from pathlib import Path


def transform_lexer(text: str) -> str:
    # Predicate calls: this.Is... -> p.Is...
    text = text.replace("this.Is", "p.Is")
    # Action calls: this.On/Open/... -> l.On/Open/...
    text = text.replace("this.", "l.")
    return text


def transform_parser(text: str) -> str:
    # Parser: all this. -> p.
    text = text.replace("this.", "p.")
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


for g4 in glob("./*.g4") + glob("./parser/*.g4"):
    transform(g4)
