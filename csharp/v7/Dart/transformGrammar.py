"""Replace adjacent-token predicates with a helper function call for Dart target.

The Dart ANTLR4 code generator produces invalid Dart for expressions like
  $first.index + 1 == $second.index
because the null-safe expansion has wrong operator precedence.
We replace each such predicate with a helper function call.
"""
import re
import shutil
from glob import glob
from pathlib import Path


def transform(file_path: str) -> None:
    print(f"Transforming {file_path}")
    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", "r", encoding="utf-8") as f:
        text = f.read()
    # Replace $first.index + 1 == $second.index with a helper call
    text = text.replace(
        "{$first.index + 1 == $second.index}?",
        "{adjacentTokens($first, $second)}?"
    )
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(text)


for g4 in glob("./*.g4"):
    transform(g4)
