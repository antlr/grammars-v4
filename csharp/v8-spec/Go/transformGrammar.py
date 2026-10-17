"""The script transforms the grammar to fit for the go target """
import sys
import re
import shutil
from glob import glob
from pathlib import Path


def _scan_block(line, start):
    """Return (block_text, end_index) for the { ... } block starting at start.

    Skips over single-quoted ANTLR char literals like '}' to avoid counting
    their braces in the depth counter.
    """
    j = start + 1
    n = len(line)
    depth = 1
    while j < n and depth > 0:
        c = line[j]
        if c == "'":
            # Single-quoted ANTLR char literal: skip until closing '
            j += 1
            while j < n and line[j] != "'":
                j += 1
            if j < n:
                j += 1  # skip closing '
            continue
        if c == '{':
            depth += 1
        elif c == '}':
            depth -= 1
        j += 1
    return line[start:j], j


def _transform_line_lexer(line):
    """For lexer grammar: non-predicate actions → l., predicates → p.

    In Go, lexer action functions use receiver 'l' while sempred functions
    use receiver 'p', so the two kinds of embedded code need different prefixes.
    """
    result = []
    i = 0
    n = len(line)
    while i < n:
        if line[i] != '{':
            result.append(line[i])
            i += 1
            continue
        block, j = _scan_block(line, i)
        is_pred = j < n and line[j] == '?'
        if is_pred:
            block = block.replace('this.', 'p.')
            result.append(block)
            result.append('?')
            i = j + 1
        else:
            block = block.replace('this.', 'l.')
            result.append(block)
            i = j
    return ''.join(result)


def _transform_line_parser(line):
    """For parser grammar: all this. → p. (both actions and predicates use p).

    Also rename _localctx → localctx to match the Go generated variable name.
    """
    line = line.replace('this.', 'p.')
    line = line.replace('_localctx', 'localctx')
    return line


def main():
    """Executes the script."""
    for file in glob("./parser/*.g4"):
        transform_grammar(file)


def transform_grammar(file_path):
    """Transforms the grammar to fit for the go target"""
    print("Altering " + file_path)
    if not Path(file_path).is_file():
        print(f"Could not find file: {file_path}")
        sys.exit(1)

    is_lexer = 'Lexer' in Path(file_path).name
    transform_line = _transform_line_lexer if is_lexer else _transform_line_parser

    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", 'r', encoding="utf-8") as input_file:
        with open(file_path, 'w', encoding="utf-8") as output_file:
            for line in input_file:
                if 'this.' in line:
                    line = transform_line(line)
                output_file.write(line)
            output_file.flush()

    print("Writing ...")


if __name__ == '__main__':
    main()
