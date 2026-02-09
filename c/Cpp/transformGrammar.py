#!/usr/bin/env python3
"""
transformGrammar.py for Cpp target.

This script transforms the official ANTLR C grammar files (CLexer.g4, CParser.g4)
to work with the C++ (Cpp) target by:
1. Replacing placeholder comments with proper @header directives
2. Converting `this.` to `this->` for C++ syntax
"""

import re
import glob


def transform_file(file_path):
    with open(file_path, 'r') as f:
        content = f.read()

    if 'CLexer' in file_path:
        # Replace: // Insert here @header for lexer.
        content = content.replace(
            '// Insert here @header for lexer.',
            '@header {#include "CLexerBase.h"}'
        )

    elif 'CParser' in file_path:
        # Replace: // Insert here @header for parser.
        content = content.replace(
            '// Insert here @header for parser.',
            '@header {#include "CParserBase.h"}'
        )

    # Convert this.MethodName() -> this->MethodName() for C++ syntax
    content = re.sub(r'\bthis\.', 'this->', content)

    with open(file_path, 'w') as f:
        f.write(content)

    print(f"Transformed {file_path}")


def main():
    for file in glob.glob("./*.g4"):
        transform_file(file)
    print("Grammar transformation for Cpp target complete.")


if __name__ == '__main__':
    main()
