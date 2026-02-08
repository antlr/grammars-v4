#!/usr/bin/env python3
"""
transformGrammar.py for Cpp target.

This script transforms the official ANTLR C grammar files (CLexer.g4, CParser.g4)
to work with the C++ (Cpp) target by:
1. Replacing placeholder comments with proper @header directives
2. Converting `this.` to `this->` for C++ syntax
"""

import re
import sys
import os
import shutil

def transform_file(input_path, output_path):
    with open(input_path, 'r') as f:
        content = f.read()

    filename = os.path.basename(input_path)

    if filename == 'CLexer.g4':
        # Replace: // Insert here @header for lexer.
        content = content.replace(
            '// Insert here @header for lexer.',
            '@header {#include "CLexerBase.h"}'
        )

    elif filename == 'CParser.g4':
        # Replace: // Insert here @header for parser.
        content = content.replace(
            '// Insert here @header for parser.',
            '@header {#include "CParserBase.h"}'
        )

    # Convert this.MethodName() -> this->MethodName() for C++ syntax
    content = re.sub(r'\bthis\.', 'this->', content)

    with open(output_path, 'w') as f:
        f.write(content)

    print(f"Transformed {input_path} -> {output_path}")

def main():
    # Determine paths
    script_dir = os.path.dirname(os.path.abspath(__file__))
    grammar_dir = os.path.dirname(script_dir)  # grammar/

    clexer_src = os.path.join(grammar_dir, 'CLexer.g4')
    cparser_src = os.path.join(grammar_dir, 'CParser.g4')

    # Output to the same grammar directory (overwrite in-place or to a target)
    # By default, transform in-place for ANTLR generation
    if len(sys.argv) > 1:
        output_dir = sys.argv[1]
    else:
        output_dir = grammar_dir

    os.makedirs(output_dir, exist_ok=True)

    clexer_dst = os.path.join(output_dir, 'CLexer.g4')
    cparser_dst = os.path.join(output_dir, 'CParser.g4')

    transform_file(clexer_src, clexer_dst)
    transform_file(cparser_src, cparser_dst)

    print("Grammar transformation for Cpp target complete.")

if __name__ == '__main__':
    main()
