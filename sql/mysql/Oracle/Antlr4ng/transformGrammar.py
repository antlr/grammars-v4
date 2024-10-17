import sys, os, re, shutil
from glob import glob
from pathlib import Path

def main(argv):
    for file in glob("./*.g4"):
        fix(file)

def fix(file_path):
    print("Altering " + file_path)
    if not os.path.exists(file_path):
        print(f"Could not find file: {file_path}")
        sys.exit(1)
    parts = os.path.split(file_path)
    file_name = parts[-1]
    shutil.move(file_path, file_path + ".bak")
    input_file = open(file_path + ".bak",'r')
    output_file = open(file_path, 'w')
    for x in input_file:
        if '// Insert here @header for lexer.' in x:
            x = x.replace('// Insert here @header for lexer.', '''
@header {
// Modified by transformGrammar.py
// This code is automatically inserted because it is for the Antlr4ng target. It cannot be
// in the .g4 directly, so we look for a specific comment in the .g4 and put it there.

/* eslint-disable @typescript-eslint/no-unused-vars, no-useless-escape */

import { MySQLLexerBase, SqlMode } from "./MySQLLexerBase.js";
}
'''
            )
        if '// Insert here @header for parser.' in x:
            x = x.replace('// Insert here @header for parser.', '''
@header {
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-useless-escape, no-lone-blocks */

import { MySQLParserBase } from "./MySQLParserBase.js";
import { SqlMode } from "./MySQLLexerBase.js";
}
'''
            )

        output_file.write(x)
        output_file.flush()

    print("Writing ...")
    input_file.close()
    output_file.close()

if __name__ == '__main__':
    main(sys.argv)
