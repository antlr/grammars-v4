import sys
import os
import re

# [[ Testing the Python2.g4 Grammar]]

# Generate the lexer and parser from the grammar:
# java -Dfile.encoding=UTF-8 org.antlr.v4.Tool -Dlanguage=Python2 Python2.g4

# Place this file in the same folder the generated code is in and 
# modify the vars below to point to the folder containing Python2 sources
# you want to scan, and the path to the ANTLR4 Python runtime.
PYTHON_SOURCE_TREE_TO_SCAN  = r"C:/Python27"
ANTLR4_PYTHON_RUNTIME       = r"../libs"

sys.path.append(ANTLR4_PYTHON_RUNTIME)
from antlr4 import *
from Python2Parser       import Python2Parser
from Python2Lexer        import Python2Lexer
from Python2Listener     import Python2Listener

regex = re.compile(r"-*-\s+coding:\s+([\w-]+)\s+-*-")
get_encoding = regex.findall

def main(argv):
    for root, dirs, files in os.walk(PYTHON_SOURCE_TREE_TO_SCAN):
        for file in files:
            path = os.path.join(root, file)
            if path.endswith(".py"):
                sys.stderr.write("Now parsing %s\n" % path)
                sys.stderr.flush()
                try:
                    # Python source files in non utf-8 encodings should have
                    # "# -*- coding: <encoding> -*-" at the top. Grab the
                    # encoding string if it's there and set FileStream to use 
                    # it.
                    with open(path, 'r') as f:
                        ln = f.readline()
                        enc = get_encoding(ln)
                        enc = 'utf-8' if len(enc) == 0 else enc[0]

                    input    = FileStream(path, enc)
                    lexer    = Python2Lexer(input)
                    tokens   = CommonTokenStream(lexer)
                    parser   = Python2Parser(tokens)
                    tree     = parser.file_input()
                    listener = Python2Listener()
                    ParseTreeWalker.DEFAULT.walk(listener, tree)
                except Exception as ex:
                    print ex
                    
if __name__ == "__main__":
    main(sys.argv)
