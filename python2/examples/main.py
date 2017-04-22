
import sys
import os

# Update the path below to find the python version of the ANTLR4 runtime files.
sys.path.append("../libs")

from Python2Parser       import Python2Parser
from Python2Lexer        import Python2Lexer
from Python2Listener     import Python2Listener
from antlr4              import *


def main(argv):
    file_count = 0
    fail_count = 0
    for root, dirs, files in os.walk(r"C:\Python27"): #r"C:\Python27"):
        for file in files:
            path = os.path.join(root, file)
            if file.endswith(".py"):
                sys.stderr.write("Now parsing %s\n" % path)
                sys.stderr.flush()
                file_count += 1
                try:
                    input = FileStream(path, 'utf-8') #'windows-1252') #argv[1])
                    lexer = Python2Lexer(input)
                    tokens = CommonTokenStream(lexer)
                    parser = Python2Parser(tokens)
                    tree = parser.file_input()
                    listener = Python2Listener() #parser);
                    ParseTreeWalker.DEFAULT.walk(listener, tree)
                except Exception as ex:
                    fail_count += 1
                    print ex
                if fail_count == 200:
                    break;

"""
def main(argv):
    input = FileStream("Python2Parser.py")
    lexer = Python2Lexer(input)
    tokens = CommonTokenStream(lexer)
    parser = Python2Parser(tokens)
    tree = parser.file_input()
    listener = Python2Listener() #parser);
    ParseTreeWalker.DEFAULT.walk(listener, tree)         
"""
   
if __name__ == "__main__":
    main(sys.argv)
