
import sys

# Update the path below to find the python version of the ANTLR4 runtime files.
sys.path.append("../libs")

from Python2Parser       import Python2Parser
from Python2Lexer        import Python2Lexer
from Python2Listener     import Python2Listener
from antlr4              import *

def main(argv):
    input  = FileStream("test_input_Python2Parser.py") #argv[1])
    lexer  = Python2Lexer(input);
    tokens = CommonTokenStream(lexer);
    parser = Python2Parser(tokens);
    tree   = parser.file_input(); 

    listener = Python2Listener();
    ParseTreeWalker.DEFAULT.walk(listener, tree); 
    

if __name__ == "__main__":
    main(sys.argv)
