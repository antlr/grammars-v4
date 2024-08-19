"""
Run this script from the Python directory to ensure examples work.


Timo Barnard (23607165@sun.ac.za)
"""
from antlr4 import *
import os
from io import StringIO
from RustLexer import RustLexer
from RustParser import RustParser
import traceback
import time
examples_clean = [l.path for l in os.scandir('../examples') if l.name.endswith('.rs')]
examples_tree = [l.path for l in os.scandir('../examples') if l.name.endswith('.rs.tree')]

def test_grammar(filepath, should_pass = True):
    inputstream = FileStream(filepath,encoding='utf-8')
    output = StringIO()
    lexer = RustLexer(inputstream, output)
    tokenstream = CommonTokenStream(lexer)
    parser = RustParser(tokenstream, output)
    did_pass = None 
    start, end = None, None
    try:
        start = time.time_ns()
        print(parser.crate().toStringTree(parser.ruleNames))
        end = time.time_ns()
        did_pass = True 
    except Exception as e:
        end = time.time_ns()
        did_pass = False 
        output.write('\n'*2)
        output.write(' *'*10)
        output.write(' EXCEPTION ')
        output.write('* '*20)
        output.write('\n')
        traceback.print_exc(file=output)
        output.write("\n"*2)
    result = output.getvalue()
    did_pass = result.replace(" ",'').replace('\n','') == ''
    d = ""
    if end != None:
        d = f'{(end - start)/ 1_000_000_000}s' 
    print(f"{filepath} -> {'pass' if did_pass else 'fail'} {d}")
    print(result)

if __name__ == "__main__":
    for example in examples_clean:
        test_grammar(example)