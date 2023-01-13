# Template generated code from trgen <version>

import sys
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from readchar import readchar
from <lexer_name> import <lexer_name>;
from <parser_name> import <parser_name>;
from CaseChangingStream import *;
from datetime import datetime

def getChar():
    xx = readchar()
    if (xx == 0):
        return '';
    return xx

class MyErrorListener(ErrorListener):
    __slots__ = 'num_errors'

    def __init__(self):
        super().__init__()
        self.num_errors = 0

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.num_errors = self.num_errors + 1
        print(f"line {line}:{column} {msg}");

def main(argv):
    show_tokens = False
    show_tree = False
    input = None
    file_name = None
    i = 1
    encoding = "utf-8"
    while i \< len(argv):
        arg = argv[i]
        if arg in ("-tokens"):
            show_tokens = True
        elif arg in ("-tree"):
            show_tree = True
        elif arg in ("-input"):
            i = i + 1
            input = argv[i]
        elif arg in ("-file"):
            i = i + 1
            file_name = argv[i]
        elif arg in ("-encoding"):
            i = i + 1
            encoding = argv[i]
        else:
            print("unknown")
        i = i + 1

    if (input == None and file_name == None):
        sb = ""
        ch = getChar()
        while (ch != ''):
            sb = sb + ch
            ch = getChar()
        input = sb
        str = InputStream(input);
    elif (input != None):
        str = InputStream(input);
    elif (file_name != None):
        str = FileStream(file_name, encoding);
<if (case_insensitive_type)>
    str = CaseChangingStream(str, "<case_insensitive_type>" == "Upper")
<endif>
    lexer = <lexer_name>(str);
    lexer.removeErrorListeners()
    l_listener = MyErrorListener()
    lexer.addErrorListener(l_listener)
    # lexer.strictMode = false
    tokens = CommonTokenStream(lexer)
    parser = <parser_name>(tokens)
    parser.removeErrorListeners()
    p_listener = MyErrorListener()
    parser.addErrorListener(p_listener)
    if (show_tokens):
        i = 0
        while True:
            ro_token = lexer.nextToken()
            token = ro_token
            # token.TokenIndex = i
            i = i + 1
            print(token)
            if (token.type == -1):
                break
        lexer.reset()
    start_time = datetime.now()
    tree = parser.<start_symbol>()
    end_time = datetime.now()
    diff = end_time - start_time
    diff_time = diff.total_seconds()
    print(f'Time: {diff_time}', file=sys.stderr);
    if p_listener.num_errors > 0 or l_listener.num_errors > 0:
        # Listener will have already printed the error(s) to stdout.
        print('Parse failed.', file=sys.stderr);
        sys.exit(1)
    else:
        print('Parse succeeded.', file=sys.stderr);
        if (show_tree):
            print(tree.toStringTree(recog=parser))
        sys.exit(0)

if __name__ == '__main__':
    main(sys.argv)
