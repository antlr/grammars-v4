# Generated from trgen <version>

import sys
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from readchar import readchar
from <lexer_name> import <lexer_name>;
from <parser_name> import <parser_name>;
from datetime import datetime

def getChar():
    xx = readchar()
    if (xx == 0):
        return '';
    return xx

class MyErrorListener(ErrorListener):

    def __init__(self, q, t, o):
        super().__init__()
        self.had_error = False
        self.quiet = q
        self.tee = t;
        self.output = o

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.had_error = True
        if ( self.tee ):
            self.output.write(f"line {line}:{column} {msg}\n");
        if (not self.quiet):
            print(f"line {line}:{column} {msg}", file=sys.stderr);

tee = False
show_tokens = False
show_tree = False
show_trace = False
encoding = "utf-8"
error_code = 0
string_instance = 0
prefix = ""
quiet = False

def main(argv):
    global tee
    global show_tokens
    global show_tree
    global show_trace
    global encoding
    global prefix
    global quiet
    global error_code
    
    inputs = []
    is_fns = []
    prefix = ""
    i = 1
    while i \< len(argv):
        arg = argv[i]
        if arg in ("-tokens"):
            show_tokens = True
        elif arg in ("-tree"):
            show_tree = True
        elif arg in ("-prefix"):
            i = i + 1
            prefix = argv[i] + " "
        elif arg in ("-input"):
            i = i + 1
            inputs.append(argv[i])
            is_fns.append(False)
        elif arg in ("-encoding"):
            i = i + 1
            encoding = argv[i]
        elif arg in ("-tee"):
            tee = True
        elif arg in ("-x"):
            while f := sys.stdin.readline():
                f = f.strip()
                inputs.append(f)
                is_fns.append(True)
        elif arg in ("-q"):
            quiet = True
        elif arg in ("-trace"):
            show_trace = True
        else:
            inputs.append(argv[i])
            is_fns.append(True)
        i = i + 1
    if len(inputs) == 0:
        ParseStdin()
    else:
        start_time = datetime.now()
        for f in range(0, len(inputs)):
            if is_fns[f]:
                ParseFilename(inputs[f], f)
            else:
                ParseString(inputs[f], f)
        end_time = datetime.now()
        diff = end_time - start_time
        diff_time = diff.total_seconds()
        if (not quiet):
            print(f'Total Time: {diff_time}', file=sys.stderr);
    sys.exit(error_code)

def ParseStdin():
    sb = ""
    ch = getChar()
    while (ch != ''):
        sb = sb + ch
        ch = getChar()
    input = sb
    str = InputStream(input);
    DoParse(str, 'stdin', 0)

def ParseString(input, row_number):
    global string_instance
    str = InputStream(input)
    DoParse(str, 'string', row_number)
    string_instance = string_instance + 1

def ParseFilename(input, row_number):
    global encoding
    str = FileStream(input, encoding)
    DoParse(str, input, row_number)

def DoParse(str, input_name, row_number):
    global tee
    global show_tokens
    global show_tree
    global show_trace
    global encoding
    global prefix
    global quiet
    global error_code

    lexer = <lexer_name>(str)
    lexer.removeErrorListeners()
    if (tee):
        output = open(input_name + ".errors", "w")
    else:
        output = sys.stderr
    listener_lexer = MyErrorListener(quiet, tee, output)
    lexer.addErrorListener(listener_lexer)
    # lexer.strictMode = false
    tokens = CommonTokenStream(lexer)
    parser = <parser_name>(tokens)
    parser.removeErrorListeners()
    listener_parser = MyErrorListener(quiet, tee, output)
    parser.addErrorListener(listener_parser)
    if (show_tokens):
        i = 0
        while True:
            ro_token = lexer.nextToken()
            token = ro_token
            # token.TokenIndex = i
            i = i + 1
            print(token, file=sys.stderr)
            if (token.type == -1):
                break
        lexer.reset()
    if (show_trace) :
        parser.setTrace(False)
        ParserATNSimulator.trace_atn_sim = True
        PredictionContext._trace_atn_sim = True
    start_time = datetime.now()
    tree = parser.<start_symbol>()
    end_time = datetime.now()
    diff = end_time - start_time
    diff_time = diff.total_seconds()
    result = ''
    if listener_parser.had_error or listener_lexer.had_error:
        result = 'fail'
        error_code = 1
    else:
        result = 'success'
    if (show_tree):
        if (tee):
            f = open(input_name + '.tree', 'w', encoding='utf-8')
            f.write(tree.toStringTree(recog=parser))
            f.close()
        else:
            print(tree.toStringTree(recog=parser), file=sys.stderr)
    if (not quiet):
        sys.stderr.write(prefix)
        sys.stderr.write('Python3 ')
        sys.stderr.write(f'{row_number}')
        sys.stderr.write(' ')
        sys.stderr.write(input_name)
        sys.stderr.write(' ')
        sys.stderr.write(result)
        sys.stderr.write(' ')
        sys.stderr.write(f'{diff_time}')
        sys.stderr.write('\n')
    if (tee):
        output.close()

if __name__ == '__main__':
    main(sys.argv)
