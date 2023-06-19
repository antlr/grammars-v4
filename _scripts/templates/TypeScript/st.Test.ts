// Generated from trgen <version>

import { CharStream } from 'antlr4';
import { CharStreams } from 'antlr4';
import { CommonTokenStream } from 'antlr4';
import { ErrorListener } from 'antlr4';
import { InputStream } from 'antlr4';
import { Recognizer } from 'antlr4';
import { RecognitionException } from 'antlr4';
import { Token } from 'antlr4';
import { readFileSync } from 'fs';
import { writeFileSync } from 'fs';
import { openSync } from 'fs';
import { readSync } from 'fs';
import { writeSync } from 'fs';
import { closeSync } from 'fs';
import { readFile } from 'fs/promises'

<tool_grammar_tuples: {x | import <x.GrammarAutomName> from './<x.GrammarAutomName>';
} >
import { StringBuilder, emptyString, joinString, formatString, isNullOrWhiteSpace } from 'typescript-string-operations';
import { Timer, Time, TimerOptions } from 'timer-node';


function getChar() {
    let buffer = Buffer.alloc(1);
    var xx = 0;
    try {
        xx = readSync(0, buffer, 0, 1, null);
    } catch (err) {
    }
    if (xx === 0) {
        return '';
    }
    return buffer.toString('utf8');
}


class MyErrorListener\<T> extends ErrorListener\<T> {
    _quiet: boolean;
    _tee: boolean;
    _output: any;
    had_error: boolean;

    constructor(quiet: boolean, tee: boolean, output: any) {
        super();
        this._quiet = quiet;
        this._tee = tee;
        this._output = output;
        this.had_error = false;
    }

    syntaxError(recognizer: Recognizer\<T>, offendingSymbol: T, line: number, column: number, msg: string, e: RecognitionException | undefined): void {
        this.had_error = true;
        if (this._tee) {
            writeSync(this._output, `line ${line}:${column} ${msg}\n`);
        }
        if (!this._quiet) {
            console.error(`line ${line}:${column} ${msg}`);
        }
    }
}

var tee = false;
var show_profile = false;
var show_tree = false;
var show_tokens = false;
var show_trace = false;
var error_code = 0;
var quiet = false;
var encoding = 'utf8';
var string_instance = 0;
var prefix = '';
var inputs: string[] = [];
var is_fns: boolean[] = [];

function splitLines(t: string) { return t.split(/\r\n|\r|\n/); }

function main() {
    for (let i = 2; i \<process.argv.length; ++i)
    {
        switch (process.argv[i]) {
            case '-tokens':
                show_tokens = true;
                break;
            case '-tree':
                show_tree = true;
                break;
            case '-prefix':
                prefix = process.argv[++i] + ' ';
                break;
            case '-input':
                inputs.push(process.argv[++i]);
                is_fns.push(false);
                break;
            case '-tee':
                tee = true;
                break;
            case '-encoding':
                encoding = process.argv[++i];
                break;
            case '-x':
                var sb = new StringBuilder();
                var ch;
                while ((ch = getChar()) != '') {
                    sb.Append(ch);
                }
                var input = sb.ToString();
                var sp = splitLines(input);
                for (var ii of sp) {
                    if (ii == '') continue;
                    inputs.push(ii);
                    is_fns.push(true);
                }
                break;
            case '-q':
                quiet = true;
                break;
            case '-trace':
                show_trace = true;
                break;
            default:
                inputs.push(process.argv[i]);
                is_fns.push(true);
                break;
        }
    }
    if (inputs.length == 0) {
        ParseStdin();
    }
    else {
        const timer = new Timer({ label: 'test-timer' });
        timer.start();
        for (var f = 0; f \<inputs.length; ++f)
        {
            if (is_fns[f])
                ParseFilename(inputs[f], f);
            else
                ParseString(inputs[f], f);
        }
        timer.stop();
        var t = timer.time().m * 60 + timer.time().s + timer.time().ms / 1000;
        if (!quiet) console.error('Total Time: ' + t);
    }
    process.exitCode = error_code;
}

function ParseStdin() {
    var sb = new StringBuilder();
    var ch;
    while ((ch = getChar()) != '') {
        sb.Append(ch);
    }
    var input = sb.ToString();
    var str = CharStreams.fromString(input);
    DoParse(str, "stdin", 0);
}

function ParseString(input: string, row_number: number) {
    var str = CharStreams.fromString(input);
    DoParse(str, "string" + string_instance++, row_number);
}

function ParseFilename(input: string, row_number: number) {
    var str = CharStreams.fromPathSync(input, encoding);
    DoParse(str, input, row_number);
}

function DoParse(str: CharStream, input_name: string, row_number: number) {
    const lexer = new <lexer_name>(str);
    const tokens = new CommonTokenStream(lexer);
    const parser = new <parser_name>(tokens);
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    var output = tee ? openSync(input_name + ".errors", 'w') : 1;
    var listener_parser = new MyErrorListener(quiet, tee, output);
    var listener_lexer = new MyErrorListener(quiet, tee, output);
    parser.addErrorListener(listener_parser);
    lexer.addErrorListener(listener_lexer);
    if (show_tokens) {
        for (var i = 0; ; ++i) {
            var ro_token = lexer.nextToken();
            var token = ro_token;
            token.tokenIndex = i;
            console.error(token.toString());
            if (token.type === Token.EOF)
                break;
        }
//        lexer.reset();
    }
    if (show_trace) {
//       parser._interp.trace_atn_sim = true;
    }
    const timer = new Timer({ label: 'test-timer2' });
    timer.start();
    const tree = parser.<start_symbol>();
    timer.stop();
    var result = "";
    if (listener_parser.had_error || listener_lexer.had_error) {
        result = 'fail';
        error_code = 1;
    }
    else {
        result = 'success';
    }
    var t = timer.time().m * 60 + timer.time().s + timer.time().ms / 1000;
    if (show_tree) {
        if (tee) {
            writeFileSync(input_name + ".tree", tree.toStringTree(parser.ruleNames, parser));
        } else {
            console.error(tree.toStringTree(parser.ruleNames, parser));
        }
    }
    if (!quiet) {
        console.error(prefix + 'TypeScript ' + row_number + ' ' + input_name + ' ' + result + ' ' + t);
    }
    if (tee) {
        closeSync(output);
    }
}


main()
