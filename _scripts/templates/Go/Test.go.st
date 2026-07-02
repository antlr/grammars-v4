// Generated from trgen <version>

package main
import (
    "fmt"
    "os"
    "io"
    "time"
    "strconv"
    "bufio"
    "github.com/antlr4-go/antlr/v4"
    "example.com/Test/<package_name>"
)
type CustomErrorListener struct {
    errors int
    quiet bool
    tee bool
    output *os.File
}

func NewCustomErrorListener(q bool, t bool, o *os.File) *CustomErrorListener {
    p := new(CustomErrorListener)
    p.errors = 0
    p.quiet = q
    p.tee = t
    p.output = o
    return p
}

func (l *CustomErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line int, column int, msg string, e antlr.RecognitionException) {
    l.errors += 1
    if l.tee {
        l.output.WriteString("line ")
        l.output.WriteString(strconv.Itoa(line))
        l.output.WriteString(":")
        l.output.WriteString(strconv.Itoa(column))
        l.output.WriteString(" ")
        l.output.WriteString(msg)
        l.output.WriteString("\n")
    }
    if ! l.quiet {
        fmt.Fprintf(os.Stderr, "line %d:%d %s", line, column, msg)
        fmt.Fprintln(os.Stderr)
    }
}

func (l *CustomErrorListener) ReportAmbiguity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, exact bool, ambigAlts *antlr.BitSet, configs *antlr.ATNConfigSet) {
    antlr.ConsoleErrorListenerINSTANCE.ReportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs)
}

func (l *CustomErrorListener) ReportAttemptingFullContext(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, conflictingAlts *antlr.BitSet, configs *antlr.ATNConfigSet) {
    antlr.ConsoleErrorListenerINSTANCE.ReportAttemptingFullContext(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs)
}

func (l *CustomErrorListener) ReportContextSensitivity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex, prediction int, configs *antlr.ATNConfigSet) {
    antlr.ConsoleErrorListenerINSTANCE.ReportContextSensitivity(recognizer, dfa, startIndex, stopIndex, prediction, configs)
}

var inputs = make([]string, 0)
var is_fns = make([]bool, 0)
var error_code int = 0
var show_tree = false
var show_tokens = false
var show_trace = false
var string_instance = 0
var prefix = ""
var quiet = false
var tee = false
var binary = <binary>
var total_tokens int64 = 0
var total_parse_seconds float64 = 0
var first_file_tokens int64 = 0
var first_file_parse_seconds float64 = 0

func main() {
    for i := 1; i \< len(os.Args); i = i + 1 {
        if os.Args[i] == "-tokens" {
            show_tokens = true
        } else if os.Args[i] == "-tree" {
            show_tree = true
        } else if os.Args[i] == "-prefix" {
            i = i + 1
            prefix = os.Args[i] + " ";
        } else if os.Args[i] == "-input" {
            i = i + 1
            inputs = append(inputs, os.Args[i])
            is_fns = append(is_fns, false)
        } else if os.Args[i] == "-tee" {
            tee = true
        } else if os.Args[i] == "-x" {
            scanner := bufio.NewScanner(os.Stdin)
            for scanner.Scan() {
                line := scanner.Text()
                inputs = append(inputs, line)
                is_fns = append(is_fns, true)
            }
        } else if os.Args[i] == "-q" {
            quiet = true
        } else if os.Args[i] == "-trace" {
            show_trace = true
        } else if os.Args[i][0] == '-' {
            // Ignore unknown option.
        } else {
            inputs = append(inputs, os.Args[i])
            is_fns = append(is_fns, true)
        }
    }
    if len(inputs) == 0 {
        ParseStdin()
    } else {
        start := time.Now()
        for i := 0; i \< len(inputs); i = i + 1 {
            if is_fns[i] {
                ParseFilename(inputs[i], i)
            } else {
                ParseString(inputs[i], i)
            }
        }
        elapsed := time.Since(start)
        if ! quiet {
            overall_seconds := elapsed.Seconds()
            warm_tokens := total_tokens - first_file_tokens
            warm_seconds := total_parse_seconds - first_file_parse_seconds
            warm_tps := "n.a."
            if len(inputs) > 1 && warm_seconds > 0 {
                warm_tps = fmt.Sprintf("%.0f", float64(warm_tokens)/warm_seconds)
            }
            first_tps := 0.0
            if first_file_parse_seconds > 0 {
                first_tps = float64(first_file_tokens) / first_file_parse_seconds
            }
            speedup := "n.a."
            if len(inputs) > 1 && warm_seconds > 0 && first_tps > 0 {
                speedup = fmt.Sprintf("%.2f", (float64(warm_tokens)/warm_seconds)/first_tps)
            }
            fmt.Fprintf(os.Stderr, "%sPT: %.3f\n", prefix, total_parse_seconds)
            fmt.Fprintf(os.Stderr, "%sOT: %.3f\n", prefix, overall_seconds - total_parse_seconds)
            fmt.Fprintf(os.Stderr, "%sTT: %.3f\n", prefix, overall_seconds)
            fmt.Fprintf(os.Stderr, "%sTPS: %.0f\n", prefix, float64(total_tokens)/total_parse_seconds)
            fmt.Fprintf(os.Stderr, "%sPost-warmup TPS: %s\n", prefix, warm_tps)
            fmt.Fprintf(os.Stderr, "%sPost-warmup speed up: %s\n", prefix, speedup)
        }
    }
    if error_code != 0 {
        os.Exit(1)
    } else {
        os.Exit(0)
    }
}

func ParseStdin() {
    var b []byte = make([]byte, 1)
    var st = ""
    for {
        _, err := os.Stdin.Read(b)
        if err == io.EOF {
            break
        }
        st = st + string(b)
    }
    var str antlr.CharStream = nil
    str = antlr.NewInputStream(st)
    DoParse(str, "stdin", 0)
}

func ParseString(input string, row_number int) {
    str := antlr.NewInputStream(input)
    DoParse(str, "string" + strconv.Itoa(string_instance), row_number)
    string_instance = string_instance + 1
}

func ParseFilename(input string, row_number int) {
    encoding := "<file_encoding>"
    var str antlr.CharStream = nil
    if encoding != "" {
        str, _ = NewEncodingFileStream(input, encoding)
    } else {
        str, _ = antlr.NewFileStream(input)
    }
    DoParse(str, input, row_number)
}

func DoParse(str antlr.CharStream, input_name string, row_number int) {
    if binary {
        str = NewBinaryCharStream(str)
    }
    var lexer = <go_lexer_name>(str);
    if show_tokens {
        j := 0
        for {
            t := lexer.NextToken()
            t.SetTokenIndex(j)
            fmt.Fprintf(os.Stderr, "%s\n", t.String())
            if t.GetTokenType() == antlr.TokenEOF {
                break
            }
            j = j + 1
        }
        lexer.Reset()
    }
    // Requires additional 0??
    var tokens = antlr.NewCommonTokenStream(lexer, 0)
    var parser = <go_parser_name>(tokens)

    var (
        output *os.File
    )
    if tee {
        output, _ = os.Create(input_name + ".errors")
    }

    lexerErrors := NewCustomErrorListener(quiet, tee, output)
    lexer.RemoveErrorListeners()
    lexer.AddErrorListener(lexerErrors)

    parserErrors := NewCustomErrorListener(quiet, tee, output)
    parser.RemoveErrorListeners()
    parser.AddErrorListener(parserErrors)

    if show_trace {
        //parser.SetTrace(true)
        //antlr.ParserATNSimulatorTraceATNSim = true
    }
    // mutated name--not lowercase.
    start := time.Now()
    var tree = parser.<cap_start_symbol>()
    elapsed := time.Since(start)
    parse_seconds := elapsed.Seconds()
    total_parse_seconds += parse_seconds
    token_count := int64(len(tokens.GetAllTokens()))
    total_tokens += token_count
    if row_number == 0 {
        first_file_tokens = token_count
        first_file_parse_seconds = parse_seconds
    }
    var result = ""
    if parserErrors.errors > 0 || lexerErrors.errors > 0 {
        result = "fail"
        error_code = 1
    } else {
        result = "success"
    }
    if show_tree {
        ss := tree.ToStringTree(parser.RuleNames, parser)
        if tee {
            f, _ := os.Create(input_name + ".tree")
            f.Write([]byte(ss))
            f.Close()
        } else {
            fmt.Fprintf(os.Stderr, "%s", ss)
        }
    }
    if tee {
        output.Close()
    }
    if ! quiet {
        fmt.Fprintf(os.Stderr, "%s", prefix)
        fmt.Fprintf(os.Stderr, "Go ")
        fmt.Fprintf(os.Stderr, "%d ", row_number)
        fmt.Fprintf(os.Stderr, "%s ", input_name)
        fmt.Fprintf(os.Stderr, "%s ", result)
        fmt.Fprintf(os.Stderr, "%.3f s %d tokens %.0f tps", parse_seconds, token_count, float64(token_count)/parse_seconds)
        fmt.Fprintln(os.Stderr)
    }
}
