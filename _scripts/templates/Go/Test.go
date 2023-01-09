// Template generated code from trgen <version>

package main
import (
    "fmt"
    "os"
    "io"
    "time"
    "strconv"
    "bufio"
    "github.com/antlr/antlr4/runtime/Go/antlr/v4"
    "example.com/myparser/<package_name>"
)
type CustomErrorListener struct {
    errors int
    quiet bool
    output io.Writer
}

func NewCustomErrorListener(q bool, o io.Writer) *CustomErrorListener {
    p := new(CustomErrorListener)
    p.quiet = q
    p.errors = 0
    p.output = o
    return p
}

func (l *CustomErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line int, column int, msg string, e antlr.RecognitionException) {
    l.errors += 1
    if ! l.quiet {
        fmt.Fprintf(l.output, "line %d:%d %s", line, column, msg)
	fmt.Fprintln(l.output)
    }
}

func (l *CustomErrorListener) ReportAmbiguity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, exact bool, ambigAlts *antlr.BitSet, configs antlr.ATNConfigSet) {
    antlr.ConsoleErrorListenerINSTANCE.ReportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs)
}

func (l *CustomErrorListener) ReportAttemptingFullContext(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, conflictingAlts *antlr.BitSet, configs antlr.ATNConfigSet) {
    antlr.ConsoleErrorListenerINSTANCE.ReportAttemptingFullContext(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs)
}

func (l *CustomErrorListener) ReportContextSensitivity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex, prediction int, configs antlr.ATNConfigSet) {
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
var shunt_output = false

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
        } else if os.Args[i] == "-shunt" {
            shunt_output = true
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
        fmt.Fprintf(os.Stderr, "Total Time: %.3f s", elapsed.Seconds())
        fmt.Fprintln(os.Stderr)
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
    var str antlr.CharStream = nil
    str, _ = antlr.NewFileStream(input)        
    DoParse(str, input, row_number)
}

func DoParse(str antlr.CharStream, input_name string, row_number int) {
    var lexer = <go_lexer_name>(str);
    if show_tokens {
        j := 0
        for {
            t := lexer.NextToken()
            fmt.Print(j)
            fmt.Print(" ")
            // missing fmt.Println(t.String())
            fmt.Println(t.GetText())
            if t.GetTokenType() == antlr.TokenEOF {
                break
            }
            j = j + 1
        }
        // missing lexer.Reset()
    }
    // Requires additional 0??
    var tokens = antlr.NewCommonTokenStream(lexer, 0)
    var parser = <go_parser_name>(tokens)

    var (
        output io.Writer
    )
    if shunt_output {
        f, _ := os.Create(input_name + ".errors")
        defer f.Close()
        output = bufio.NewWriter(f)
    } else {
        output = os.Stdout
    }

    lexerErrors := NewCustomErrorListener(quiet, output)
    lexer.RemoveErrorListeners()
    lexer.AddErrorListener(lexerErrors)

    parserErrors := NewCustomErrorListener(quiet, output)
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
    var result = ""
    if parserErrors.errors > 0 || lexerErrors.errors > 0 {
        result = "fail"
        error_code = 1
    } else {
        result = "success"
    }
    if show_tree {
        ss := tree.ToStringTree(parser.RuleNames, parser)
        if shunt_output {
            f, _ := os.Create(input_name + ".tree")
            defer f.Close()
            w := bufio.NewWriter(f)
            fmt.Fprintln(w, ss)
            w.Flush()
        } else {
            fmt.Println(ss)
        }
    }
    if ! quiet {
        fmt.Fprintf(os.Stderr, "%s", prefix)
        fmt.Fprintf(os.Stderr, "Go ")
        fmt.Fprintf(os.Stderr, "%d ", row_number)
        fmt.Fprintf(os.Stderr, "%s ", input_name)
        fmt.Fprintf(os.Stderr, "%s ", result)
        fmt.Fprintf(os.Stderr, "%.3f", elapsed.Seconds())
        fmt.Fprintln(os.Stderr)
    }
}
