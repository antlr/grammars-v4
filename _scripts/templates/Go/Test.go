// Template generated code from trgen <version>

package main
import (
    "fmt"
    "os"
    "io"
    "time"
    "github.com/antlr/antlr4/runtime/Go/antlr/v4"
    "example.com/myparser/<package_name>"
<if (case_insensitive_type)>
    "example.com/myparser/antlr_resource"
<endif>
)
type CustomErrorListener struct {
    errors int
}

func NewCustomErrorListener() *CustomErrorListener {
    return new(CustomErrorListener)
}

func (l *CustomErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, e antlr.RecognitionException) {
    l.errors += 1
    fmt.Printf("line %d:%d %s", line, column, msg)
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


func main() {
    var show_tree = false
    var show_tokens = false
    var file_name = ""
    var input = ""
    var str antlr.CharStream = nil
    for i := 0; i \< len(os.Args); i = i + 1 {
        if os.Args[i] == "-tokens" {
            show_tokens = true
            continue
        } else if os.Args[i] == "-tree" {
            show_tree = true
            continue
        } else if os.Args[i] == "-input" {
            i = i + 1
            input = os.Args[i]
        } else if os.Args[i] == "-file" {
            i = i + 1
            file_name = os.Args[i]
        }
    }
    if input == "" && file_name == "" {
        var b []byte = make([]byte, 1)
        var st = ""
        for {
            _, err := os.Stdin.Read(b)
            if err == io.EOF {
                break
            }
            st = st + string(b)
        }
        str = antlr.NewInputStream(st)
    } else if input != "" {
        str = antlr.NewInputStream(input)
    } else if file_name != "" {
        str, _ = antlr.NewFileStream(file_name);        
    }
<if (case_insensitive_type)>
    str = antlr_resource.NewCaseChangingStream(str, "<case_insensitive_type>" == "Upper");
<endif>
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

    lexerErrors := &CustomErrorListener{}
    lexer.RemoveErrorListeners()
    lexer.AddErrorListener(lexerErrors)

    parserErrors := &CustomErrorListener{}
    parser.RemoveErrorListeners()
    parser.AddErrorListener(parserErrors)

    // mutated name--not lowercase.
    start := time.Now()
    var tree = parser.<cap_start_symbol>()
    elapsed := time.Since(start)
    fmt.Fprintf(os.Stderr, "Time: %.3f s", elapsed.Seconds())
    fmt.Fprintln(os.Stderr)
    if parserErrors.errors > 0 || lexerErrors.errors > 0 {
        fmt.Fprintln(os.Stderr, "Parse failed.");
    } else {
        fmt.Fprintln(os.Stderr, "Parse succeeded.")
        if show_tree {
            ss := tree.ToStringTree(parser.RuleNames, parser)
            fmt.Println(ss)
        }
    }
    if parserErrors.errors > 0 || lexerErrors.errors > 0 {
        os.Exit(1)
    } else {
        os.Exit(0)
    }
}
