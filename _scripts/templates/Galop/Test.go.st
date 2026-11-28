// Generated from trgen <version>

package main

import (
    "bufio"
    "fmt"
    "io"
    "os"
    "path/filepath"
    "strconv"
    "strings"
    "time"
)

var inputs []string
var filenames []bool
var quiet, tee, treeOutput, tokenOutput, perf, perFile bool
var prefix, outputDir string
var totalTokens int64
var totalSeconds float64
var firstTokens int64
var firstSeconds float64
var failures int

func main() {
    for i := 1; i \< len(os.Args); i++ {
        arg := os.Args[i]
        switch arg {
        case "-tokens": tokenOutput = true
        case "-tree": treeOutput = true
        case "-tee": tee = true
        case "-q", "--no-output": quiet = true
        case "--perf": perf = true
        case "--per-file": perFile = true
        case "-trace": // Galop does not expose an execution trace.
        case "-prefix", "-input", "-o":
            i++
            if i >= len(os.Args) { fatal("missing value for " + arg) }
            value := os.Args[i]
            switch arg {
            case "-prefix": prefix = value + " "
            case "-input": inputs = append(inputs, value); filenames = append(filenames, false)
            case "-o": outputDir = value; tee = true
            }
        case "-x":
            reader := bufio.NewReader(os.Stdin)
            for {
                line, err := reader.ReadString('\n')
                line = strings.TrimSuffix(strings.TrimSuffix(line, "\n"), "\r")
                if line != "" { inputs = append(inputs, line); filenames = append(filenames, true) }
                if err == io.EOF { break }
                if err != nil { fatal(err.Error()) }
            }
        case "--sll": fatal("--sll is not supported by Galop")
        default:
            if strings.HasPrefix(arg, "-") { fatal("unknown option: " + arg) }
            inputs = append(inputs, arg); filenames = append(filenames, true)
        }
    }
    if len(inputs) == 0 {
        src, err := io.ReadAll(os.Stdin)
        if err != nil { fatal(err.Error()) }
        parse(string(src), "stdin", 0)
    } else {
        start := time.Now()
        for i, input := range inputs {
            if filenames[i] {
                src, err := os.ReadFile(input)
                if err != nil { reportFileError(input, i, err); continue }
                parse(string(src), input, i)
            } else {
                parse(input, "string" + strconv.Itoa(i), i)
            }
        }
        if !quiet {
            all := time.Since(start).Seconds()
            if perf {
                warmTokens := totalTokens - firstTokens
                warmSeconds := totalSeconds - firstSeconds
                warmRate, speedup := "n.a.", "n.a."
                if len(inputs) > 1 && warmSeconds > 0 {
                    warmRate = fmt.Sprintf("%.0f", float64(warmTokens)/warmSeconds)
                    if firstSeconds > 0 && firstTokens > 0 {
                        speedup = fmt.Sprintf("%.2f", (float64(warmTokens)/warmSeconds)/(float64(firstTokens)/firstSeconds))
                    }
                }
                fmt.Fprintf(os.Stderr, "%sPT: %.3f\n%sOT: %.3f\n%sTT: %.3f\n%sPR: %.0f\n%sPost-warmup PR: %s\n%sPost-warmup speed up: %s\n", prefix, totalSeconds, prefix, all-totalSeconds, prefix, all, prefix, rate(totalTokens,totalSeconds), prefix, warmRate, prefix, speedup)
            } else {
                fmt.Fprintf(os.Stderr, "%sTT: %.3f\n", prefix, all)
            }
        }
    }
    if failures != 0 { os.Exit(1) }
}

func fatal(message string) { fmt.Fprintln(os.Stderr, message); os.Exit(2) }
func rate(tokens int64, seconds float64) float64 {
    if seconds == 0 { return 0 }
    return float64(tokens)/seconds
}
func reportFileError(name string, row int, err error) {
    failures++
    if !quiet { fmt.Fprintf(os.Stderr, "%s: %v\n", name, err) }
    if !quiet && perFile { fmt.Fprintf(os.Stderr, "%sGalop %d %s failed\n", prefix, row, name) }
}

func parse(src, name string, row int) {
    out := name
    if outputDir != "" {
        abs, _ := filepath.Abs(name)
        root, _ := filepath.Abs("<source_example_dir_unix>")
        rel, err := filepath.Rel(root, abs)
        if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) || filepath.IsAbs(rel) { rel = filepath.Base(abs) }
        out = filepath.Join(outputDir, rel)
        if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil { fatal(err.Error()) }
    }
    start := time.Now()
    tokens := <grammar_name>Lex(src)
    result, parseErr := Parse<galop_start_symbol>(tokens)
    seconds := time.Since(start).Seconds()
    count := int64(len(tokens.Tokens) + len(tokens.Hidden))
    totalTokens += count
    totalSeconds += seconds
    if row == 0 { firstTokens = count; firstSeconds = seconds }

    if tokenOutput {
        index := 0
        for i, token := range tokens.Tokens {
            for _, hidden := range tokens.HiddenBefore(i) {
                fmt.Fprintf(os.Stderr, "[@%d,%d:%d=%q,\<%d>,%d:%d,channel=%d]\n", index, hidden.Start, hidden.End-1, hidden.Text(src), hidden.Type, hidden.Line, hidden.Column, hidden.Channel)
                index++
            }
            fmt.Fprintf(os.Stderr, "[@%d,%d:%d=%q,\<%d>,%d:%d]\n", index, token.Start, token.End-1, token.Text(src), token.Type, token.Line, token.Column)
            index++
        }
    }

    var messages []string
    for _, lexErr := range tokens.Errors { messages = append(messages, lexErr.Error()) }
    if parseErr != nil { messages = append(messages, parseErr.Error()) }
    if len(messages) != 0 { failures++ }
    if tee {
        body := ""
        if len(messages) != 0 { body = strings.Join(messages, "\n") + "\n" }
        if err := os.WriteFile(out+".errors", []byte(body), 0644); err != nil { fatal(err.Error()) }
    }
    if !quiet { for _, message := range messages { fmt.Fprintln(os.Stderr, message) } }
    if treeOutput && parseErr == nil {
        text := result.ToStringTree()
        if tee {
            if err := os.WriteFile(out+".tree", []byte(text), 0644); err != nil { fatal(err.Error()) }
        } else { fmt.Fprint(os.Stderr, text) }
    }
    if !quiet && perFile {
        status := "success"
        if len(messages) != 0 { status = "fail" }
        fmt.Fprintf(os.Stderr, "%sGalop %d %s %s %.3f s %d tokens %.0f pr\n", prefix, row, name, status, seconds, count, rate(count, seconds))
    }
}
