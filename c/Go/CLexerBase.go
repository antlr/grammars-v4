package parser

import (
	"os"
	"os/exec"
	"runtime"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

type CLexerBase struct {
	*antlr.BaseLexer
	preprocessed bool
}

// NextToken overrides BaseLexer.NextToken to run the C preprocessor once
// before any token is consumed, mirroring what the C# and Java CLexerBase
// constructors do with RunGccAndMakeStream / runGccAndMakeStream.
func (l *CLexerBase) NextToken() antlr.Token {
	if !l.preprocessed {
		l.preprocessed = true
		newInput := runGccAndMakeStream(l.GetInputStream())
		l.SetInputStream(newInput)
	}
	return l.BaseLexer.NextToken()
}

func runGccAndMakeStream(input antlr.CharStream) antlr.CharStream {
	isWindows := runtime.GOOS == "windows"
	args := os.Args

	vsc := hasArg(args, "--vsc")
	gcc := hasArg(args, "--gcc")
	clang := hasArg(args, "--clang")
	nopp := hasArg(args, "--nopp")

	if !(vsc || gcc || clang) {
		gcc = true
	}

	ppOptions := extractPreprocessorOptions(args)

	sourceName := input.GetSourceName()
	if sourceName == "" || sourceName == "<unknown source>" || !strings.HasSuffix(sourceName, ".c") {
		sourceName = "stdin.c"
	}
	outputName := sourceName + ".p"
	inputText := input.GetText(0, input.Size()-1)

	if nopp {
		_ = os.WriteFile(outputName, []byte(inputText), 0644)
		return antlr.NewInputStream(inputText)
	}

	if sourceName == "stdin.c" {
		_ = os.WriteFile(sourceName, []byte(inputText), 0644)
	}

	if gcc {
		gccCmd := "gcc"
		if isWindows {
			gccCmd = "gcc.exe"
		}
		cmdArgs := []string{"-std=c2x", "-E", "-C"}
		cmdArgs = append(cmdArgs, ppOptions...)
		cmdArgs = append(cmdArgs, sourceName)
		output, err := exec.Command(gccCmd, cmdArgs...).Output()
		if err == nil && len(output) > 0 {
			_ = os.WriteFile(outputName, output, 0644)
			return antlr.NewInputStream(string(output))
		}
		// Fall back to original input if preprocessing fails.
		return antlr.NewInputStream(inputText)
	}

	if clang {
		clangCmd := "clang"
		if isWindows {
			clangCmd = "clang.exe"
		}
		cmdArgs := []string{"-std=c2x", "-E", "-C"}
		cmdArgs = append(cmdArgs, ppOptions...)
		cmdArgs = append(cmdArgs, sourceName)
		output, err := exec.Command(clangCmd, cmdArgs...).Output()
		if err == nil && len(output) > 0 {
			_ = os.WriteFile(outputName, output, 0644)
			return antlr.NewInputStream(string(output))
		}
		// Fall back to original input if preprocessing fails.
		return antlr.NewInputStream(inputText)
	}

	panic("no preprocessor specified")
}

func hasArg(args []string, arg string) bool {
	for _, a := range args {
		if strings.Contains(strings.ToLower(a), arg) {
			return true
		}
	}
	return false
}

func extractPreprocessorOptions(args []string) []string {
	var options []string
	for _, arg := range args {
		if strings.HasPrefix(arg, "--D") {
			options = append(options, "-D"+arg[3:])
		} else if strings.HasPrefix(arg, "--I") {
			options = append(options, "-I"+arg[3:])
		}
	}
	return options
}
