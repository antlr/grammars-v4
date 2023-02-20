package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

type PostgreSQLParserErrorListener struct {
	grammar *PostgreSQLParser
}

var _ antlr.ErrorListener = &PostgreSQLParserErrorListener{}

func (receiver PostgreSQLParserErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, e antlr.RecognitionException) {
	receiver.grammar.parseErrors = append(receiver.grammar.parseErrors, &PostgreSQLParseError{
		Number:  0,
		Offset:  0,
		Line:    line,
		Column:  column,
		Message: msg,
	})
}

func (receiver PostgreSQLParserErrorListener) ReportAmbiguity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, exact bool, ambigAlts *antlr.BitSet, configs antlr.ATNConfigSet) {
	// ignore
}

func (receiver PostgreSQLParserErrorListener) ReportAttemptingFullContext(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, conflictingAlts *antlr.BitSet, configs antlr.ATNConfigSet) {
	// ignore
}

func (receiver PostgreSQLParserErrorListener) ReportContextSensitivity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex, prediction int, configs antlr.ATNConfigSet) {
	// ignore
}
