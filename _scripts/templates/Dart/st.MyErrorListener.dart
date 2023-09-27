// Generated from trgen <version>

import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

/// This is all the parsing support code essentially; most of it is error recovery stuff. */
class MyErrorListener extends BaseErrorListener
{
    bool _quiet = false;
    bool had_error = false;
    bool _tee = false;
    IOSink _output = stdout;

    MyErrorListener(bool quiet, bool tee, IOSink output)
    {
        had_error = false;
        _quiet = quiet;
        _tee = tee;
        _output = output;
    }

    @override
    void syntaxError(
        Recognizer recognizer,
        Object? offendingSymbol,
        int? line,
        int charPositionInLine,
        String msg,
        RecognitionException? e,
    ) {
        had_error = true;
        if (_tee)
        {
            _output.writeln('line $line:$charPositionInLine $msg');
        }
        if (!_quiet)
        {
            stderr.writeln('line $line:$charPositionInLine $msg');
        }
    }

    /* @override
    void reportAmbiguity(
        Parser recognizer,
        DFA dfa,
        int startIndex,
        int stopIndex,
        bool exact,
        BitSet? ambigAlts,
        ATNConfigSet configs,
    ) {}
    */

/*  @override
    void reportAttemptingFullContext(
        Parser recognizer,
        DFA dfa,
        int startIndex,
        int stopIndex,
        BitSet? conflictingAlts,
        ATNConfigSet configs,
    ) {}
    */

/*
    @override
    void reportContextSensitivity(
        Parser recognizer,
        DFA dfa,
        int startIndex,
        int stopIndex,
        int prediction,
        ATNConfigSet configs,
    ) {}
    */
}
