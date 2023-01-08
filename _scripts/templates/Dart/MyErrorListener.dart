/*
 * Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */

import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

/// This is all the parsing support code essentially; most of it is error recovery stuff. */
class MyErrorListener extends BaseErrorListener
{
  bool _quiet = false;
  bool had_error = false;
  IOSink _output = stdout;

  MyErrorListener(bool quiet, IOSink output)
  {
      _quiet = quiet;
      had_error = false;
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
    if (!_quiet)
    {
        _output.writeln('line $line:$charPositionInLine $msg');
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
