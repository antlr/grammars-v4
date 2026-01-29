import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'CLexer.dart';

class ErrorListener extends BaseErrorListener {
  bool hadError = false;
  final bool _quiet;
  final bool _tee;
  final IOSink? _out;

  ErrorListener(this._quiet, this._tee, this._out);

  @override
  void syntaxError(
    Recognizer<ATNSimulator> recognizer,
    Object? offendingSymbol,
    int? line,
    int charPositionInLine,
    String msg,
    RecognitionException? e,
  ) {
    var fileName = "<unknown>";
    var lineAdjusted = line ?? 0;

    // Get token stream.
    if (recognizer is Parser) {
      var p = recognizer;
      var q = offendingSymbol as Token?;
      if (q != null) {
        var ts2 = p.inputStream as CommonTokenStream;
        // Search back from offending symbol index to find last LineDirective.
        var ind = q.tokenIndex;
        for (var j = ind; j >= 0; j--) {
          var t = ts2.get(j);
          if (t.type == CLexer.TOKEN_LineDirective) {
            // Found it.
            var txt = t.text ?? "";
            var parts = txt.split(RegExp(r'\s+'));
            if (parts.length >= 3) {
              // Get line number from directive.
              var dirLine = int.tryParse(parts[1]);
              if (dirLine != null) {
                // Get line number of directive.
                var lineDirective = t.line ?? 0;
                // Get line difference from line directive.
                var lineDiff = (line ?? 0) - lineDirective;
                // Adjust line number.
                lineAdjusted = lineDiff + dirLine - 1;
                fileName = parts[2].trim();
              }
            }
            break;
          }
        }
      }
    }
    hadError = true;
    if (_tee && _out != null) {
      _out.writeln("$fileName line $lineAdjusted, .p $line:$charPositionInLine $msg");
    }
    if (!_quiet) {
      stderr.writeln("$fileName line $lineAdjusted, .p $line:$charPositionInLine $msg");
    }
  }
}
