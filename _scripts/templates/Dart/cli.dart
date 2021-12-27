// Template generated code from trgen <version>

import 'package:antlr4/antlr4.dart';
<tool_grammar_tuples:{x | import '<x.GeneratedFileName>';
} >
import 'dart:io';
import 'dart:convert';

<if (case_insensitive_type)>

/// This class supports case-insensitive lexing by wrapping an existing
/// {@link CharStream} and forcing the lexer to see either upper or
/// lowercase characters. Grammar literals should then be either upper or
/// lower case such as 'BEGIN' or 'begin'. The text of the character
/// stream is unaffected. Example: input 'BeGiN' would match lexer rule
/// 'BEGIN' if constructor parameter upper=true but getText() would return
/// 'BeGiN'.
class CaseChangingCharStream extends CharStream {
  final CharStream stream;
  final bool upper;

  /// Constructs a new CaseChangingCharStream wrapping the given [stream] forcing
  /// all characters to upper case or lower case depending on [upper].
  CaseChangingCharStream(this.stream, this.upper);

  @override
  int? LA(int i) {
    int? c = stream.LA(i);
    if (c == null || c \<= 0) {
      return c;
    }
    String newCaseStr;
    if (upper) {
      newCaseStr = String.fromCharCode(c).toUpperCase();
    } else {
      newCaseStr = String.fromCharCode(c).toLowerCase();
    }
    // Skip changing case if length changes (e.g., ß -> SS).
    if (newCaseStr.length != 1) {
      return c;
    } else {
      return newCaseStr.codeUnitAt(0);
    }
  }

  @override
  String get sourceName => stream.sourceName;

  @override
  void consume() => stream.consume();

  @override
  String getText(Interval interval) => stream.getText(interval);

  @override
  int get index => stream.index;

  @override
  int mark() => stream.mark();

  @override
  void release(int marker) => stream.release(marker);

  @override
  void seek(int index) => stream.seek(index);

  @override
  int get size => stream.size;
}

<endif>

void main(List\<String> args) async {
    var show_tree = false;
    var show_tokens = false;
    var file_name = null;
    var input = null;
    var str = null;
    for (int i = 0; i \< args.length; ++i)
    {
        if (args[i] == "-tokens")
        {
            show_tokens = true;
            continue;
        }
        else if (args[i] == "-tree")
        {
            show_tree = true;
            continue;
        }
        else if (args[i] == "-input")
            input = args[++i];
        else if (args[i] == "-file")
            file_name = args[++i];
    }
    <tool_grammar_tuples:{x|<x.GrammarAutomName>.checkVersion();
    }>
    if (input == null && file_name == null)
    {
        final List\<int> bytes = \<int>[];
        int byte = stdin.readByteSync();
        while (byte >= 0) {
            bytes.add(byte);
            byte = stdin.readByteSync();
        }
        input = utf8.decode(bytes);
        str = await InputStream.fromString(input);
    } else if (input != null)
    {
        str = await InputStream.fromString(input);
    } else if (file_name != null)
    {
        str = await InputStream.fromPath(file_name);        
    }
<if (case_insensitive_type)>
    str = CaseChangingCharStream(str, "<case_insensitive_type>" == "Upper");
<endif>
    var lexer = <lexer_name>(str);
    if (show_tokens)
    {
        for (int i = 0; ; ++i)
        {
            var token = lexer.nextToken();
            print(token.toString());
            if (token.type == -1)
                break;
        }
        lexer.reset();
    }
    var tokens = CommonTokenStream(lexer);
    var parser = <parser_name>(tokens);
//    var listener_lexer = ErrorListener();
//    var listener_parser = ErrorListener();
//    lexer.AddErrorListener(listener_lexer);
//    parser.AddErrorListener(listener_parser);
    var tree = parser.<start_symbol>();
    if (parser.numberOfSyntaxErrors > 0)
    {
        stderr.writeln("Parse failed.");
    }
    else
    {
        stderr.writeln("Parse succeeded.");
    }
    if (show_tree)
    {
        print(tree.toStringTree(parser: parser));
    }
    exit(parser.numberOfSyntaxErrors > 0 ? 1 : 0);
}
