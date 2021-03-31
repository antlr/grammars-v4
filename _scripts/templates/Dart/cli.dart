// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

import 'package:antlr4/antlr4.dart';
<tool_grammar_tuples:{x | import '<x.GeneratedFileName>';
} >
import 'dart:io';
import 'dart:convert';

<if (case_insensitive_type)>

class CaseChangingCharStream extends CharStream {
        CharStream stream;
        bool upper;

	CaseChangingCharStream(CharStream str, bool up)
	{
		stream = str;
		upper = up;
	}

	@override
	int get index {
		return stream.index;
	}

	@override
	int get size {
		return stream.size;
	}

//	@override
//	void reset() {
//		stream.reset();
//	}

	@override
	void consume() {
		stream.consume();
	}

	@override
	int LA(int offset) {
            int c = stream.LA(offset);

            if (c \<= 0)
            {
                return c;
            }

            int o = c;
            if (upper)
            {
                // Dart extremely painful--there is no simple function that
                // maps a single character to upper-/lower- case. Do this the
                // old fashion way to just get some damn thing working. I am
                // not an expert at pouring over 1000's of web pages to find the
                // function and SO has nothing.
                if (97 \<= o && o \<= 122)
                    o = o + (65 - 97);
                return o;
            }
            else {
                if (65 \<= o && o \<= 90)
                    o = o + (97 - 65);
                return o;
            }
	}

	@override
	int mark() {
		return stream.mark();
	}

	@override
	void release(int marker) {
		stream.release(marker);
	}

	@override
	void seek(int _index) {
		stream.seek(_index);
	}

//	String getText(Interval interval)
//	{
//		this.stream.getText(interval);
//	}

	@override
	String toString() {
		return this.stream.toString();
	}

	@override
	String get sourceName {
		return stream.sourceName;
	}

@override
    noSuchMethod(Invocation msg) => "got ${msg.memberName} "
                      "with arguments ${msg.positionalArguments}";
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
        print("Parse failed.");
    }
    else
    {
        print("Parse succeeded.");
    }
    if (show_tree)
    {
        print(tree.toStringTree(parser: parser));
    }
    exit(parser.numberOfSyntaxErrors > 0 ? 1 : 0);
}
