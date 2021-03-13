// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

import 'package:antlr4/antlr4.dart';
<tool_grammar_tuples:{x | import '<x.GeneratedFileName>';
} >
import 'dart:io';
import 'dart:convert';

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
