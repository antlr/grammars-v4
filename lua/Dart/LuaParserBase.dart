import 'package:antlr4/antlr4.dart';
import 'LuaParser.dart';
import 'dart:collection';

abstract class LuaParserBase extends Parser
{
    final bool debug = false;

    LuaParserBase(TokenStream input) : super(input);

    bool IsFunctionCall()
    {
        final la = this.tokenStream.LT(1);
        if (la?.type != LuaParser.TOKEN_NAME) return false;
        final la2 = this.tokenStream.LT(2);
        if (la2?.type == LuaParser.TOKEN_OP) return false;
	return true;
    }
}
