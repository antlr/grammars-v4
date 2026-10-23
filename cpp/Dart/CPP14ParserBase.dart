import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';
import 'CPP14Parser.dart';

abstract class CPP14ParserBase extends Parser
{
    CPP14ParserBase(TokenStream input) : super(input)
    {
    }

    bool IsPureSpecifierAllowed()
    {
        try
        {
            var x = this.context; // memberDeclarator
            var c = x?.getChild(0)?.getChild(0);
            var c2 = c?.getChild(0);
            var p = c2?.getChild(1);
            if (p == null) return false;
            return p.runtimeType == ParametersAndQualifiersContext;
        }
        catch (e)
        {
        }
        return false;
    }
}
