import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class PlSqlParserBase extends Parser
{
    PlSqlParserBase self;
    bool _isVersion12 = true;
    bool _isVersion10 = true;

    PlSqlParserBase(TokenStream input) : super(input)
    {
        self = this;
    }

    bool isVersion12() {
        return _isVersion12;
    }

    void setVersion12(bool value) {
        _isVersion12 = value;
    }

    bool isVersion10() {
        return _isVersion10;
    }

    void setVersion10(bool value) {
        _isVersion10 = value;
    }
}
