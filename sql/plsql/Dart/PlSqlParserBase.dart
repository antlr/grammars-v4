import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';
import 'PlSqlLexer.dart';

abstract class PlSqlParserBase extends Parser
{
    bool _isVersion12 = true;
    bool _isVersion11 = true;
    bool _isVersion10 = true;

    bool _lastUnitWasPlsql = false;

    PlSqlParserBase(TokenStream input) : super(input)
    {
    }

    @override
    void reset([bool resetInput = true]) {
        _lastUnitWasPlsql = false;
        super.reset(resetInput);
    }

    void setLastUnitPlsql() { _lastUnitWasPlsql = true; }
    void setLastUnitSql()   { _lastUnitWasPlsql = false; }
    bool isLastUnitSql()    { return !_lastUnitWasPlsql; }
    bool isLastUnitPlsql()  { return _lastUnitWasPlsql; }

    bool isSolidusSeparator() {
        var stream = tokenStream as CommonTokenStream;
        var solidus = stream.LT(1);
        if (solidus == null || solidus!.type != PlSqlLexer.TOKEN_SOLIDUS)
            return false;

        int solidusLine = solidus!.line;

        var prev = stream.LT(-1);
        if (prev != null && prev!.type != Token.EOF && prev.line == solidusLine)
            return false;

        var next = stream.LT(2);
        if (next != null && next!.type != Token.EOF && next.line == solidusLine)
            return false;

        return true;
    }

    bool isVersion12() {
        return _isVersion12;
    }

    void setVersion12(bool value) {
        _isVersion12 = value;
    }

    bool isVersion11() {
        return _isVersion11;
    }

    void setVersion11(bool value) {
        _isVersion11 = value;
    }

    bool isVersion10() {
        return _isVersion10;
    }

    void setVersion10(bool value) {
        _isVersion10 = value;
    }

    bool IsNotNumericFunction() {
        var stream = tokenStream as CommonTokenStream;
        var lt1 = stream.LT(1);
        var lt2 = stream.LT(2);
        if ((lt1!.type == PlSqlLexer.TOKEN_SUM ||
             lt1.type == PlSqlLexer.TOKEN_COUNT ||
             lt1.type == PlSqlLexer.TOKEN_AVG ||
             lt1.type == PlSqlLexer.TOKEN_MIN ||
             lt1.type == PlSqlLexer.TOKEN_MAX ||
             lt1.type == PlSqlLexer.TOKEN_ROUND ||
             lt1.type == PlSqlLexer.TOKEN_LEAST ||
             lt1.type == PlSqlLexer.TOKEN_GREATEST) && lt2!.type == PlSqlLexer.TOKEN_LEFT_PAREN)
            return false;
        return true;
    }

    bool isNotStartOfJoin() {
        var stream = tokenStream as CommonTokenStream;
        var lt1 = stream.LT(1);
        if (lt1!.type == PlSqlLexer.TOKEN_INNER ||
            lt1.type == PlSqlLexer.TOKEN_CROSS ||
            lt1.type == PlSqlLexer.TOKEN_NATURAL ||
            lt1.type == PlSqlLexer.TOKEN_PARTITION ||
            lt1.type == PlSqlLexer.TOKEN_FULL ||
            lt1.type == PlSqlLexer.TOKEN_LEFT ||
            lt1.type == PlSqlLexer.TOKEN_RIGHT ||
            lt1.type == PlSqlLexer.TOKEN_OUTER)
            return false;
        return true;
    }
}
