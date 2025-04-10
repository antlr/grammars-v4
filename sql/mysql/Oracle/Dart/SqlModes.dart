import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'MySQLLexer.dart';
import 'MySQLParser.dart';
import 'SqlMode.dart';

class SqlModes
{
    static HashSet<SqlMode> sqlModeFromString(String modes)
    {
        var result = HashSet<SqlMode>();
        List<String> parts = modes.toUpperCase().split(',');
        for (String mode in parts)
	{
            if (['ANSI', 'DB2', 'MAXDB', 'MSSQL', 'ORACLE', 'POSTGRESQL'].contains(mode))
	    {
		result.add(SqlMode.ansiQuotes);
                result.add(SqlMode.pipesAsConcat);
                result.add(SqlMode.ignoreSpace);
            } else if (mode == 'ANSI_QUOTES') {
                result.add(SqlMode.ansiQuotes);
            } else if (mode == 'PIPES_AS_CONCAT') {
                result.add(SqlMode.pipesAsConcat);
            } else if (mode == 'NO_BACKSLASH_ESCAPES') {
                result.add(SqlMode.noBackslashEscapes);
            } else if (mode == 'IGNORE_SPACE') {
                result.add(SqlMode.ignoreSpace);
            } else if (['HIGH_NOT_PRECEDENCE', 'MYSQL323', 'MYSQL40']
                .contains(mode)) {
                result.add(SqlMode.highNotPrecedence);
            }
        }
        return result;
    }
}
