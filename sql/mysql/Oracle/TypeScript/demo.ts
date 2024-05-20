/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

/* cspell: ignore antlr, armscii, keybcs, ujis, macce, macroman, geostd, eucjpms, sakila */

import { CharStream, CommonTokenStream, } from "antlr4ng";

import { MySQLLexer } from "./MySQLLexer.js";
import { MySQLParser } from "./MySQLParser.js";

// A list of character sets supported by MySQL. These are used for the lexer to handle string repertoires.
const charSets = new Set<string>([
    "_big5", "_dec8", "_cp850", "_hp8", "_koi8r", "_latin1", "_latin2", "_swe7", "_ascii", "_ujis",
    "_sjis", "_hebrew", "_tis620", "_euckr", "_koi8u", "_gb18030", "_gb2312", "_greek", "_cp1250", "_gbk",
    "_latin5", "_armscii8", "_utf8", "_ucs2", "_cp866", "_keybcs2", "_macce", "_macroman", "_cp852", "_latin7",
    "_cp1251", "_cp1256", "_cp1257", "_binary", "_geostd8", "_cp932", "_eucjpms", "_utf8mb4", "_utf16", "_utf32",
]);

const lexer = new MySQLLexer(CharStream.fromString("select * from sakila.actor where actor_id = 1;"));
const tokenStream = new CommonTokenStream(lexer);
const parser = new MySQLParser(tokenStream);
lexer.serverVersion = 80200;
lexer.sqlModeFromString("ANSI_QUOTES");
parser.serverVersion = lexer.serverVersion;
parser.sqlModes = lexer.sqlModes;
const tree = parser.query();

console.log(tree.toStringTree(parser));
