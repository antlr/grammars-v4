/*
PostgreSQL grammar.
The MIT License (MIT).
Copyright (c) 2021-2023, Oleksii Kovalov (Oleksii.Kovalov@outlook.com).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.antlr.v4.runtime.*;

public abstract class PostgreSQLParserBase extends Parser {

    public PostgreSQLParserBase(TokenStream input) {
        super(input);
    }

    ParserRuleContext GetParsedSqlTree(String script, int line) {
        PostgreSQLParser ph = getPostgreSQLParser(script);
        ParserRuleContext result = ph.root();
        return result;
    }

    public void ParseRoutineBody(PostgreSQLParser.Createfunc_opt_listContext _localctx) {
        String lang = null;
        for (PostgreSQLParser.Createfunc_opt_itemContext coi : _localctx.createfunc_opt_item()) {
            if (coi.LANGUAGE() != null) {
                if (coi.nonreservedword_or_sconst() != null)
                    if (coi.nonreservedword_or_sconst().nonreservedword() != null)
                        if (coi.nonreservedword_or_sconst().nonreservedword().identifier() != null)
                            if (coi.nonreservedword_or_sconst().nonreservedword().identifier()
                                    .Identifier() != null) {
                                lang = coi.nonreservedword_or_sconst().nonreservedword().identifier()
                                        .Identifier().getText();
                                break;
                            }
            }
        }
        if (null == lang) return;
        PostgreSQLParser.Createfunc_opt_itemContext func_as = null;
        for (PostgreSQLParser.Createfunc_opt_itemContext a : _localctx.createfunc_opt_item()) {
            if (a.func_as() != null) {
                func_as = a;
                break;

            }

        }
        if (func_as != null) {
            String txt = GetRoutineBodyString(func_as.func_as().sconst(0));
            PostgreSQLParser ph = getPostgreSQLParser(txt);
            switch (lang) {
                case "plpgsql":
                    func_as.func_as().Definition = ph.plsqlroot();
                    break;
                case "sql":
                    func_as.func_as().Definition = ph.root();
                    break;
            }
        }
    }

    private String TrimQuotes(String s) {
        return (s == null || s.isEmpty()) ? s : s.substring(1, s.length() - 1);
    }

    public String unquote(String s) {
        int slength = s.length();
        StringBuilder r = new StringBuilder(slength);
        int i = 0;
        while (i < slength) {
            Character c = s.charAt(i);
            r.append(c);
            if (c == '\'' && i < slength - 1 && (s.charAt(i + 1) == '\'')) i++;
            i++;
        }
        return r.toString();
    }

    public String GetRoutineBodyString(PostgreSQLParser.SconstContext rule) {
        PostgreSQLParser.AnysconstContext anysconst = rule.anysconst();
        org.antlr.v4.runtime.tree.TerminalNode StringConstant = anysconst.StringConstant();
        if (null != StringConstant) return unquote(TrimQuotes(StringConstant.getText()));
        org.antlr.v4.runtime.tree.TerminalNode UnicodeEscapeStringConstant = anysconst.UnicodeEscapeStringConstant();
        if (null != UnicodeEscapeStringConstant) return TrimQuotes(UnicodeEscapeStringConstant.getText());
        org.antlr.v4.runtime.tree.TerminalNode EscapeStringConstant = anysconst.EscapeStringConstant();
        if (null != EscapeStringConstant) return TrimQuotes(EscapeStringConstant.getText());
        String result = "";
        List<org.antlr.v4.runtime.tree.TerminalNode> dollartext = anysconst.DollarText();
        for (org.antlr.v4.runtime.tree.TerminalNode s : dollartext) {
            result += s.getText();
        }
        return result;
    }

    public PostgreSQLParser getPostgreSQLParser(String script) {
        CharStream charStream = CharStreams.fromString(script);
        Lexer lexer = new PostgreSQLLexer(charStream);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        PostgreSQLParser parser = new PostgreSQLParser(tokens);
        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        LexerDispatchingErrorListener listener_lexer = new LexerDispatchingErrorListener((Lexer)(((CommonTokenStream)(this.getInputStream())).getTokenSource()));
        ParserDispatchingErrorListener listener_parser = new ParserDispatchingErrorListener(this);
        lexer.addErrorListener(listener_lexer);
        parser.addErrorListener(listener_parser);
        return parser;
    }
}
