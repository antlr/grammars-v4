/*
[The "BSD licence"]
Copyright (c) 2014 Vlad Shlosberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import static org.junit.Assert.fail;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.DiagnosticErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

public class TestBase {

  protected ScssParser.StylesheetContext parse(String... lines) {
    StringBuilder fileLines = new StringBuilder();
    for (String line : lines) {
      fileLines.append(line);
    }

    CharStream inputStream = CharStreams.fromString(fileLines.toString());

    ScssLexer lexer = new ScssLexer(inputStream);
    TokenStream stream = new BufferedTokenStream(lexer);
    ScssParser par = new ScssParser(stream);
    par.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);

    par.addErrorListener(new DiagnosticErrorListener());
    par.addErrorListener(new FailOnErrorListener());
    return par.stylesheet();
  }

  private static class FailOnErrorListener extends BaseErrorListener {
    @Override
    public void syntaxError(
        Recognizer<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e) {
      // These two messages don't indicate problems.
      if (msg.startsWith("reportAttemptingFullContext")
          || msg.startsWith("reportContextSensitivity")) {
        return;
      }

      fail("Syntax error in detection. line " + line + ":" + charPositionInLine + ". " + msg);
    }
  }

  private String getExpressionMap(ParseTree start, int tabs) {
    if (start == null) {
      return "";
    }

    StringBuilder bld = new StringBuilder();
    if (start instanceof TerminalNode) {
      for (int t = 0; t < tabs; t++) {
        bld.append("  ");
      }
      bld.append(start.getText());
      bld.append("\n");
    }
    int count = start.getChildCount();
    for (int i = 0; i < count; i++) {
      ParseTree child = start.getChild(i);
      for (int t = 0; t < tabs; t++) {
        bld.append("  ");
      }

      bld.append(child.getClass().getSimpleName())
          .append(":\n")
          .append(getExpressionMap(child, tabs + 1));
    }
    return bld.toString();
  }
}
