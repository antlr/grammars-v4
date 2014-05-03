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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.misc.Nullable;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.testng.Assert;

public class TestBase
{
  protected void assertEqualsNoOrder(List actual, List expected)
  {
    Assert.assertEqualsNoOrder(actual.toArray(), expected.toArray(), "Not Equal Expected: " +  Arrays.toString(expected.toArray()) + " Actual: " + Arrays.toString(
        actual.toArray()));
  }

  protected ScssParser.StylesheetContext parse(String ... lines)
  {
    try
    {
      StringBuilder fileLines = new StringBuilder();
      for (String line : lines)
      {
        fileLines.append(line);
      }

      ANTLRInputStream source = new ANTLRInputStream(new ByteArrayInputStream(fileLines.toString().getBytes()));

      ScssLexer lexer = new ScssLexer(source);
      TokenStream stream = new BufferedTokenStream(lexer);
      ScssParser par = new ScssParser(stream);
      par.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);

      par.addErrorListener(new FailOnErrorListener());
      return par.stylesheet();
    }
    catch (IOException e)
    {
      Assert.fail("Exception should not happen here", e);
    }
    return null;
  }

  private class FailOnErrorListener implements ANTLRErrorListener
  {
    @Override
    public void syntaxError(@NotNull Recognizer<?, ?> recognizer,
                            @Nullable Object o,
                            int i,
                            int i2,
                            @NotNull String s,
                            @Nullable RecognitionException e)
    {
      Assert.fail("Syntax error in detection.");
    }

    @Override
    public void reportAmbiguity(@NotNull Parser parser,
                                @NotNull DFA dfa,
                                int i,
                                int i2,
                                boolean b,
                                @Nullable BitSet bitSet,
                                @NotNull ATNConfigSet atnConfigs)
    {
      Assert.fail("Ambiguity error in detection.");

    }

    @Override
    public void reportAttemptingFullContext(@NotNull Parser parser,
                                            @NotNull DFA dfa,
                                            int i,
                                            int i2,
                                            @Nullable BitSet bitSet,
                                            @NotNull ATNConfigSet atnConfigs)
    {
      Assert.fail("Attempting full context error in detection.");

    }

    @Override
    public void reportContextSensitivity(@NotNull Parser parser,
                                         @NotNull DFA dfa,
                                         int i,
                                         int i2,
                                         int i3,
                                         @NotNull ATNConfigSet atnConfigs)
    {
      Assert.fail("Context sensitivity error in detection.");

    }
  }


  private String getExpressionMap(ParseTree start, int tabs)
  {
    if (start == null)
    {
      return "";
    }

    StringBuilder bld = new StringBuilder();
    if (TerminalNode.class.isAssignableFrom(start.getClass()))
    {
      for (int t = 0; t < tabs; t++)
      {
        bld.append("  ");
      }
      bld.append(start.getText());
      bld.append("\n");

    }
    int count = start.getChildCount();
    for (int i = 0; i < count; i++)
    {
      ParseTree child = start.getChild(i);
      for (int t = 0; t < tabs; t++)
      {
        bld.append("  ");
      }

      bld.append(child.getClass().getSimpleName() + ":\n" + getExpressionMap(child, tabs + 1));
    }
    return bld.toString();
  }

}
