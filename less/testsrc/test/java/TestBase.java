/*
[The "MIT licence"]
Copyright (c) 2014 Kyle Lee
All rights reserved.
*/

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.misc.Nullable;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.testng.Assert;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;

public class TestBase
{
  protected void assertEqualsNoOrder(List actual, List expected)
  {
    Assert.assertEqualsNoOrder(actual.toArray(), expected.toArray(), "Not Equal Expected: " +  Arrays.toString(expected.toArray()) + " Actual: " + Arrays.toString(
        actual.toArray()));
  }

  protected LessParser.StylesheetContext parse(String ... lines)
  {
    try
    {
      StringBuilder fileLines = new StringBuilder();
      for (String line : lines)
      {
        fileLines.append(line);
      }

      ANTLRInputStream source = new ANTLRInputStream(new ByteArrayInputStream(fileLines.toString().getBytes()));

      LessLexer lexer = new LessLexer(source);
      TokenStream stream = new BufferedTokenStream(lexer);
      LessParser par = new LessParser(stream);
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

    private String getExpressionMap(ParseTree start, int tabs) {
        if (start == null) {
            return "";
        }

        StringBuilder bld = new StringBuilder();
        if (TerminalNode.class.isAssignableFrom(start.getClass())) {
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

            bld.append(child.getClass().getSimpleName() + ":\n" + getExpressionMap(child, tabs + 1));
        }
        return bld.toString();
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
        Assert.fail("Syntax error in detection." + o);
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

}
