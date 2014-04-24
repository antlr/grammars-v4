
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.TokenStream;
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
      return par.stylesheet();
    }
    catch (IOException e)
    {
      Assert.fail("Exception should not happen here", e);
    }
    return null;
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
