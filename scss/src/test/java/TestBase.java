
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.TokenStream;
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

}
