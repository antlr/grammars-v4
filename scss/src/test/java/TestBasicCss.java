import org.testng.Assert;
import org.testng.annotations.Test;


public class TestBasicCss extends TestBase
{

  @Test
  public void testSimple()
  {
    String [] lines = {
        "body {}",
    };
    Assert.assertEquals(getSelector(lines).selector(0).element(0).Identifier().getText(), "body");
  }

  @Test
  public void testOneSelectorMultipleParts()
  {
    String [] lines = {
        "body .cls {}",
    };
    ScssParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), "body");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls");
  }

  @Test
  public void testMultipleSelectorsDifferentSelectors()
  {
    String [] lines = {
        "body .cls, h1 {}",
    };
    ScssParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), "body");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls");

    Assert.assertEquals(context.selector(1).element(0).getText(), "h1");
  }

  @Test
  public void testMultipleSelectorsSameElement()
  {
    String [] lines = {
        "body .cls, h1.cls1 {}",
    };
    ScssParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), "body");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls");

    Assert.assertEquals(context.selector(1).element(0).getText(), "h1");
    Assert.assertEquals(context.selector(1).element(1).getText(), ".cls1");

  }

  private ScssParser.SelectorsContext getSelector( String ... lines)
  {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).ruleset().selectors();
  }
}
