import org.testng.Assert;
import org.testng.annotations.Test;


public class TestImports extends TestBase
{
  @Test
  public void testImport()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import \"hello\"");
    Assert.assertEquals(context.StringLiteral().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlString()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(\"hello\")");
    Assert.assertEquals(context.Url().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlNonStrings()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(hello)");
    Assert.assertEquals(context.Url().getText(), "hello");
  }

  @Test
  public void testImportUrlNonStringsSpace()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(hello world)");
    Assert.assertEquals(context.Url().getText(), "hello world");
  }

  @Test
  public void testMultipleImports()
  {
    String [] lines = {
        "@import url(hello world)",
        "@import url(\"foobar\")",
        "@import \"hi\""

    };
    ScssParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).importDeclaration()
                            .referenceUrl().Url().getText(), "hello world");
    Assert.assertEquals(context.statement(1).importDeclaration()
                            .referenceUrl().Url().getText(), "\"foobar\"");
    Assert.assertEquals(context.statement(2).importDeclaration()
                            .referenceUrl().StringLiteral().getText(), "\"hi\"");
  }


  private ScssParser.ReferenceUrlContext parseImport(String ... lines)
  {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).importDeclaration().referenceUrl();
  }
}
