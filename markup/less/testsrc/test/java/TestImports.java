/*
 [The "MIT licence"]
 Copyright (c) 2014 Kyle Lee
 All rights reserved.
*/

import org.testng.Assert;
import org.testng.annotations.Test;


public class TestImports extends TestBase
{
  @Test
  public void testImport()
  {
    LessParser.ReferenceUrlContext context = parseImport("@import \"hello\";");
    Assert.assertEquals(context.StringLiteral().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlString()
  {
    LessParser.ReferenceUrlContext context = parseImport("@import url(\"hello\");");
    Assert.assertEquals(context.Url().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlNonStrings()
  {
    LessParser.ReferenceUrlContext context = parseImport("@import url(hello);");
    Assert.assertEquals(context.Url().getText(), "hello");
  }

  @Test
  public void testImportUrlNonStringsSpace()
  {
    LessParser.ReferenceUrlContext context = parseImport("@import url(hello world);");
    Assert.assertEquals(context.Url().getText(), "hello world");
  }

  @Test
  public void testMultipleImports()
  {
    String [] lines = {
        "@import url(hello world);",
        "@import url(\"foobar\");",
        "@import \"hi\";"

    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).importDeclaration()
                            .referenceUrl().Url().getText(), "hello world");
    Assert.assertEquals(context.statement(1).importDeclaration()
                            .referenceUrl().Url().getText(), "\"foobar\"");
    Assert.assertEquals(context.statement(2).importDeclaration()
                            .referenceUrl().StringLiteral().getText(), "\"hi\"");
  }


  private LessParser.ReferenceUrlContext parseImport(String ... lines)
  {
    LessParser.StylesheetContext context = parse(lines);
    return context.statement(0).importDeclaration().referenceUrl();
  }
}
