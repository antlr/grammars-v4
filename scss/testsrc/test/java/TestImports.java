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

import org.testng.Assert;
import org.testng.annotations.Test;


public class TestImports extends TestBase
{
  @Test
  public void testImport()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import \"hello\";");
    Assert.assertEquals(context.StringLiteral().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlString()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(\"hello\");");
    Assert.assertEquals(context.Url().getText(), "\"hello\"");
  }

  @Test
  public void testImportUrlNonStrings()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(hello);");
    Assert.assertEquals(context.Url().getText(), "hello");
  }

  @Test
  public void testImportUrlNonStringsSpace()
  {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(hello world);");
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
