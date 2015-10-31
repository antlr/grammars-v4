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


import org.antlr.v4.runtime.ParserRuleContext;
import org.testng.Assert;
import org.testng.annotations.Test;


public class TestMixins extends TestBase
{
  @Test
   public void testMixin()
  {
    String [] lines = {
        "@mixin something-awesome {}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "something-awesome");
  }

  @Test
  public void testMixinProperties()
  {
    String [] lines = {
        "@mixin test {",
        "  display: $hello;",
        "}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");
    Assert.assertEquals(context.block().property(0).identifier().getText(), "display");
    Assert.assertEquals(context.block().property(0).values().commandStatement(0).expression(0)
                            .variableName().Identifier().getText(), "hello");
  }

  @Test
  public void testMixinNesting()
  {
    String [] lines = {
        "@mixin test {",
        "  @mixin hello {}",
        "}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");

    ScssParser.MixinDeclarationContext innerContext = context.block().statement(0).mixinDeclaration();
    Assert.assertEquals(innerContext.Identifier().getText(), "hello");
  }

  @Test
  public void testMixinParams()
  {
    String [] lines = {
        "@mixin test ($val1, $val2) {}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$val1");
    Assert.assertEquals(context.params().param(1).variableName().getText(), "$val2");

  }

  @Test
  public void testMixinParamWithValues()
  {
    String [] lines = {
        "@mixin test ($val1 : 3px, $val2) {}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$val1");
    Assert.assertEquals(context.params().param(0).paramOptionalValue().expression(0)
                            .measurement().Number().getText(), "3");
    Assert.assertEquals(context.params().param(0).paramOptionalValue().expression(0)
                            .measurement().Unit().getText(), "px");


    Assert.assertEquals(context.params().param(1).variableName().getText(), "$val2");

  }

  @Test
  public void testMixinParamWithElipses()
  {
    String [] lines = {
        "@mixin test ($val1, $val2...) {}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$val1");
    Assert.assertEquals(context.params().param(1).variableName().getText(), "$val2");
    Assert.assertEquals(context.params().Ellipsis().getText(), "...");
  }

  @Test
  public void testMixinParamAndBody()
  {
    String [] lines = {
        "@mixin box-shadow ($shadows...) {",
        "  -moz-box-shadow: $shadows;",
        "  -webkit-box-shadow: $shadows;",
        "  box-shadow: $shadows;",
        "}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "box-shadow");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$shadows");
    Assert.assertNotNull(context.params().Ellipsis());

    Assert.assertEquals(context.block().property(0).identifier().getText(), "-moz-box-shadow");
    Assert.assertEquals(context.block().property(0).values().commandStatement(0)
                            .expression(0).variableName().Identifier().getText(), "shadows");

    Assert.assertEquals(context.block().property(1).identifier().getText(), "-webkit-box-shadow");
    Assert.assertEquals(context.block().property(1).values().commandStatement(0)
                            .expression(0).variableName().Identifier().getText(), "shadows");

    Assert.assertEquals(context.block().property(2).identifier().getText(), "box-shadow");
    Assert.assertEquals(context.block().property(2).values().commandStatement(0)
                            .expression(0).variableName().Identifier().getText(), "shadows");

  }

  @Test
  public void testMixinPropertyAndRule()
  {
    String [] lines = {
        "@mixin clearfix {",
        "  display: inline-block;",
        "  &:after {",
        "    content: \".\";",
        "    display: block;",
        "    height: 0;",
        "    clear: both;",
        "    visibility: hidden;",
        "  }",
        "  * html & { height: 1px }",
        "}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "clearfix");

    Assert.assertEquals(context.block().property(0).identifier().getText(), "display");
    Assert.assertEquals(context.block().property(0).values().commandStatement(0)
                            .expression(0).identifier().getText(), "inline-block");

    Assert.assertEquals(context.block().statement(0).ruleset().selectors()
                            .selector(0).element(0).getText(), "&");
    Assert.assertEquals(context.block().statement(0).ruleset().selectors()
                            .selector(0).pseudo().Identifier().getText(), "after");


    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(0).getText(), "*");
    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(1).identifier().getText(), "html");
    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(2).getText(), "&");

    Assert.assertEquals(context.block().statement(1).ruleset().block().property(0).identifier().getText(), "height");
    Assert.assertEquals(context.block().statement(1).ruleset().block().property(0).values()
                            .commandStatement(0).expression(0).measurement().getText(), "1px");


  }

  @Test
  public void testMixinBlockInterpolation()
  {
    String [] lines = {
        "@mixin test () {",
        "  #{$attr}-color: blue;"
        ,"}"
    };
    ScssParser.MixinDeclarationContext context = parseImport(lines);
    Assert.assertEquals(context.Identifier().getText(), "test");
    Assert.assertEquals(context.block().property(0).identifier().identifierVariableName().getText(), "$attr");
    Assert.assertEquals(context.block().property(0).identifier().identifierPart(0).IdentifierAfter().getText(), "-color");
  }

  @Test
  public void testNoErrors()
  {
    String lines = "@mixin retina-image($filename, $background-size, $extension: png, $retina-filename: null, $retina-suffix: _2x, $asset-pipeline: false) {\n" +
        "  @if $asset-pipeline {\n" +
        "    background-image: image-url(\"#{$filename}.#{$extension}\");\n" +
        "  }\n" +
        "  @else {\n" +
        "    background-image:       url(\"#{$filename}.#{$extension}\");\n" +
        "  }\n" +
        "\n" +
        "  @include hidpi {\n" +
        "    @if $asset-pipeline {\n" +
        "      @if $retina-filename {\n" +
        "        background-image: image-url(\"#{$retina-filename}.#{$extension}\");\n" +
        "      }\n" +
        "      @else {\n" +
        "        background-image: image-url(\"#{$filename}#{$retina-suffix}.#{$extension}\");\n" +
        "      }\n" +
        "    }\n" +
        "\n" +
        "    @else {\n" +
        "      @if $retina-filename {\n" +
        "        background-image: url(\"#{$retina-filename}.#{$extension}\");\n" +
        "      }\n" +
        "      @else {\n" +
        "        background-image: url(\"#{$filename}#{$retina-suffix}.#{$extension}\");\n" +
        "      }\n" +
        "    }\n" +
        "\n" +
        "    background-size: $background-size;\n" +
        "\n" +
        "  }\n" +
        "}";

    ScssParser.StylesheetContext context = parse(lines);
    for (ParserRuleContext child : context.getRuleContexts(ParserRuleContext.class))
    {

    }

  }

  private void assertNoErrors(ParserRuleContext context)
  {

  }


  private ScssParser.MixinDeclarationContext parseImport(String ... lines)
  {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).mixinDeclaration();
  }
}
