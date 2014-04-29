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
    Assert.assertEquals(context.block().property(0).value().commandStatement(0).expression(0)
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
    Assert.assertEquals(context.block().property(0).value().commandStatement(0)
                            .expression(0).variableName().Identifier().getText(), "shadows");

    Assert.assertEquals(context.block().property(1).identifier().getText(), "-webkit-box-shadow");
    Assert.assertEquals(context.block().property(1).value().commandStatement(0)
                            .expression(0).variableName().Identifier().getText(), "shadows");

    Assert.assertEquals(context.block().property(2).identifier().getText(), "box-shadow");
    Assert.assertEquals(context.block().property(2).value().commandStatement(0)
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
    Assert.assertEquals(context.block().property(0).value().commandStatement(0)
                            .expression(0).identifier().getText(), "inline-block");

    Assert.assertEquals(context.block().statement(0).ruleset().selectors()
                            .selector(0).element(0).getText(), "&");
    Assert.assertEquals(context.block().statement(0).ruleset().selectors()
                            .selector(0).pseudo().Identifier().getText(), "after");


    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(0).getText(), "*");
    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(1).getText(), "html");
    Assert.assertEquals(context.block().statement(1).ruleset().selectors()
                            .selector(0).element(2).getText(), "&");

    Assert.assertEquals(context.block().statement(1).ruleset().block().property(0).identifier().getText(), "height");
    Assert.assertEquals(context.block().statement(1).ruleset().block().property(0).value()
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
    Assert.assertEquals(context.block().property(0).identifier().interpolation(0).variableName().getText(), "$attr");
    Assert.assertEquals(context.block().property(0).identifier().Identifier(0).getText(), "-color");
  }

  private ScssParser.MixinDeclarationContext parseImport(String ... lines)
  {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).mixinDeclaration();
  }
}
