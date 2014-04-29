
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
    Assert.assertEquals(getSelector(lines).selector(0).element(0).identifier().getText(), "body");
  }

  @Test
  public void testIds()
  {
    String [] lines = {
        "#id1 {}",
    };
    Assert.assertEquals(getSelector(lines).selector(0).element(0).identifier().getText(), "id1");
  }

  @Test
  public void testClass()
  {
    String [] lines = {
        ".cls {}",
    };
    Assert.assertEquals(getSelector(lines).selector(0).element(0).identifier().getText(), "cls");
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
    Assert.assertEquals(context.selector(0).element(0).identifier().getText(), "body");
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
    Assert.assertEquals(context.selector(0).element(0).identifier().getText(), "body");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls");

    Assert.assertEquals(context.selector(1).element(0).getText(), "h1");
    Assert.assertEquals(context.selector(1).element(1).getText(), ".cls1");

  }

  @Test
  public void testMultipleSelectorsMix()
  {
    String [] lines = {
        ".cls1 .cls2, .cls3, .cls4 > .cls5 {}",
    };
    ScssParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), ".cls1");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls2");

    Assert.assertEquals(context.selector(1).element(0).getText(), ".cls3");

    Assert.assertEquals(context.selector(2).element(0).getText(), ".cls4");

    Assert.assertEquals(context.selector(2).selectorPrefix(0).getText(), ">");
    Assert.assertEquals(context.selector(2).element(1).getText(), ".cls5");

  }

  @Test
  public void testNesting()
  {
    String [] lines = {
        "@media hello,world {",
        "  body, head {}",
        "}",
    };
    ScssParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).nested().nest().Identifier(0).getText(), "media");
    Assert.assertEquals(context.statement(0).nested().nest().Identifier(1).getText(), "hello");
    Assert.assertEquals(context.statement(0).nested().nest().Identifier(2).getText(), "world");

    ScssParser.StylesheetContext innerSheet = context.statement(0).nested().stylesheet();
    Assert.assertEquals(innerSheet.statement(0).ruleset().selectors().selector(0).getText(), "body");
    Assert.assertEquals(innerSheet.statement(0).ruleset().selectors().selector(1).getText(), "head");

  }

  @Test
  public void testProperties()
  {
    String [] lines = {
        "h1 {",
        "  display: block",
        "}",
    };
    ScssParser.BlockContext context = parse(lines).statement(0).ruleset().block();
    Assert.assertEquals(context.property(0).identifier().getText(), "display");
    Assert.assertEquals(context.property(0).value().commandStatement(0).getText(), "block");
  }

  @Test
  public void testPropertiesMultiValue()
  {
    String [] lines = {
        "h1 {",
        "  background: url('a'), 1px 2px",
        "}",
    };
    ScssParser.BlockContext context = parse(lines).statement(0).ruleset().block();
    Assert.assertEquals(context.property(0).identifier().getText(), "background");

    ScssParser.ValueContext val = context.property(0).value();
    Assert.assertEquals(val.commandStatement(0).expression(0).url().Url().getText(), "'a'");
    Assert.assertEquals(val.commandStatement(1).expression(0).measurement().Number().getText(), "1");
    Assert.assertEquals(val.commandStatement(1).expression(0).measurement().Unit().getText(), "px");

    Assert.assertEquals(val.commandStatement(1).expression(1).measurement().Number().getText(), "2");
    Assert.assertEquals(val.commandStatement(1).expression(1).measurement().Unit().getText(), "px");

  }

  @Test
  public void testPropertiesMultiLines()
  {
    String [] lines = {
        "h1 {",
        "  color: 1px;",
        "  font-size: #fff",
        "}",
    };
    ScssParser.BlockContext context = parse(lines).statement(0).ruleset().block();

    Assert.assertEquals(context.property(0).identifier().getText(), "color");
    ScssParser.ValueContext val = context.property(0).value();
    Assert.assertEquals(val.commandStatement(0).expression(0).measurement().Number().getText(), "1");
    Assert.assertEquals(val.commandStatement(0).expression(0).measurement().Unit().getText(), "px");

    Assert.assertEquals(context.property(1).identifier().getText(), "font-size");
    val = context.property(1).value();
    Assert.assertEquals(val.commandStatement(0).expression(0).Color().getText(), "#fff");

  }

  @Test
  public void testPropertyMeasurement()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: 1;");
    Assert.assertEquals(exp.measurement().Number().getText(), "1");
    Assert.assertNull(exp.measurement().Unit());
  }

  @Test
  public void testPropertyMeasurementAndUnit()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: 1px;");
    Assert.assertEquals(exp.measurement().Number().getText(), "1");
    Assert.assertEquals(exp.measurement().Unit().getText(), "px");
  }

  @Test
  public void testPropertyShortColor()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: #fff;");
    Assert.assertEquals(exp.Color().getText(), "#fff");
  }

  @Test
  public void testPropertyLongColor()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: #ababab;");
    Assert.assertEquals(exp.Color().getText(), "#ababab");
  }

  @Test
  public void testPropertyIdentifier()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: solid;");
    Assert.assertEquals(exp.identifier().getText(), "solid");
  }

  @Test
  public void testPropertyUrlString()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: url(\"hello\");");
    Assert.assertEquals(exp.url().Url().getText(), "\"hello\"");
  }

  @Test
  public void testPropertyUrl()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: url(hello);");
    Assert.assertEquals(exp.url().Url().getText(), "hello");
  }

  @Test
  public void testPropertyString()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: \"hello\";");
    Assert.assertEquals(exp.StringLiteral().getText(), "\"hello\"");
  }

  @Test
  public void testPropertyVariable()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: $hello;");
    Assert.assertEquals(exp.variableName().getText(), "$hello");
  }

  @Test
  public void testPropertyFunctionMath()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: calc(100% / 3);");
    Assert.assertEquals(exp.functionCall().Identifier().getText(), "calc");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).expression(0).getText(), "100%");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).mathCharacter().getText(), "/");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).commandStatement().expression(0)
                            .measurement().Number().getText(), "3");


  }

  @Test
  public void testPropertyFunctionMathMinus()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: calc(100% - 80px);");
    Assert.assertEquals(exp.functionCall().Identifier().getText(), "calc");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).expression(0).measurement().Number().getText(), "100");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).expression(0).measurement().Unit().getText(), "%");

    Assert.assertEquals(exp.functionCall().value().commandStatement(0).mathCharacter().getText(), "-");

    ScssParser.MeasurementContext measure = exp.functionCall().value().commandStatement(0).commandStatement().expression(0).measurement();
    Assert.assertEquals(measure.Number().getText(), "80");
    Assert.assertEquals(measure.Unit().getText(), "px");

  }


  @Test
  public void testPropertyFunctionMathVar()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: calc(100% - $var);");
    Assert.assertEquals(exp.functionCall().Identifier().getText(), "calc");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).expression(0).measurement().Number().getText(), "100");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).expression(0).measurement().Unit().getText(), "%");

    Assert.assertEquals(exp.functionCall().value().commandStatement(0).mathCharacter().getText(), "-");

    Assert.assertEquals(exp.functionCall().value().commandStatement(0).commandStatement().expression(0).variableName().getText(), "$var");

  }

  @Test
  public void testPropertyFunctionMathParen()
  {
    ScssParser.ExpressionContext exp = createProperty("p1: calc(((100%)));");
    Assert.assertEquals(exp.functionCall().Identifier().getText(), "calc");
    Assert.assertEquals(exp.functionCall().value().commandStatement(0).commandStatement().commandStatement()
                            .expression(0).measurement().Number().getText(), "100");

  }

  @Test
  public void testInterpolation()
  {
    String [] lines = {
        "p.#{$name} > select2 {}"
    };
    ScssParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(0).getText(), "p");
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(1)
                            .identifier().interpolation(0).variableName().getText(), "$name");

    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(2).getText(), "select2");


  }





  private ScssParser.SelectorsContext getSelector( String ... lines)
  {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).ruleset().selectors();
  }


  private ScssParser.ExpressionContext createProperty(String ... lines)
  {
    String [] all = new String[lines.length + 2];
    all[0] = "h1 {";
    System.arraycopy(lines, 0, all, 1, lines.length);
    all[all.length - 1] = "}";

    ScssParser.StylesheetContext styleContext = parse(all);
    ScssParser.BlockContext context = styleContext.statement(0).ruleset().block();
    return context.property(0).value().commandStatement(0).expression(0);
  }
}
