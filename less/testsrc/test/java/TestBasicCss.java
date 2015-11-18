/*
 [The "MIT licence"]
 Copyright (c) 2014 Kyle Lee
 All rights reserved.
*/

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
    LessParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), "body");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls");
  }

  @Test
  public void testMultipleSelectorsDifferentSelectors()
  {
    String [] lines = {
        "body .cls, h1 {}",
    };
    LessParser.SelectorsContext context = getSelector(lines);
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
    LessParser.SelectorsContext context = getSelector(lines);
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
    LessParser.SelectorsContext context = getSelector(lines);
    Assert.assertEquals(context.selector(0).element(0).getText(), ".cls1");
    Assert.assertEquals(context.selector(0).element(1).getText(), ".cls2");

    Assert.assertEquals(context.selector(1).element(0).getText(), ".cls3");

    Assert.assertEquals(context.selector(2).element(0).getText(), ".cls4");

    Assert.assertEquals(context.selector(2).selectorPrefix(0).getText(), ">");
    Assert.assertEquals(context.selector(2).element(1).getText(), ".cls5");

  }

  @Test
  public void testProperties()
  {
    String [] lines = {
        "h1 {",
        "  display: block",
        "}",
    };
    LessParser.BlockContext context = parse(lines).statement(0).ruleset().block();
    Assert.assertEquals(context.property(0).identifier().getText(), "display");
    Assert.assertEquals(context.property(0).values().commandStatement(0).getText(), "block");
  }

  @Test
  public void testPropertiesMultivalues()
  {
    String [] lines = {
        "h1 {",
        "  background: url('a'), 1px 2px",
        "}",
    };
    LessParser.BlockContext context = parse(lines).statement(0).ruleset().block();
    Assert.assertEquals(context.property(0).identifier().getText(), "background");

    LessParser.ValuesContext val = context.property(0).values();
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
    LessParser.BlockContext context = parse(lines).statement(0).ruleset().block();

    Assert.assertEquals(context.property(0).identifier().getText(), "color");
    LessParser.ValuesContext val = context.property(0).values();
    Assert.assertEquals(val.commandStatement(0).expression(0).measurement().Number().getText(), "1");
    Assert.assertEquals(val.commandStatement(0).expression(0).measurement().Unit().getText(), "px");

    Assert.assertEquals(context.property(1).identifier().getText(), "font-size");
    val = context.property(1).values();
    Assert.assertEquals(val.commandStatement(0).expression(0).Color().getText(), "#fff");

  }

  @Test
  public void testPropertyMeasurement()
  {
    LessParser.ExpressionContext exp = createProperty("p1: 1;");
    Assert.assertEquals(exp.measurement().Number().getText(), "1");
    Assert.assertNull(exp.measurement().Unit());
  }

  @Test
  public void testPropertyMeasurementAndUnit()
  {
    LessParser.ExpressionContext exp = createProperty("p1: 1px;");
    Assert.assertEquals(exp.measurement().Number().getText(), "1");
    Assert.assertEquals(exp.measurement().Unit().getText(), "px");
  }

  @Test
  public void testPropertyShortColor()
  {
    LessParser.ExpressionContext exp = createProperty("p1: #fff;");
    Assert.assertEquals(exp.Color().getText(), "#fff");
  }

  @Test
  public void testPropertyLongColor()
  {
    LessParser.ExpressionContext exp = createProperty("p1: #ababab;");
    Assert.assertEquals(exp.Color().getText(), "#ababab");
  }

  @Test
  public void testPropertyIdentifier()
  {
    LessParser.ExpressionContext exp = createProperty("p1: solid;");
    Assert.assertEquals(exp.identifier().getText(), "solid");
  }

  @Test
  public void testPropertyUrlString()
  {
    LessParser.ExpressionContext exp = createProperty("p1: url(\"hello\");");
    Assert.assertEquals(exp.url().Url().getText(), "\"hello\"");
  }

  @Test
  public void testPropertyUrl()
  {
    LessParser.ExpressionContext exp = createProperty("p1: url(hello);");
    Assert.assertEquals(exp.url().Url().getText(), "hello");
  }

  @Test
  public void testPropertyString()
  {
    LessParser.ExpressionContext exp = createProperty("p1: \"hello\";");
    Assert.assertEquals(exp.StringLiteral().getText(), "\"hello\"");
  }

  @Test
  public void testPropertyVariable()
  {
    LessParser.ExpressionContext exp = createProperty("p1: @hello;");
    Assert.assertEquals(exp.variableName().getText(), "@hello");
  }

  @Test
  public void testVariableDeclaration()
  {
    String [] lines = {
            "@name:body;"
    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).variableDeclaration().variableName().Identifier().getText(), "name");
    Assert.assertEquals(context.statement(0).variableDeclaration().values().commandStatement(0).getText(), "body");
  }

  @Test
  public void testVariableReference()
  {
    String [] lines = {
            "a{",
            "  color:@linkColor;",
            "}",
    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).ruleset().block().property(0).values().commandStatement(0).expression(0).variableName().Identifier().getText(), "linkColor");
  }

  @Test
  public void testVariableDualReference()
  {
    String [] lines = {
            "a{color:@@linkColor;}"
    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).ruleset().block().property(0).values().commandStatement(0).expression(0).variableName().variableName().Identifier().getText(), "linkColor");
  }

  @Test
  public void testVariableInterpolation()
  {
    String [] lines = {
        "p.@{name} > select2 {}"
    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(0).identifier().getText(), "p");
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(1).identifier().identifierVariableName()
            .getText(), "name");

    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(2).getText(), "select2");
  }

  @Test
  public void testInterpolationSpace()
  {
    String [] lines = {
        "p #@{name} {}"
    };
    LessParser.StylesheetContext context = parse(lines);
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(0).identifier().getText(), "p");
    Assert.assertEquals(context.statement(0).ruleset().selectors().selector(0).element(1).identifier().identifierVariableName()
                            .getText(), "name");
  }

  private LessParser.SelectorsContext getSelector( String ... lines)
  {
    LessParser.StylesheetContext context = parse(lines);
    return context.statement(0).ruleset().selectors();
  }

  private LessParser.ExpressionContext createProperty(String ... lines)
  {
    String [] all = new String[lines.length + 2];
    all[0] = "h1 {";
    System.arraycopy(lines, 0, all, 1, lines.length);
    all[all.length - 1] = "}";

    LessParser.StylesheetContext styleContext = parse(all);
    LessParser.BlockContext context = styleContext.statement(0).ruleset().block();
    return context.property(0).values().commandStatement(0).expression(0);
  }
}
