import org.testng.Assert;
import org.testng.annotations.Test;


public class TestConditionals extends TestBase
{
  @Test
  public void testIf()
  {
    String [] lines = {
        "@if 1 + 1 == 2 {}"
    };
    ScssParser.IfDeclarationContext context = parse(lines).statement(0).ifDeclaration();
    Assert.assertEquals(context.conditions().condition().commandStatement().expression().getText(), "1");
    Assert.assertEquals(context.conditions().condition().commandStatement().mathCharacter().getText(), "+");
    Assert.assertEquals(context.conditions().condition().commandStatement().commandStatement().expression().getText(), "1");

    Assert.assertEquals(context.conditions().condition().conditions().condition().commandStatement().expression().getText()
        , "2");


  }

  @Test
  public void testElseIf()
  {
    String [] lines = {
        "@if 1 + 1 == 2 {}",
        "@else if 1 + 1 == 3 {}"
    };
    ScssParser.ElseIfStatementContext context = parse(lines).statement(0).ifDeclaration().elseIfStatement(0);
    Assert.assertEquals(context.conditions().condition().commandStatement().expression().getText(), "1");
    Assert.assertEquals(context.conditions().condition().commandStatement().mathCharacter().getText(), "+");
    Assert.assertEquals(context.conditions().condition().commandStatement().commandStatement().expression().getText(), "1");

    Assert.assertEquals(context.conditions().condition().conditions().condition().commandStatement().expression().getText()
        , "3");
  }


  @Test
  public void testElseIfElse()
  {
    String [] lines = {
        "@if 1 + 1 == 2 {}",
        "@else if 1 + 1 == 3 {}",
        "@else { color: red }"

    };
    ScssParser.ElseStatementContext context = parse(lines).statement(0).ifDeclaration().elseStatement();
    Assert.assertEquals(context.block().property(0).identifier().getText(), "color");
    Assert.assertEquals(context.block().property(0).value().getText(), "red");

  }



  @Test
  public void testForLoop()
  {
    String [] lines = {
        "@for $i from 1 through 3 {}"

    };
    ScssParser.ForDeclarationContext context = parse(lines).statement(0).forDeclaration();
    Assert.assertEquals(context.variableName().getText(), "$i");
    Assert.assertEquals(context.fromNumber().getText(), "1");
    Assert.assertEquals(context.throughNumber().getText(), "3");



  }

  @Test
  public void testWhileLoop()
  {
    String [] lines = {
        "@while $i > 0 {}"

    };
    ScssParser.WhileDeclarationContext context = parse(lines).statement(0).whileDeclaration();
    Assert.assertEquals(context.conditions().condition().commandStatement().expression().variableName().getText(), "$i");
    Assert.assertEquals(context.conditions().condition().conditions().condition().commandStatement().getText(), "0");



  }
}
