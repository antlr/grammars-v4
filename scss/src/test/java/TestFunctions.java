import org.testng.Assert;
import org.testng.annotations.Test;


public class TestFunctions extends TestBase
{
  @Test
  public void testFunction()
  {
    //TODO: make it work with parans in the math.
    //($n - 1) * $gutter-width;
    String [] lines = {
        "@function grid-width($n) {",
        "  @return $n * $grid-width;",
        "}"
    };
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();
    Assert.assertEquals(context.Identifier().getText(), "grid-width");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$n");

    ScssParser.FunctionReturnContext funcContext = context.functionBody().functionReturn();
    Assert.assertEquals(funcContext.commandStatement().expression(0).getText(), "$n");

    Assert.assertEquals(funcContext.commandStatement().mathCharacter().getText(), "*");
    Assert.assertEquals(funcContext.commandStatement().commandStatement().expression(0).getText(), "$grid-width");


  }

 @Test
 public void testFunctionMultiLine()
 {
   //TODO: make it work with parans in the math.
   //($n - 1) * $gutter-width;
   String [] lines = {
       "@function grid-width () {",
       "  $color: red;",
       "  @return $color;",
       "}"
   };
   ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();

   ScssParser.FunctionStatementContext funcContext = context.functionBody().functionStatement(0);
   Assert.assertEquals(funcContext.statement().variableDeclaration().variableName().getText(), "$color");
   Assert.assertEquals(funcContext.statement().variableDeclaration().value().getText(), "red");



   ScssParser.FunctionReturnContext returnContext = context.functionBody().functionReturn();
   Assert.assertEquals(returnContext.commandStatement().expression(0).getText(), "$color");
 }

  @Test
  public void testNestedFunctions()
  {
    String [] lines = {
        "@function grid-width ($a) {",
        "  @function grid-height($b) {",
        "    @return $color;",
        "  }",
        "  @return $world;",
        "}"
    };
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();
    Assert.assertEquals(context.Identifier().getText(), "grid-width");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$a");

    ScssParser.FunctionStatementContext funcContext = context.functionBody().functionStatement(0);
    Assert.assertEquals(funcContext.statement().functionDeclaration().Identifier().getText(), "grid-height");
    Assert.assertEquals(funcContext.statement().functionDeclaration().params().param(0).variableName().getText(), "$b");

    Assert.assertEquals(funcContext.statement().functionDeclaration().functionBody().functionReturn()
                            .commandStatement().expression(0).variableName().getText(), "$color");


    ScssParser.FunctionReturnContext returnContext = context.functionBody().functionReturn();
    Assert.assertEquals(returnContext.commandStatement().expression(0).getText(), "$world");


  }
}
