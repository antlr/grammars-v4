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
    Assert.assertEquals(context.params().param(0).ParamName().getText(), "$n");

    ScssParser.FunctionReturnContext funcContext = context.functionBody().functionReturn();
    Assert.assertEquals(funcContext.commandStatement().expression().getText(), "$n");

    Assert.assertEquals(funcContext.commandStatement().mathCharacter().getText(), "*");
    Assert.assertEquals(funcContext.commandStatement().commandStatement().expression().getText(), "$grid-width");


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
   Assert.assertEquals(returnContext.commandStatement().expression().getText(), "$color");


 }
}
