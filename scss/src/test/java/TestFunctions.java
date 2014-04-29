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


public class TestFunctions extends TestBase
{
  @Test
  public void testFunction()
  {
    //TODO: make it work with parans in the math.
    //($n - 1) * $gutter-width;
    String [] lines = {
        "@function grid-width($n) {",
        "  @return ($n - 1) * $gutter-width;",
        "}"
    };
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();
    Assert.assertEquals(context.Identifier().getText(), "grid-width");
    Assert.assertEquals(context.params().param(0).variableName().getText(), "$n");

    ScssParser.FunctionReturnContext funcContext = context.functionBody().functionReturn();
    Assert.assertEquals(funcContext.commandStatement().commandStatement().expression(0).getText(), "$n");
    Assert.assertEquals(funcContext.commandStatement().commandStatement().mathStatement().commandStatement().getText(), "1");


    Assert.assertEquals(funcContext.commandStatement().mathStatement().mathCharacter().getText(), "*");
    Assert.assertEquals(funcContext.commandStatement().mathStatement().commandStatement().getText(), "$gutter-width");


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
   Assert.assertEquals(funcContext.statement().variableDeclaration().values().getText(), "red");



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
