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

import static com.google.common.truth.Truth.assertThat;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

@RunWith(JUnit4.class)
public class TestFunctions extends TestBase {
  @Test
  public void testFunction() {
    // TODO: make it work with parans in the math.
    // ($n - 1) * $gutter-width;
    String[] lines = {"@function grid-width($n) {", "  @return ($n - 1) * $gutter-width;", "}"};
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();
    assertThat(context.Identifier().getText()).isEqualTo("grid-width");
    assertThat(context.declaredParams().declaredParam(0).variableName().getText()).isEqualTo("$n");

    ScssParser.FunctionReturnContext funcContext = context.functionBody().functionReturn();
    assertThat(funcContext.commandStatement().commandStatement().expression(0).getText())
        .isEqualTo("$n");
    assertThat(
            funcContext
                .commandStatement()
                .commandStatement()
                .mathStatement()
                .commandStatement()
                .getText())
        .isEqualTo("1");

    assertThat(funcContext.commandStatement().mathStatement().mathCharacter().getText())
        .isEqualTo("*");
    assertThat(funcContext.commandStatement().mathStatement().commandStatement().getText())
        .isEqualTo("$gutter-width");
  }

  @Test
  public void testFunctionMultiLine() {
    // TODO: make it work with parans in the math.
    // ($n - 1) * $gutter-width;
    String[] lines = {"@function grid-width () {", "  $color: red;", "  @return $color;", "}"};
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();

    ScssParser.FunctionStatementContext funcContext = context.functionBody().functionStatement(0);
    assertThat(funcContext.statement().variableDeclaration().variableName().getText())
        .isEqualTo("$color");
    assertThat(funcContext.statement().variableDeclaration().values().getText()).isEqualTo("red");

    ScssParser.FunctionReturnContext returnContext = context.functionBody().functionReturn();
    assertThat(returnContext.commandStatement().expression(0).getText()).isEqualTo("$color");
  }

  @Test
  public void testNestedFunctions() {
    String[] lines = {
      "@function grid-width ($a) {",
      "  @function grid-height($b) {",
      "    @return $color;",
      "  }",
      "  @return $world;",
      "}"
    };
    ScssParser.FunctionDeclarationContext context = parse(lines).statement(0).functionDeclaration();
    assertThat(context.Identifier().getText()).isEqualTo("grid-width");
    assertThat(context.declaredParams().declaredParam(0).variableName().getText()).isEqualTo("$a");

    ScssParser.FunctionStatementContext funcContext = context.functionBody().functionStatement(0);
    assertThat(funcContext.statement().functionDeclaration().Identifier().getText())
        .isEqualTo("grid-height");
    assertThat(
            funcContext
                .statement()
                .functionDeclaration()
                .declaredParams()
                .declaredParam(0)
                .variableName()
                .getText())
        .isEqualTo("$b");

    assertThat(
            funcContext
                .statement()
                .functionDeclaration()
                .functionBody()
                .functionReturn()
                .commandStatement()
                .expression(0)
                .variableName()
                .getText())
        .isEqualTo("$color");

    ScssParser.FunctionReturnContext returnContext = context.functionBody().functionReturn();
    assertThat(returnContext.commandStatement().expression(0).getText()).isEqualTo("$world");
  }
}
