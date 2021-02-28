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
public class TestConditionals extends TestBase {
  @Test
  public void testIf() {
    String[] lines = {"@if 1 + 1 == 2 {}"};
    ScssParser.IfDeclarationContext context = parse(lines).statement(0).ifDeclaration();
    assertThat(context.conditions().condition().commandStatement().expression(0).getText())
        .isEqualTo("1");
    assertThat(
            context
                .conditions()
                .condition()
                .commandStatement()
                .mathStatement()
                .mathCharacter()
                .getText())
        .isEqualTo("+");
    assertThat(
            context
                .conditions()
                .condition()
                .commandStatement()
                .mathStatement()
                .commandStatement()
                .expression(0)
                .getText())
        .isEqualTo("1");

    assertThat(
            context
                .conditions()
                .condition()
                .conditions()
                .condition()
                .commandStatement()
                .expression(0)
                .getText())
        .isEqualTo("2");
  }

  @Test
  public void testElseIf() {
    String[] lines = {"@if 1 + 1 == 2 {}", "@else if 1 + 1 == 3 {}"};
    ScssParser.ElseIfStatementContext context =
        parse(lines).statement(0).ifDeclaration().elseIfStatement(0);
    assertThat(context.conditions().condition().commandStatement().expression(0).getText())
        .isEqualTo("1");
    assertThat(
            context
                .conditions()
                .condition()
                .commandStatement()
                .mathStatement()
                .mathCharacter()
                .getText())
        .isEqualTo("+");
    assertThat(
            context
                .conditions()
                .condition()
                .commandStatement()
                .mathStatement()
                .commandStatement()
                .expression(0)
                .getText())
        .isEqualTo("1");

    assertThat(
            context
                .conditions()
                .condition()
                .conditions()
                .condition()
                .commandStatement()
                .expression(0)
                .getText())
        .isEqualTo("3");
  }

  @Test
  public void testElseIfElse() {
    String[] lines = {"@if 1 + 1 == 2 {}", "@else if 1 + 1 == 3 {}", "@else { color: red }"};

    ScssParser.ElseStatementContext context =
        parse(lines).statement(0).ifDeclaration().elseStatement();
    assertThat(context.block().lastProperty().identifier().getText()).isEqualTo("color");
    assertThat(context.block().lastProperty().values().getText()).isEqualTo("red");
  }

  @Test
  public void testForLoop() {
    String[] lines = {"@for $i from 1 through 3 {}"};

    ScssParser.ForDeclarationContext context = parse(lines).statement(0).forDeclaration();
    assertThat(context.variableName().getText()).isEqualTo("$i");
    assertThat(context.fromNumber().getText()).isEqualTo("1");
    assertThat(context.throughNumber().getText()).isEqualTo("3");
  }

  @Test
  public void testWhileLoop() {
    String[] lines = {"@while $i > 0 {}"};

    ScssParser.WhileDeclarationContext context = parse(lines).statement(0).whileDeclaration();
    assertThat(
            context
                .conditions()
                .condition()
                .commandStatement()
                .expression(0)
                .variableName()
                .getText())
        .isEqualTo("$i");
    assertThat(
            context.conditions().condition().conditions().condition().commandStatement().getText())
        .isEqualTo("0");
  }

  @Test
  public void testBasicEach() {
    String[] lines = {"@each $animal in puma, sea-slug, egret, salamander {}"};

    ScssParser.EachDeclarationContext context = parse(lines).statement(0).eachDeclaration();
    assertThat(context.variableName(0).getText()).isEqualTo("$animal");
    assertThat(context.eachValueList().Identifier(0).getText()).isEqualTo("puma");
    assertThat(context.eachValueList().Identifier(1).getText()).isEqualTo("sea-slug");
    assertThat(context.eachValueList().Identifier(2).getText()).isEqualTo("egret");
    assertThat(context.eachValueList().Identifier(3).getText()).isEqualTo("salamander");
  }

  @Test
  public void testBasicEachMultiAssign() {
    String[] lines = {
      "@each $animal, $color, $cursor in (puma, black, default), (sea-slug, blue, pointer) {}"
    };
    ScssParser.EachDeclarationContext context = parse(lines).statement(0).eachDeclaration();
    assertThat(context.variableName(0).getText()).isEqualTo("$animal");
    assertThat(context.variableName(1).getText()).isEqualTo("$color");
    assertThat(context.variableName(2).getText()).isEqualTo("$cursor");

    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(0).getText())
        .isEqualTo("puma");
    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(1).getText())
        .isEqualTo("black");
    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(2).getText())
        .isEqualTo("default");

    assertThat(context.eachValueList().identifierListOrMap(1).identifierValue(0).getText())
        .isEqualTo("sea-slug");
    assertThat(context.eachValueList().identifierListOrMap(1).identifierValue(1).getText())
        .isEqualTo("blue");
    assertThat(context.eachValueList().identifierListOrMap(1).identifierValue(2).getText())
        .isEqualTo("pointer");
  }

  @Test
  public void testBasicEachMultiAssignWithvalues() {
    String[] lines = {"@each $header, $size in (h1: 2em, h2: 1.5em, h3: 1.2em) {}"};

    ScssParser.EachDeclarationContext context = parse(lines).statement(0).eachDeclaration();
    assertThat(context.variableName(0).getText()).isEqualTo("$header");
    assertThat(context.variableName(1).getText()).isEqualTo("$size");

    assertThat(
            context
                .eachValueList()
                .identifierListOrMap(0)
                .identifierValue(0)
                .identifier()
                .getText())
        .isEqualTo("h1");
    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(0).values().getText())
        .isEqualTo("2em");

    assertThat(
            context
                .eachValueList()
                .identifierListOrMap(0)
                .identifierValue(1)
                .identifier()
                .getText())
        .isEqualTo("h2");
    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(1).values().getText())
        .isEqualTo("1.5em");

    assertThat(
            context
                .eachValueList()
                .identifierListOrMap(0)
                .identifierValue(2)
                .identifier()
                .getText())
        .isEqualTo("h3");
    assertThat(context.eachValueList().identifierListOrMap(0).identifierValue(2).values().getText())
        .isEqualTo("1.2em");
  }
}
