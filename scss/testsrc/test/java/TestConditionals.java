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
    assertThat(context.conditions().condition().commandStatement().expression().getText())
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
                .expression()
                .getText())
        .isEqualTo("1");

    assertThat(
            context
                .conditions()
                .condition()
                .conditions()
                .condition()
                .commandStatement()
                .expression()
                .getText())
        .isEqualTo("2");
  }

  @Test
  public void testElseIf() {
    String[] lines = {"@if 1 + 1 == 2 {}", "@else if 1 + 1 == 3 {}"};
    ScssParser.ElseIfStatementContext context =
        parse(lines).statement(0).ifDeclaration().elseIfStatement(0);
    assertThat(context.conditions().condition().commandStatement().expression().getText())
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
                .expression()
                .getText())
        .isEqualTo("1");

    assertThat(
            context
                .conditions()
                .condition()
                .conditions()
                .condition()
                .commandStatement()
                .expression()
                .getText())
        .isEqualTo("3");
  }

  @Test
  public void testElseIfElse() {
    String[] lines = {"@if 1 + 1 == 2 {}", "@else if 1 + 1 == 3 {}", "@else { color: red }"};

    ScssParser.ElseStatementContext context =
        parse(lines).statement(0).ifDeclaration().elseStatement();
    assertThat(context.block().lastProperty().identifier().getText()).isEqualTo("color");
    assertThat(context.block().lastProperty().propertyValue().getText()).isEqualTo("red");
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
                .expression()
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
    ScssParser.ListCommaSeparatedContext list = context.eachValueList().list().listCommaSeparated();
    assertThat(list.listElement(0).commandStatement().getText()).isEqualTo("puma");
    assertThat(list.listElement(1).commandStatement().getText()).isEqualTo("sea-slug");
    assertThat(list.listElement(2).commandStatement().getText()).isEqualTo("egret");
    assertThat(list.listElement(3).commandStatement().getText()).isEqualTo("salamander");
  }

  @Test
  public void eachDestructuringWithListOfLists() {
    String[] lines = {
      "@each $animal, $color, $cursor in (puma, black, default), (sea-slug, blue, pointer) {}"
    };
    ScssParser.EachDeclarationContext context = parse(lines).statement(0).eachDeclaration();
    assertThat(context.variableName(0).getText()).isEqualTo("$animal");
    assertThat(context.variableName(1).getText()).isEqualTo("$color");
    assertThat(context.variableName(2).getText()).isEqualTo("$cursor");

    ScssParser.ListCommaSeparatedContext list1 =
        context
            .eachValueList()
            .list()
            .listCommaSeparated()
            .listElement(0)
            .list()
            .listCommaSeparated();
    assertThat(list1.listElement(0).commandStatement().getText()).isEqualTo("puma");
    assertThat(list1.listElement(1).commandStatement().getText()).isEqualTo("black");
    assertThat(list1.listElement(2).commandStatement().getText()).isEqualTo("default");

    ScssParser.ListCommaSeparatedContext list2 =
        context
            .eachValueList()
            .list()
            .listCommaSeparated()
            .listElement(1)
            .list()
            .listCommaSeparated();
    assertThat(list2.listElement(0).commandStatement().getText()).isEqualTo("sea-slug");
    assertThat(list2.listElement(1).commandStatement().getText()).isEqualTo("blue");
    assertThat(list2.listElement(2).commandStatement().getText()).isEqualTo("pointer");
  }

  @Test
  public void eachDestructuringWithMap() {
    String[] lines = {"@each $header, $size in (h1: 2em, h2: 1.5em, h3: 1.2em) {}"};

    ScssParser.EachDeclarationContext context = parse(lines).statement(0).eachDeclaration();
    assertThat(context.variableName(0).getText()).isEqualTo("$header");
    assertThat(context.variableName(1).getText()).isEqualTo("$size");

    assertThat(
            context
                .eachValueList()
                .map()
                .mapEntry(0)
                .mapKey()
                .commandStatement()
                .expression()
                .identifier()
                .getText())
        .isEqualTo("h1");
    assertThat(
            context
                .eachValueList()
                .map()
                .mapEntry(0)
                .mapValue()
                .commandStatement()
                .expression()
                .measurement()
                .getText())
        .isEqualTo("2em");

    assertThat(context.eachValueList().map().mapEntry(1).mapKey().getText()).isEqualTo("h2");
    assertThat(context.eachValueList().map().mapEntry(1).mapValue().getText()).isEqualTo("1.5em");

    assertThat(context.eachValueList().map().mapEntry(2).mapKey().getText()).isEqualTo("h3");
    assertThat(context.eachValueList().map().mapEntry(2).mapValue().getText()).isEqualTo("1.2em");
  }
}
