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
public class TestInclude extends TestBase {
  @Test
  public void testInclude() {
    String[] lines = {"@include large-text;"};
    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).includeDeclaration().Identifier().getText())
        .isEqualTo("large-text");
  }

  @Test
  public void testIncludeInBlock() {
    String[] lines = {".test {", "  @include large-text;", "}"};
    ScssParser.StylesheetContext context = parse(lines);
    assertThat(
            context
                .statement(0)
                .ruleset()
                .block()
                .statement(0)
                .includeDeclaration()
                .Identifier()
                .getText())
        .isEqualTo("large-text");
  }

  @Test
  public void testMultipleIncludes() {
    String[] lines = {
      "@include large-text;", "@include large-button;",
    };
    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).includeDeclaration().Identifier().getText())
        .isEqualTo("large-text");
    assertThat(context.statement(1).includeDeclaration().Identifier().getText())
        .isEqualTo("large-button");
  }

  @Test
  public void testIncludesInMixin() {
    String[] lines = {"@mixin large-text {", "   @include large-button;", "}"};

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).mixinDeclaration().Identifier().getText())
        .isEqualTo("large-text");
    assertThat(
            context
                .statement(0)
                .mixinDeclaration()
                .block()
                .statement(0)
                .includeDeclaration()
                .Identifier()
                .getText())
        .isEqualTo("large-button");
  }

  @Test
  public void testIncludesWithParams() {
    String[] lines = {"@include large-button(blue, $var);"};

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).includeDeclaration().Identifier().getText())
        .isEqualTo("large-button");
    assertThat(
            context
                .statement(0)
                .includeDeclaration()
                .values()
                .commandStatement(0)
                .expression(0)
                .identifier()
                .getText())
        .isEqualTo("blue");
    assertThat(
            context
                .statement(0)
                .includeDeclaration()
                .values()
                .commandStatement(1)
                .expression(0)
                .variableName()
                .getText())
        .isEqualTo("$var");
  }

  @Test
  public void testIncludesWithBody() {
    String[] lines = {"@include large-button {", "  color: black", "}"};

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).includeDeclaration().Identifier().getText())
        .isEqualTo("large-button");
    assertThat(context.statement(0).includeDeclaration().block().property(0).identifier().getText())
        .isEqualTo("color");
    assertThat(
            context
                .statement(0)
                .includeDeclaration()
                .block()
                .property(0)
                .values()
                .commandStatement(0)
                .expression(0)
                .identifier()
                .getText())
        .isEqualTo("black");
  }

  @Test
  public void testIncludesWithInterpolation() {
    String[] lines = {"@include large-button (#{$var1}) {", "  color-#{$var2}: black", "}"};

    ScssParser.IncludeDeclarationContext context = parse(lines).statement(0).includeDeclaration();

    assertThat(
            context
                .values()
                .commandStatement(0)
                .expression(0)
                .identifier()
                .identifierVariableName()
                .getText())
        .isEqualTo("$var1");
    assertThat(context.block().property(0).identifier().Identifier().getText()).isEqualTo("color-");
    assertThat(
            context
                .block()
                .property(0)
                .identifier()
                .identifierPart(0)
                .identifierVariableName()
                .getText())
        .isEqualTo("$var2");
  }
}
