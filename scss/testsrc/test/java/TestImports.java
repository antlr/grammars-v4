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
public class TestImports extends TestBase {
  @Test
  public void testImport() {
    ScssParser.ReferenceUrlContext context = parseImport("@import \"hello\";").referenceUrl();
    assertThat(context.StringLiteral().getText()).isEqualTo("\"hello\"");
  }

  @Test
  public void testImportUrlString() {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(\"hello\");").referenceUrl();
    assertThat(context.Url().getText()).isEqualTo("\"hello\"");
  }

  @Test
  public void testImportUrlNonStrings() {
    ScssParser.ReferenceUrlContext context = parseImport("@import url(hello);").referenceUrl();
    assertThat(context.Url().getText()).isEqualTo("hello");
  }

  @Test
  public void testImportUrlNonStringsSpace() {
    ScssParser.ReferenceUrlContext context =
        parseImport("@import url(hello world);").referenceUrl();
    assertThat(context.Url().getText()).isEqualTo("hello world");
  }

  @Test
  public void testMultipleImports() {
    String[] lines = {"@import url(hello world);", "@import url(\"foobar\");", "@import \"hi\";"};

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).importDeclaration().referenceUrl().Url().getText())
        .isEqualTo("hello world");
    assertThat(context.statement(1).importDeclaration().referenceUrl().Url().getText())
        .isEqualTo("\"foobar\"");
    assertThat(context.statement(2).importDeclaration().referenceUrl().StringLiteral().getText())
        .isEqualTo("\"hi\"");
  }

  @Test
  public void testUse() {
    ScssParser.ImportDeclarationContext context = parseImport("@use \"sass:color\";");

    assertThat(context.referenceUrl().StringLiteral().getText()).isEqualTo("\"sass:color\"");
  }

  @Test
  public void testUseAs() {
    ScssParser.ImportDeclarationContext context =
        parseImport("@use 'third_party/javascript/angular2/material' as mat;");

    assertThat(context.referenceUrl().StringLiteral().getText())
        .isEqualTo("'third_party/javascript/angular2/material'");
    assertThat(context.asClause().identifier().getText()).isEqualTo("mat");
  }

  @Test
  public void testUseWith() {
    String[] lines = {
      "@use \"library\" with (",
      "  $base-color: $base-color,",
      "  $secondary-color: color.scale($base-color, $lightness: -10%),",
      ");"
    };
    ScssParser.ImportDeclarationContext context = parseImport(lines);

    assertThat(context.referenceUrl().StringLiteral().getText()).isEqualTo("\"library\"");
    assertThat(context.withClause().keywordArgument(0).getText())
        .isEqualTo("$base-color:$base-color");
    assertThat(context.withClause().keywordArgument(1).getText())
        .isEqualTo("$secondary-color:color.scale($base-color,$lightness:-10%)");
  }

  private ScssParser.ImportDeclarationContext parseImport(String... lines) {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).importDeclaration();
  }
}
