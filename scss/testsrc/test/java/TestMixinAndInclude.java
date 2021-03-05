/*
 [The "BSD licence"]

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

/** Most tests taken from here https://sass-lang.com/documentation/at-rules/mixin */
@RunWith(JUnit4.class)
public final class TestMixinAndInclude extends TestBase {

  @Test
  public void noArguments() {
    String[] lines = {
      "@mixin reset-list {",
      "  margin: 0;",
      "  padding: 0;",
      "  list-style: none;",
      "}",
      "",
      "@mixin horizontal-list {",
      "  @include reset-list;",
      "",
      "  li {",
      "    display: inline-block;",
      "    margin: {",
      "      left: -2px;",
      "      right: 2em;",
      "    }",
      "  }",
      "}",
      "",
      "nav ul {",
      "  @include horizontal-list;",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).mixinDeclaration().Identifier().getText())
        .isEqualTo("reset-list");
    assertThat(context.statement(0).mixinDeclaration().declaredParams()).isNull();
    assertThat(context.statement(1).mixinDeclaration().Identifier().getText())
        .isEqualTo("horizontal-list");
    assertThat(context.statement(1).mixinDeclaration().declaredParams()).isNull();

    ScssParser.IncludeDeclarationContext include1 =
        context.statement(1).mixinDeclaration().block().statement(0).includeDeclaration();
    assertThat(include1.Identifier().getText()).isEqualTo("reset-list");
    assertThat(include1.functionCall()).isNull();

    ScssParser.IncludeDeclarationContext include2 =
        context.statement(2).ruleset().block().statement(0).includeDeclaration();
    assertThat(include2.Identifier().getText()).isEqualTo("horizontal-list");
    assertThat(include2.functionCall()).isNull();
  }

  @Test
  public void basicArguments() {
    String[] lines = {
      "@mixin rtl($property, $ltr-value, $rtl-value) {",
      "  #{$property}: $ltr-value;",
      "",
      "  [dir=rtl] & {",
      "    #{$property}: $rtl-value;",
      "  }",
      "}",
      "",
      ".sidebar {",
      "  @include rtl(float, left, right);",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).mixinDeclaration().declaredParams().declaredParam(0).getText())
        .isEqualTo("$property");
    assertThat(context.statement(0).mixinDeclaration().declaredParams().declaredParam(1).getText())
        .isEqualTo("$ltr-value");
    assertThat(context.statement(0).mixinDeclaration().declaredParams().declaredParam(2).getText())
        .isEqualTo("$rtl-value");

    ScssParser.IncludeDeclarationContext include =
        context.statement(1).ruleset().block().statement(0).includeDeclaration();
    assertThat(include.functionCall().passedParams().passedParam(0).getText()).isEqualTo("float");
    assertThat(include.functionCall().passedParams().passedParam(1).getText()).isEqualTo("left");
    assertThat(include.functionCall().passedParams().passedParam(2).getText()).isEqualTo("right");
  }

  @Test
  public void optionalArguments() {
    String[] lines = {
      "@mixin replace-text($image, $x: 50%, $y: 50%) {",
      "  text-indent: -99999em;",
      "  overflow: hidden;",
      "  text-align: left;",
      "",
      "  background: {",
      "    image: $image;",
      "    repeat: no-repeat;",
      "    position: $x $y;",
      "  }",
      "}",
      "",
      ".mail-icon {",
      "  @include replace-text(url(\"/images/mail.svg\"), 0);",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    ScssParser.MixinDeclarationContext mixin = context.statement(0).mixinDeclaration();
    assertThat(mixin.declaredParams().Ellipsis()).isNull();
    assertThat(mixin.declaredParams().declaredParam(0).variableName().getText())
        .isEqualTo("$image");
    assertThat(mixin.declaredParams().declaredParam(0).paramOptionalValue()).isNull();
    assertThat(mixin.declaredParams().declaredParam(1).variableName().getText()).isEqualTo("$x");
    assertThat(mixin.declaredParams().declaredParam(1).paramOptionalValue().expression(0).getText())
        .isEqualTo("50%");
    assertThat(mixin.declaredParams().declaredParam(2).variableName().getText()).isEqualTo("$y");
    assertThat(mixin.declaredParams().declaredParam(2).paramOptionalValue().expression(0).getText())
        .isEqualTo("50%");

    ScssParser.IncludeDeclarationContext include =
        context.statement(1).ruleset().block().statement(0).includeDeclaration();
    assertThat(include.functionCall().passedParams().Ellipsis()).isNull();
    assertThat(include.functionCall().passedParams().passedParam()).hasSize(2);
    assertThat(include.functionCall().passedParams().passedParam(0).getText())
        .isEqualTo("url(\"/images/mail.svg\")");
    assertThat(include.functionCall().passedParams().passedParam(1).getText()).isEqualTo("0");
  }

  @Test
  public void keywordArguments() {
    String[] lines = {
      "@mixin square($size, $radius: 0) {",
      "  width: $size;",
      "  height: $size;",
      "",
      "  @if $radius != 0 {",
      "    border-radius: $radius;",
      "  }",
      "}",
      "",
      ".avatar {",
      "  @include square(100px, $radius: 4px);",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    ScssParser.IncludeDeclarationContext include =
        context.statement(1).ruleset().block().statement(0).includeDeclaration();
    assertThat(include.functionCall().passedParams().passedParam(0).variableName()).isNull();
    assertThat(include.functionCall().passedParams().passedParam(0).commandStatement().getText())
        .isEqualTo("100px");
    assertThat(include.functionCall().passedParams().passedParam(1).variableName().getText())
        .isEqualTo("$radius");
    assertThat(include.functionCall().passedParams().passedParam(1).commandStatement().getText())
        .isEqualTo("4px");
  }

  @Test
  public void argumentList() {
    String[] lines = {
      "@mixin order($height, $selectors...) {",
      "  @for $i from 0 to length($selectors) {",
      // TODO(nickwalther) uncomment this when improving interpolation.
      /*"    #{nth($selectors, $i + 1)} {",
      "      position: absolute;",
      "      height: $height;",
      "      margin-top: $i * $height;",
      "    }",*/
      "  }",
      "}",
      "",
      "@include order(150px, \"input.name\", \"input.address\", \"input.zip\");",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).mixinDeclaration().declaredParams().declaredParam()).hasSize(2);
    assertThat(context.statement(0).mixinDeclaration().declaredParams().Ellipsis()).isNotNull();
    assertThat(
            context.statement(1).includeDeclaration().functionCall().passedParams().passedParam())
        .hasSize(4);
  }

  @Test
  public void argumentListWithArbitraryKeywordArguments() {
    String[] lines = {
      "@mixin syntax-colors($args...) {",
      "  @each $name, $color in keywords($args) {",
      "    pre span.stx-#{$name} {",
      "      color: $color;",
      "    }",
      "  }",
      "}",
      "",
      "@include syntax-colors(",
      "  $string: #080,",
      "  $comment: #800,",
      "  $variable: #60b,",
      ")",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).mixinDeclaration().declaredParams().declaredParam(0).getText())
        .isEqualTo("$args");
    assertThat(context.statement(0).mixinDeclaration().declaredParams().Ellipsis()).isNotNull();
    assertThat(
            context
                .statement(1)
                .includeDeclaration()
                .functionCall()
                .passedParams()
                .passedParam(0)
                .getText())
        .isEqualTo("$string:#080");
    assertThat(
            context
                .statement(1)
                .includeDeclaration()
                .functionCall()
                .passedParams()
                .passedParam(1)
                .getText())
        .isEqualTo("$comment:#800");
    assertThat(
            context
                .statement(1)
                .includeDeclaration()
                .functionCall()
                .passedParams()
                .passedParam(2)
                .getText())
        .isEqualTo("$variable:#60b");
  }

  @Test
  public void passingArbitraryArguments() {
    String[] lines = {
      "$form-selectors: \"input.name\", \"input.address\", \"input.zip\" !default;",
      "",
      "@include order(150px, $form-selectors...);",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(context.statement(0).variableDeclaration().variableName().getText())
        .isEqualTo("$form-selectors");
    assertThat(context.statement(0).variableDeclaration().propertyValue().commandStatement())
        .hasSize(3);
    assertThat(
            context.statement(1).includeDeclaration().functionCall().passedParams().passedParam())
        .hasSize(2);
    assertThat(context.statement(1).includeDeclaration().functionCall().passedParams().Ellipsis())
        .isNotNull();
  }

  @Test
  public void includeWithContentBlock() {
    String[] lines = {
      "@mixin hover {",
      "  &:not([disabled]):hover {",
      "    @content;",
      "  }",
      "}",
      "",
      ".button {",
      "  border: 1px solid black;",
      "  @include hover {",
      "    border-width: 2px;",
      "  }",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);

    assertThat(
            context
                .statement(0)
                .mixinDeclaration()
                .block()
                .statement(0)
                .ruleset()
                .block()
                .statement(0)
                .contentDeclaration())
        .isNotNull();
    ScssParser.IncludeDeclarationContext include =
        context.statement(1).ruleset().block().statement(0).includeDeclaration();
    assertThat(include.Identifier().getText()).isEqualTo("hover");
    assertThat(include.functionCall()).isNull();
    assertThat(include.block().property(0).getText()).isEqualTo("border-width:2px;");
  }

  @Test
  public void contentBlockWithArguments() {
    String[] lines = {
      "@mixin media($types...) {",
      "  @each $type in $types {",
      // TODO(nickwalther) uncomment this when improving interpolation.
      // "    @media #{$type} {",
      "      @content($type);",
      // "    }",
      "  }",
      "}",
      "",
      "@include media(screen, print) using ($type) {",
      "  h1 {",
      "    font-size: 40px;",
      "    @if $type == print {",
      "      font-family: Calluna;",
      "    }",
      "  }",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    ScssParser.ContentDeclarationContext content =
        context
            .statement(0)
            .mixinDeclaration()
            .block()
            .statement(0)
            .eachDeclaration()
            .block()
            .statement(0)
            .contentDeclaration();
    assertThat(content.passedParams().passedParam(0).getText()).isEqualTo("$type");
    ScssParser.IncludeDeclarationContext include = context.statement(1).includeDeclaration();
    assertThat(include.USING()).isNotNull();
    assertThat(include.declaredParams().declaredParam(0).getText()).isEqualTo("$type");
  }
}
