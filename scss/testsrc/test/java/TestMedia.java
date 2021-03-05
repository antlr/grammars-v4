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

/**
 * Most tests taken from here
 * https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries
 */
@RunWith(JUnit4.class)
public class TestMedia extends TestBase {
  @Test
  public void oneMediaType() {
    String[] lines = {
      "@media print {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("print");
  }

  @Test
  public void twoMediaTypes() {
    String[] lines = {
      "@media screen, print {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("screen");
    assertThat(context.mediaQuery(1).mediaType().getText()).isEqualTo("print");
  }

  @Test
  public void mediaFeature() {
    String[] lines = {
      "@media (hover: hover) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("hover");
    assertThat(context.mediaQuery(0).mediaExpression(0).commandStatement().getText())
        .isEqualTo("hover");
  }

  @Test
  public void rangeMediaFeature() {
    String[] lines = {
      "@media (max-width: 12450px) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("max-width");
    assertThat(context.mediaQuery(0).mediaExpression(0).commandStatement().getText())
        .isEqualTo("12450px");
  }

  @Test
  public void mediaFeatureWithoutValue() {
    String[] lines = {
      "@media (color) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("color");
  }

  @Test
  public void combiningMediaFeatures() {
    String[] lines = {
      "@media (min-width: 30em) and (orientation: landscape) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("min-width");
    assertThat(context.mediaQuery(0).mediaExpression(1).mediaFeature().getText())
        .isEqualTo("orientation");
  }

  @Test
  public void mediaTypeWithMediaFeatures() {
    String[] lines = {
      "@media screen and (min-width: 30em) and (orientation: landscape) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("screen");
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("min-width");
    assertThat(context.mediaQuery(0).mediaExpression(1).mediaFeature().getText())
        .isEqualTo("orientation");
  }

  @Test
  public void multipleMediaQueries() {
    String[] lines = {
      "@media (min-height: 680px), screen and (orientation: portrait) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("min-height");
    assertThat(context.mediaQuery(1).mediaType().getText()).isEqualTo("screen");
    assertThat(context.mediaQuery(1).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("orientation");
  }

  @Test
  public void invertingMediaQuery() {
    String[] lines = {
      "@media not all and (monochrome) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).NOT()).isNotNull();
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("all");
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("monochrome");
  }

  @Test
  public void invertingOneOfManyMediaQueries() {
    String[] lines = {
      "@media not screen and (color), print and (color) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).NOT()).isNotNull();
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("screen");
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("color");
    assertThat(context.mediaQuery(1).NOT()).isNull();
    assertThat(context.mediaQuery(1).mediaType().getText()).isEqualTo("print");
    assertThat(context.mediaQuery(1).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("color");
  }

  @Test
  public void onlyKeyword() {
    String[] lines = {
      "@media only screen and (color) {}",
    };

    ScssParser.MediaQueryListContext context = getMedia(lines);
    assertThat(context.mediaQuery(0).ONLY()).isNotNull();
    assertThat(context.mediaQuery(0).mediaType().getText()).isEqualTo("screen");
    assertThat(context.mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("color");
  }

  @Test
  public void nestedMediaQueries() {
    String[] lines = {
      "@media (hover: hover) {",
      "  .button:hover {",
      "    border: 2px solid black;",
      "    @media (color) {",
      "      border-color: #036;",
      "    }",
      "  }",
      "}",
    };

    ScssParser.StylesheetContext context = parse(lines);
    assertThat(
            context
                .statement(0)
                .mediaDeclaration()
                .block()
                .statement(0)
                .ruleset()
                .selectors()
                .selector(0)
                .getText())
        .isEqualTo(".button:hover");
    ScssParser.MediaDeclarationContext innerMedia =
        context
            .statement(0)
            .mediaDeclaration()
            .block()
            .statement(0)
            .ruleset()
            .block()
            .statement(0)
            .mediaDeclaration();
    assertThat(
            innerMedia.mediaQueryList().mediaQuery(0).mediaExpression(0).mediaFeature().getText())
        .isEqualTo("color");
    assertThat(
            innerMedia
                .block()
                .property(0)
                .propertyValue()
                .commandStatement(0)
                .expression()
                .Color()
                .getText())
        .isEqualTo("#036");
  }

  private ScssParser.MediaQueryListContext getMedia(String... lines) {
    ScssParser.StylesheetContext context = parse(lines);
    return context.statement(0).mediaDeclaration().mediaQueryList();
  }
}
