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

@RunWith(JUnit4.class)
public final class TestListAndMap extends TestBase {
  @Test
  public void listSpaceSeparated() {
    String[] lines = {
      "$var: list.nth(10px 1em 0 -5px, 2);",
    };

    ScssParser.ListSpaceSeparatedContext list =
        parse(lines)
            .statement(0)
            .variableDeclaration()
            .propertyValue()
            .commandStatement(0)
            .expression()
            .functionCall()
            .passedParams()
            .passedParam(0)
            .listSpaceSeparated();
    assertThat(list.listElement()).hasSize(4);
    assertThat(list.listElement(0).commandStatement().getText()).isEqualTo("10px");
    assertThat(list.listElement(1).commandStatement().getText()).isEqualTo("1em");
    assertThat(list.listElement(2).commandStatement().getText()).isEqualTo("0");
    assertThat(list.listElement(3).commandStatement().getText()).isEqualTo("-5px");
  }

  @Test
  public void listCommaSeparated() {
    String[] lines = {
      "@each $var in red, blue, yellow {}",
    };

    ScssParser.ListCommaSeparatedContext list =
        parse(lines).statement(0).eachDeclaration().eachValueList().list().listCommaSeparated();
    assertThat(list.listElement()).hasSize(3);
    assertThat(list.listElement(0).commandStatement().getText()).isEqualTo("red");
    assertThat(list.listElement(1).commandStatement().getText()).isEqualTo("blue");
    assertThat(list.listElement(2).commandStatement().getText()).isEqualTo("yellow");
  }

  @Test
  public void listBracketedSpaceSeparated() {
    String[] lines = {
      "$var: [$a $b];",
    };

    ScssParser.ListSpaceSeparatedContext list =
        parse(lines).statement(0).variableDeclaration().listBracketed().listSpaceSeparated();
    assertThat(list.listElement()).hasSize(2);
    assertThat(list.listElement(0).commandStatement().getText()).isEqualTo("$a");
    assertThat(list.listElement(1).commandStatement().getText()).isEqualTo("$b");
  }

  @Test
  public void listBracketedCommaSeparated() {
    String[] lines = {
      "$var: [1 + 2, 3 / 4];",
    };

    ScssParser.ListCommaSeparatedContext list =
        parse(lines).statement(0).variableDeclaration().listBracketed().listCommaSeparated();
    assertThat(list.listElement()).hasSize(2);
    assertThat(list.listElement(0).commandStatement().getText()).isEqualTo("1+2");
    assertThat(list.listElement(1).commandStatement().getText()).isEqualTo("3/4");
  }

  @Test
  public void nestedList() {
    String[] lines = {
      "$var: [(a b) (c, (d e f), g, h)];",
    };

    ScssParser.ListSpaceSeparatedContext outerList =
        parse(lines).statement(0).variableDeclaration().listBracketed().listSpaceSeparated();
    assertThat(outerList.listElement(0).list().listSpaceSeparated().listElement()).hasSize(2);
    assertThat(outerList.listElement(1).list().listCommaSeparated().listElement()).hasSize(4);
    assertThat(
            outerList
                .listElement(1)
                .list()
                .listCommaSeparated()
                .listElement(1)
                .list()
                .listSpaceSeparated()
                .listElement())
        .hasSize(3);
  }

  @Test
  public void map() {
    String[] lines = {"$font-weights: (\"regular\": 400, \"medium\": 500, \"bold\": 700);"};

    ScssParser.MapContext map = parse(lines).statement(0).variableDeclaration().map();
    assertThat(map.mapEntry()).hasSize(3);
    assertThat(map.mapEntry(0).mapKey().commandStatement().getText()).isEqualTo("\"regular\"");
    assertThat(map.mapEntry(0).mapValue().commandStatement().getText()).isEqualTo("400");
  }

  @Test
  public void mapUnquotedKeys() {
    String[] lines = {"$var: (1: 2, a: b, red: blue);"};

    ScssParser.MapContext map = parse(lines).statement(0).variableDeclaration().map();
    assertThat(map.mapEntry()).hasSize(3);
    assertThat(map.mapEntry(2).mapKey().commandStatement().getText()).isEqualTo("red");
    assertThat(map.mapEntry(2).mapValue().commandStatement().getText()).isEqualTo("blue");
  }

  @Test
  public void mapListValue() {
    String[] lines = {
      "$new-theme: map-merge(",
      "  base-theme(),",
      "  (",
      "    background-color: $backgroundColor,",
      "    border: 1px solid $borderColor,",
      "  )",
      ");",
    };

    ScssParser.MapContext map =
        parse(lines)
            .statement(0)
            .variableDeclaration()
            .propertyValue()
            .commandStatement(0)
            .expression()
            .functionCall()
            .passedParams()
            .passedParam(1)
            .map();
    assertThat(map.mapEntry()).hasSize(2);
    ScssParser.ListSpaceSeparatedContext list =
        map.mapEntry(1).mapValue().list().listSpaceSeparated();
    assertThat(list.listElement()).hasSize(3);
    assertThat(list.listElement(0).getText()).isEqualTo("1px");
    assertThat(list.listElement(1).getText()).isEqualTo("solid");
    assertThat(list.listElement(2).getText()).isEqualTo("$borderColor");
  }

  @Test
  public void nestedMap() {
    String[] lines = {"$config: (a: (b: (c: d)));"};

    ScssParser.MapContext map = parse(lines).statement(0).variableDeclaration().map();
    assertThat(
            map.mapEntry(0)
                .mapValue()
                .map()
                .mapEntry(0)
                .mapValue()
                .map()
                .mapEntry(0)
                .mapValue()
                .getText())
        .isEqualTo("d");
  }
}
