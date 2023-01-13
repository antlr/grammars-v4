/*
 [The "MIT licence"]
 Copyright (c) 2014 Kyle Lee
 All rights reserved.
*/

import org.testng.Assert;
import org.testng.annotations.Test;


public class TestMixins extends TestBase {

    @Test
    public void testMixinDef() {
        String[] lines = {
                "#name() {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().selectors().selector(0).getText(), "#name");
    }

    @Test
    public void testMixinDefinitionWithParameters() {
        String[] lines = {
                "#name(@n1; @n2) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(0).variableName().Identifier().getText(), "n1");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(1).variableName().Identifier().getText(), "n2");
    }

    @Test
    public void testMixinDefinitionWithParametersAndDefaultValue() {
        String[] lines = {
                "#name(@n1:blue; @n2:red) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(0).variableDeclaration().variableName().Identifier().getText(), "n1");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(0).variableDeclaration().values().getText(), "blue");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(1).variableDeclaration().variableName().Identifier().getText(), "n2");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(1).variableDeclaration().values().getText(), "red");
    }

    @Test
    public void testMixinDefinitionWithEllipsis() {
        String[] lines = {
                "#name(...) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().Ellipsis().getText(), "...");
    }

    @Test
    public void testMixinDefinitionWithParamAndEllipsis()
    {
        String [] lines = {
                "#name(@param...) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinDefinitionParam(0).variableName().Identifier().getText(), "param");
        Assert.assertEquals(context.statement(0).mixinDefinition().Ellipsis().getText(), "...");
    }

    @Test
    public void testMixinDefinitionWithGuard() {
        String[] lines = {
                "#name(@a) when (lightness(@a) >= 50%) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(0).expression(0).identifier().getText(), "lightness");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(0).expression(0).values().commandStatement(0).expression(0).variableName().getText(), "@a");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().GTEQ().getText(), ">=");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(1).expression(0).measurement().Number().getText(), "50");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(1).expression(0).measurement().Unit().getText(), "%");
    }

    @Test
    public void testMixinDefinitionWithGuardCondition1() {
        String[] lines = {
                "#name(@a) when (isnumber(@a)) and (@a > 0) {}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(0).expression(0).identifier().getText(), "isnumber");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(0).conditionStatement().commandStatement(0).expression(0).values().commandStatement(0).expression(0).variableName().getText(), "@a");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().AND(0).getText(), "and");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(1).conditionStatement().commandStatement(0).expression(0).variableName().getText(), "@a");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(1).conditionStatement().GT().getText(), ">");
        Assert.assertEquals(context.statement(0).mixinDefinition().mixinGuard().conditions().condition(1).conditionStatement().commandStatement(1).expression(0).measurement().Number().getText(), "0");
    }

    @Test
    public void testMixinRefererence() {
        String[] lines = {
                "#usemixin { .classmixin(); #idmixin(); #importantMixin()!important;}"
        };
        LessParser.StylesheetContext context = parse(lines);
        Assert.assertEquals(context.statement(0).ruleset().block().mixinReference(0).selector().getText(), ".classmixin");
        Assert.assertEquals(context.statement(0).ruleset().block().mixinReference(1).selector().getText(), "#idmixin");
        Assert.assertEquals(context.statement(0).ruleset().block().mixinReference(2).IMPORTANT().getText(), "!important");
    }

    private LessParser.SelectorsContext getSelector(String... lines) {
        LessParser.StylesheetContext context = parse(lines);
        return context.statement(0).ruleset().selectors();
    }

    private LessParser.ExpressionContext createProperty(String... lines) {
        String[] all = new String[lines.length + 2];
        all[0] = "h1 {";
        System.arraycopy(lines, 0, all, 1, lines.length);
        all[all.length - 1] = "}";

        LessParser.StylesheetContext styleContext = parse(all);
        LessParser.BlockContext context = styleContext.statement(0).ruleset().block();
        return context.property(0).values().commandStatement(0).expression(0);
    }
}
