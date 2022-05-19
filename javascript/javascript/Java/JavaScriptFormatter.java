import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import static jjtrans.JavaScriptParser.*;
import static org.antlr.v4.runtime.Recognizer.EOF;

// TODO:
//  - option to enable comments (how?)
//  - method to get current line length
//  - split long lines in:
//      [x] objectLiteral
//      [ ] expressionSequence
//      [ ] MemberDotExpression
//      [ ] arguments
//  - option to insert "missing" semicolons
public class JavaScriptFormatter extends JavaScriptParserBaseVisitor<Void> {

    StringBuilder source;
    int indentLevel = 0;
    int previousToken = -1;

    public JavaScriptFormatter() {
        source = new StringBuilder();
    }
    void write(String s) {
        source.append(s);
    }
    void space() {
        source.append(" ");
    }

    void newLine() {
        source.append("\n");
    }

    void indent() {
        source.append(getIndents(0));
    }

    void newLineIndent() {
        newLine();
        indent();
    }

    String getIndents(int minus) {
        return "\t".repeat(Math.max(0, indentLevel - minus));
    }

    @java.lang.Override
    public java.lang.Void visitChildren(RuleNode node) {
        int rule = node.getRuleContext().getRuleIndex();
        enterRule(rule, node);
        if (shouldAdjustIndent(rule))
            indentLevel++;
        super.visitChildren(node);
        if (shouldAdjustIndent(rule))
            indentLevel--;
        return null;
    }

    public String getSource() {
        // replaceAll can be removed when double spaces are removed. `let obj = {  }`.
        return source.toString().replaceAll(" +", " ");
    }

    @java.lang.Override
    public java.lang.Void visitIdentifier(IdentifierContext ctx) {
        int parentType = ctx.parent.getRuleIndex();
        if (ctx.parent instanceof ForOfStatementContext && ctx.getText().equals("of")) {
            space();
            super.visitIdentifier(ctx);
            space();
        } else if (parentType == RULE_getter || parentType == RULE_setter) {
            super.visitIdentifier(ctx);
            space();
        } else {
            super.visitIdentifier(ctx);
        }
        return null;
    }

    @java.lang.Override
    public java.lang.Void visitIfStatement(IfStatementContext ctx) {
        boolean elseIf = previousToken == Else;
        if (elseIf) {
            indentLevel--;
            space();
        } else {
            newLineIndent();
        }
        visit(ctx.If());
        space();
        visit(ctx.OpenParen());
        visit(ctx.expressionSequence());
        visit(ctx.CloseParen());
        visit(ctx.statement(0));
        if (ctx.Else() != null) {
            newLineIndent();
            visit(ctx.Else());
            var stmt = ctx.statement(1);
            visit(stmt);
        }
        if (elseIf)
            indentLevel++;
        return null;
    }

    @java.lang.Override
    public java.lang.Void visitStatement(StatementContext ctx) {
        var childBlock = ctx.getChild(0) instanceof BlockContext;
        var parentIterIf = ctx.getParent() instanceof IfStatementContext ||
                ctx.getParent() instanceof IterationStatementContext;
        if (!childBlock && parentIterIf) {
            indentLevel++;
            super.visitStatement(ctx);
            indentLevel--;
        } else {
            if (childBlock && !parentIterIf)
                newLine();
            super.visitStatement(ctx);
        }
        return null;
    }

    @java.lang.Override
    public java.lang.Void visitObjectLiteral(ObjectLiteralContext ctx) {
        boolean largeObject = ctx.propertyAssignment().size() > 1;
        if (largeObject)
            indentLevel++;
        else
            return super.visitObjectLiteral(ctx);
        for (var c : ctx.children.subList(0, ctx.children.size() - 1)) {
            if (c instanceof PropertyAssignmentContext)
                newLineIndent();
            visit(c);
        }
        indentLevel--;
        newLineIndent();
        source.append("}");
        return null;
    }

    boolean shouldAdjustIndent(int ruleType) {
        return switch (ruleType) {
            case RULE_block, RULE_functionBody, RULE_classTail, RULE_objectLiteral -> true;
            default -> false;
        };
    }

    private void enterRule(int rule, RuleNode node) {
        switch (rule) {
            case RULE_classElement -> {
                if (!(node.getChild(0) instanceof EmptyStatement_Context)) {
                    newLineIndent();
                }
            }
            case RULE_iterationStatement, RULE_variableStatement, RULE_importStatement,
                    RULE_exportStatement, RULE_classDeclaration, RULE_expressionStatement, RULE_continueStatement,
                    RULE_breakStatement, RULE_returnStatement, RULE_withStatement, RULE_labelledStatement,
                    RULE_switchStatement, RULE_throwStatement, RULE_tryStatement, RULE_debuggerStatement,
                    RULE_functionDeclaration -> {
                newLineIndent();
            }
        }
    }

    @java.lang.Override
    public java.lang.Void visitTerminal(TerminalNode terminal) {
        var type = terminal.getSymbol().getType();
        if (type == EOF) {
            // remove first empty line
            source.replace(0, 1, "");
            return null;
        }
        var p = terminal.getParent();
        var before = "";
        var after = "";
        switch (type) {
            case OpenBrace -> {
                if (p instanceof ClassTailContext ||
                        p instanceof FunctionBodyContext ||
                        previousToken == CloseParen)
                    before = " ";
                else
                    after = " ";
            }
            case CloseBrace -> {
                before = p instanceof ObjectLiteralContext ||
                        p instanceof ClassTailContext && ((ClassTailContext) p).classElement().stream()
                                .allMatch(ctx -> ctx.emptyStatement_() != null) ||
                        p instanceof FunctionBodyContext &&
                                ((FunctionBodyContext) p).sourceElements() == null
                        ? " " : "\n" + getIndents(1);
            }
            case Multiply -> {
                if (p instanceof PropertyAssignmentContext) {
                } else if (!(p instanceof MethodDefinitionContext))
                    after = " ";
                else if (!(p instanceof FunctionDeclarationContext
                        || p instanceof AnonymousFunctionDeclContext))
                    before = " ";
            }
            case Function_ -> {
                if (p instanceof FunctionDeclarationContext &&
                        ((FunctionDeclarationContext) p).Multiply() == null) {
                    after = " ";
                }
            }
            case Assign, Plus, Minus, NullCoalesce, Modulus, RightShiftArithmetic,
                    LeftShiftArithmetic, RightShiftLogical, LessThan, MoreThan, LessThanEquals,
                    GreaterThanEquals, Equals_, NotEquals, IdentityEquals, IdentityNotEquals, BitAnd,
                    BitXOr, BitOr, And, Or, MultiplyAssign, DivideAssign, ModulusAssign, PlusAssign,
                    MinusAssign, LeftShiftArithmeticAssign, RightShiftArithmeticAssign,
                    RightShiftLogicalAssign, BitAndAssign, BitXorAssign, BitOrAssign, PowerAssign, ARROW,
                    Extends, Catch -> {
                before = after = " ";
            }
            case Break, Do, Instanceof, Typeof, Case, New, Var,
                    Finally, Return, Void, Continue, For, Switch, While, Debugger,
                    With, Default, Throw, Delete, In, Try, As,
                    From, Class, Enum, Const, Export, Import, Async,
                    Await, Implements, StrictLet, NonStrictLet, Private, Public, Interface,
                    Package, Protected, Static, Yield, Comma, Colon -> {
                after = " ";
            }
        }
        write(before + terminal.getText() + after);
        previousToken = type;
        return null;
    }

    public static String format(RuleNode node) {
        var formatter = new JavaScriptFormatter();
        formatter.visit(node);
        return formatter.getSource();
    }

}
