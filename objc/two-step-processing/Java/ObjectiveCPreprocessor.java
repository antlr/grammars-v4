package objc;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ListTokenSource;
import org.antlr.v4.runtime.Token;

import java.util.*;

/**
 * Created by ikochurkin on 28.07.2016.
 */
public class ObjectiveCPreprocessor extends ObjectiveCPreprocessorParserBaseVisitor<String> {
    private final Deque<Boolean> _conditions = new ArrayDeque<Boolean>();
    private boolean _compilied = true;
    private CommonTokenStream _tokensStream;

    public Map<String, String> ConditionalSymbols = new HashMap<String, String>();

    public ObjectiveCPreprocessor(CommonTokenStream commonTokenStream)
    {
        _conditions.push(true);
        _tokensStream = commonTokenStream;
    }

    @Override
    public String visitObjectiveCDocument(ObjectiveCPreprocessorParser.ObjectiveCDocumentContext ctx) {
        StringBuilder result = new StringBuilder();
        for (ObjectiveCPreprocessorParser.TextContext text : ctx.text()) {
            result.append(visit(text));
        }
        return result.toString();
    }

    @Override
    public String visitText(ObjectiveCPreprocessorParser.TextContext context)
    {
        String result = _tokensStream.getText(context);
        boolean directive = false;
        if (context.directive() != null)
        {
            _compilied = visit(context.directive()) == Boolean.toString(true);
            directive = true;
        }
        if (!_compilied || directive)
        {
            StringBuilder sb = new StringBuilder(result.length());
            char[] charArray = result.toCharArray();
            for (char c : charArray) {
                sb.append((c == '\r' || c == '\n') ? c : ' ');
            }
            result = sb.toString();
        }
        return result;
    }

    @Override
    public String visitPreprocessorImport(ObjectiveCPreprocessorParser.PreprocessorImportContext context)
    {
        return Boolean.toString(IsCompiliedText());
    }

    @Override
    public String visitPreprocessorConditional(ObjectiveCPreprocessorParser.PreprocessorConditionalContext context)
    {
        if (context.IF() != null)
        {
            boolean exprResult = visit(context.preprocessor_expression()) == Boolean.toString(true);
            _conditions.push(exprResult);
            return Boolean.toString(exprResult && IsCompiliedText());
        }
        else if (context.ELIF() != null)
        {
            _conditions.pop();
            boolean exprResult = visit(context.preprocessor_expression()) == Boolean.toString(true);
            _conditions.push(exprResult);
            return Boolean.toString(exprResult && IsCompiliedText());
        }
        else if (context.ELSE() != null)
        {
            boolean val = _conditions.pop();
            _conditions.push(!val);
            return Boolean.toString(!val ? IsCompiliedText() : false);
        }
        else
        {
            _conditions.pop();
            return Boolean.toString(_conditions.peek());
        }
    }

    @Override
    public String visitPreprocessorDef(ObjectiveCPreprocessorParser.PreprocessorDefContext context)
    {
        String conditionalSymbolText = context.CONDITIONAL_SYMBOL().getText();
        if (context.IFDEF() != null || context.IFNDEF() != null)
        {
            boolean condition = ConditionalSymbols.containsKey(conditionalSymbolText);
            if (context.IFNDEF() != null)
            {
                condition = !condition;
            }
            _conditions.push(condition);
            return Boolean.toString(condition && IsCompiliedText());
        }
        else
        {
            if (IsCompiliedText())
            {
                ConditionalSymbols.remove(conditionalSymbolText);
            }
            return Boolean.toString(IsCompiliedText());
        }
    }

    @Override
    public String visitPreprocessorPragma(ObjectiveCPreprocessorParser.PreprocessorPragmaContext context)
    {
        return Boolean.toString(IsCompiliedText());
    }

    @Override
    public String visitPreprocessorError(ObjectiveCPreprocessorParser.PreprocessorErrorContext context)
    {
        return Boolean.toString(IsCompiliedText());
    }

    @Override
    public String visitPreprocessorDefine(ObjectiveCPreprocessorParser.PreprocessorDefineContext context)
    {
        if (IsCompiliedText())
        {
            StringBuilder str = new StringBuilder();
            for (ObjectiveCPreprocessorParser.Directive_textContext d : context.directive_text()) {
                str.append(d.TEXT() != null ? d.TEXT() : "\r\n");
            }
            String directiveText = str.toString().trim();
            ConditionalSymbols.put(context.CONDITIONAL_SYMBOL().getText().replace(" ", ""), directiveText);
        }
        return Boolean.toString(IsCompiliedText());
    }

    @Override
    public String visitPreprocessorConstant(ObjectiveCPreprocessorParser.PreprocessorConstantContext context)
    {
        if (context.TRUE() != null || context.FALSE() != null)
        {
            return Boolean.toString(context.TRUE() != null);
        }
        else
        {
            return context.getText();
        }
    }

    @Override
    public String visitPreprocessorConditionalSymbol(ObjectiveCPreprocessorParser.PreprocessorConditionalSymbolContext context)
    {
        String symbol = ConditionalSymbols.get(context.CONDITIONAL_SYMBOL().getText());
        if (symbol != null) {
            return symbol;
        } else {
            return Boolean.toString(false);
        }
    }

    @Override
    public String visitPreprocessorParenthesis(ObjectiveCPreprocessorParser.PreprocessorParenthesisContext context)
    {
        return visit(context.preprocessor_expression());
    }

    @Override
    public String visitPreprocessorNot(ObjectiveCPreprocessorParser.PreprocessorNotContext context)
    {
        return Boolean.toString(!Boolean.parseBoolean(visit(context.preprocessor_expression())));
    }

    @Override
    public String visitPreprocessorBinary(ObjectiveCPreprocessorParser.PreprocessorBinaryContext context)
    {
        String expr1Result = visit(context.preprocessor_expression(0));
        String expr2Result = visit(context.preprocessor_expression(1));
        String op = context.op.getText();
        boolean result;
        switch (op)
        {
            case "&&":
                result = expr1Result == Boolean.toString(true) && expr2Result == Boolean.toString(true);
                break;
            case "||":
                result = expr1Result == Boolean.toString(true) || expr2Result == Boolean.toString(true);
                break;
            case "==":
                result = expr1Result == expr2Result;
                break;
            case "!=":
                result = expr1Result != expr2Result;
                break;
            case "<":
            case ">":
            case "<=":
            case ">=":
                int x1, x2;
                if (tryParseInt(expr1Result) && tryParseInt(expr2Result))
                {
                    x1 = Integer.parseInt(expr1Result);
                    x2 = Integer.parseInt(expr2Result);
                    switch (op)
                    {
                        case "<":
                            result = x1 < x2;
                            break;
                        case ">":
                            result = x1 > x2;
                            break;
                        case "<=":
                            result = x1 <= x2;
                            break;
                        case ">=":
                            result = x1 >= x2;
                            break;
                    }
                }
                result = false;
                break;
            default:
                result = true;
                break;
        }
        return Boolean.toString(result);
    }

    @Override
    public String visitPreprocessorDefined(ObjectiveCPreprocessorParser.PreprocessorDefinedContext context)
    {
        return Boolean.toString(ConditionalSymbols.containsKey(context.CONDITIONAL_SYMBOL().getText()));
    }

    private boolean IsCompiliedText()
    {
        return !_conditions.contains(false);
    }

    private boolean tryParseInt(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
