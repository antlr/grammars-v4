import org.antlr.v4.runtime.*;
import java.util.HashSet;
import java.util.Set;

public abstract class CSharpParserBase extends Parser
{
    private static final String[] ALL_SEMANTIC_FUNCTIONS = {
        "IsLocalVariableDeclaration"
    };

    private final Set<String> noSemantics;

    protected CSharpParserBase(TokenStream input)
    {
        super(input);
        noSemantics = parseNoSemantics(System.getProperty("sun.java.command", "").split("\\s+"));
    }

    private static Set<String> parseNoSemantics(String[] args)
    {
        Set<String> result = new HashSet<>();
        for (String a : args)
        {
            if (a.toLowerCase().startsWith("--no-semantics"))
            {
                int eq = a.indexOf('=');
                if (eq == -1)
                {
                    for (String f : ALL_SEMANTIC_FUNCTIONS) result.add(f);
                }
                else
                {
                    for (String f : a.substring(eq + 1).split(","))
                        result.add(f.trim());
                }
            }
        }
        return result;
    }

    protected boolean IsRightArrow() { return areAdjacent(); }
    protected boolean IsRightShift() { return areAdjacent(); }
    protected boolean IsRightShiftAssignment() { return areAdjacent(); }

    private boolean areAdjacent()
    {
        Token first  = _input.LT(-2);
        Token second = _input.LT(-1);
        return first != null && second != null &&
               first.getTokenIndex() + 1 == second.getTokenIndex();
    }

    protected boolean IsLocalVariableDeclaration()
    {
        if (noSemantics.contains("IsLocalVariableDeclaration")) return true;
        if (!(this._ctx instanceof CSharpParser.Local_variable_declarationContext)) {
            return false;
        }
        CSharpParser.Local_variable_declarationContext local_var_decl = (CSharpParser.Local_variable_declarationContext)this._ctx;
        if (local_var_decl == null) return true;
        CSharpParser.Local_variable_typeContext local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.getText().equals("var")) return false;
        return true;
    }
}
