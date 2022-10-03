import org.antlr.v4.runtime.*;
import java.util.Stack;

public abstract class CSharpParserBase extends Parser
{
    protected CSharpParserBase(TokenStream input)
    {
        super(input);
    }

    protected boolean IsLocalVariableDeclaration()
    {
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
