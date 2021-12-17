import org.antlr.v4.runtime.*;
import java.util.Stack;

abstract class CSharpParserBase extends Parser
{
    protected CSharpParserBase(TokenStream input)
    {
	super(input);
    }

    protected boolean VerifyVar()
    {
	CSharpParser.Local_variable_declarationContext local_var_decl = (CSharpParser.Local_variable_declarationContext)this._ctx;
	System.out.println("Yo1 " + local_var_decl);
	if (local_var_decl == null) return true;
	var local_variable_type = local_var_decl.local_variable_type();
	System.out.println("Yo2 " + local_variable_type);
	if (local_variable_type == null) return true;
	System.out.println("Yo3 " + local_variable_type.getText());
	if (local_variable_type.getText().equals("var")) return false;
	return true;
    }
}
