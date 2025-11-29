using Antlr4.Runtime;
using System.IO;
using System.Linq;
using static System.Net.Mime.MediaTypeNames;

public abstract class CParserBase : Parser
{
    SymbolTable _st;
    private bool debug = false;
    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _st = new SymbolTable();
    }

    public bool IsType()
    {
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write(text);
        var resolved = _st.Resolve(text);
        bool result = false;
        if (resolved == null)
        {
            //// C can reference unresolved types if it's a
            // pointer.
            //var la2 = (this.InputStream as CommonTokenStream).LT(2).Text;
            //if (la2 != null && la2 == "*")
            //    result = true;
            //else
                result = false;
        }
        else if (resolved.Classification != TypeClassification.Variable_)
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsNotType()
    {
	    return ! IsType();
    }
    
    public void EnterDeclaration()
    {
        ParserRuleContext context = this.Context;
        CParser.DeclarationContext declaration_context = (CParser.DeclarationContext)context;
        CParser.DeclarationSpecifiersContext declaration_specifiers = declaration_context.declarationSpecifiers();
        CParser.DeclarationSpecifierContext[] declaration_specifier = declaration_specifiers?.declarationSpecifier();
        bool is_typedef = declaration_specifier.Where(ds =>
        {
            return ds.storageClassSpecifier()?.Typedef() != null;
        }).Any();

        // Declare any typeSpecifiers that declare something.
        foreach (var ds in declaration_specifier)
        {
            var sous = ds.typeSpecifier()?.structOrUnionSpecifier();
            if (sous != null)
            {
                var id = sous.Identifier()?.GetText();
                if (id != null)
                    _st.Define(new Symbol() { Name = id, Classification = TypeClassification.Type_ });
            }
        }

        CParser.InitDeclaratorListContext init_declaration_list = declaration_context.initDeclaratorList();
        CParser.InitDeclaratorContext[] x = init_declaration_list?.initDeclarator();
        if (x != null)
        {
            foreach (var id in x)
            {
                CParser.DeclaratorContext y = id?.declarator();
                var identifier = y.directDeclarator()?.Identifier();
                if (identifier != null)
                {
                    // If a typedef is used in the declaration, the declarator
                    // itself is a type, not a variable.
                    var text = identifier.GetText();
                    if (is_typedef)
                        _st.Define(new Symbol() { Name = text, Classification = TypeClassification.Type_ });
                    else 
                        _st.Define(new Symbol() { Name = text, Classification = TypeClassification.Variable_ });
                }
            }
        }
	}

	// Define to return "true" because "gcc -c -std=c2x" accepts an empty
	// struct-declaration-list.
	public bool NullStructDeclarationListExtension()
	{
		return true;
	}
}

