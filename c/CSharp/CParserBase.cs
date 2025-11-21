using Antlr4.Runtime;
using System.IO;
using System.Linq;

public abstract class CParserBase : Parser
{
    SymbolTable _st;
    
    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _st = new SymbolTable();
    }

    public bool IsType()
    {
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        var resolved = _st.Resolve(text);
        if (resolved == null)
            return false;
        else if (resolved.Classification != TypeClassification.Variable_)
            return true;
        return false;
    }

    public void EnterDeclaration()
    {
        ParserRuleContext context = this.Context;
        CParser.DeclarationContext declaration_context = (CParser.DeclarationContext)context;
        CParser.DeclarationSpecifiersContext declaration_specifiers = declaration_context.declarationSpecifiers();
        CParser.DeclarationSpecifierContext[] declaration_specifier = declaration_specifiers?.declarationSpecifier();
        bool is_typedef = declaration_specifier.Where(ds =>
        {
            return ds.typeSpecifier() != null;
        }).Any();
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
                    var text = identifier.GetText();
                    if (is_typedef)
                        _st.Define(new Symbol() { Name = text, Classification = TypeClassification.Type_ });
                    else 
                        _st.Define(new Symbol() { Name = text, Classification = TypeClassification.Variable_ });
                }
            }
        }
    }
}

