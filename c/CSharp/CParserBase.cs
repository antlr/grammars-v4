using System.Collections.Generic;
using Antlr4.Runtime;
using System.IO;
using System.Linq;
using static System.Net.Mime.MediaTypeNames;

public abstract class CParserBase : Parser
{
    SymbolTable _st;
    private bool debug = false;
    private bool enable_all = true;

    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _st = new SymbolTable();
    }

    public bool IsAlignmentSpecifier()
    {
        if (enable_all) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write(text);
        var resolved = _st.Resolve(text);
        bool result = false;
        if (resolved == null)
        {
            result = false;
        }
        else if (resolved.Classification.Contains(TypeClassification.AlignmentSpecifier_))
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsAtomicTypeSpecifier()
    {
        if (enable_all) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write(text);
        var resolved = _st.Resolve(text);
        bool result = false;
        if (resolved == null)
        {
            result = false;
        }
        else if (resolved.Classification.Contains(TypeClassification.AtomicTypeSpecifier_))
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsAttributeDeclaration()
    {
        if (enable_all) return true;
        return IsAttributeSpecifierSequence();
    }

    public bool IsAttributeSpecifier()
    {
        if (enable_all) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write(token);
        var result = token.Type == CLexer.LeftBracket;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsAttributeSpecifierSequence()
    {
        if (enable_all) return true;
        return IsAttributeSpecifier();
    }

	public bool IsDeclaration()
	{
        if (enable_all) return true;
		var result = IsDeclarationSpecifiers()
                     || IsAttributeSpecifierSequence()
                     || IsStaticAssertDeclaration()
                     || IsAttributeDeclaration();
		return result;
	}

    public bool IsDeclarationSpecifier()
    {
        if (enable_all) return true;
        return
            IsStorageClassSpecifier()
            || IsTypeSpecifier()
            || IsTypeQualifier()
            || IsFunctionSpecifier()
            || IsAlignmentSpecifier();
    }

    public bool IsDeclarationSpecifiers()
    {
        return IsDeclarationSpecifier();
    }

    public bool IsEnumSpecifier()
    {
        if (enable_all) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write(token);
        var result = token.Type == CLexer.Enum;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsFunctionSpecifier()
    {
        if (enable_all) return true;
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
        else if (resolved.Classification.Contains(TypeClassification.FunctionSpecifier_))
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsStatement()
    {
        if (enable_all) return true;
        return !IsDeclaration();
    }

    public bool IsStaticAssertDeclaration()
	{
        if (enable_all) return true;
		var token = (this.InputStream as CommonTokenStream).LT(1);
		if (this.debug) System.Console.Write(token);
		var result = token.Type == CLexer.Static_assert;
		if (this.debug) System.Console.WriteLine(result);
		return result;
	}

    public bool IsStorageClassSpecifier()
    {
        if (enable_all) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write(text);
        var resolved = _st.Resolve(text);
        bool result = false;
        if (resolved == null)
        {
            result = false;
        }
        else if (resolved.Classification.Contains(TypeClassification.StorageClassSpecifier_))
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

	public bool IsStructOrUnionSpecifier()
	{
        if (enable_all) return true;
		var token = (this.InputStream as CommonTokenStream).LT(1);
		if (this.debug) System.Console.Write(token);
		var result = token.Type == CLexer.Struct ||
					 token.Type == CLexer.Union;
		if (this.debug) System.Console.WriteLine(result);
		return result;
	}


    public bool IsTypedefName()
    {
        if (enable_all) return true;
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
        else if (!resolved.Classification.Contains(TypeClassification.Variable_))
            result = true;
        else
            result = false;

        if (result) return result;
        return result;
    }

    public bool IsTypeofSpecifier()
    {
        if (enable_all) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write(token);
        var result = token.Type == CLexer.Typeof ||
                     token.Type == CLexer.Typeof_unqual;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }

    public bool IsTypeQualifier()
    {
        if (enable_all) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write(text);
        var resolved = _st.Resolve(text);
        bool result = false;
        if (resolved == null)
        {
            result = false;
        }
        else if (resolved.Classification.Contains(TypeClassification.TypeQualifier_))
            result = true;
        else
            result = false;
        if (this.debug) System.Console.WriteLine(result);
        return result;
    }


    public bool IsTypeSpecifier()
    {
        if (enable_all) return true;
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
        else if (resolved.Classification.Contains(TypeClassification.TypeSpecifier_))
            result = true;
        else
            result = false;

        if (result) return result;
        //if (this.debug) System.Console.WriteLine(result);
        result = IsAtomicTypeSpecifier() || IsStructOrUnionSpecifier() || IsEnumSpecifier()
            || IsTypedefName() || IsTypeofSpecifier();
        return result;
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
                    _st.Define(new Symbol() { Name = id, Classification = new HashSet<TypeClassification>() {TypeClassification.TypeSpecifier_} });
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
                        _st.Define(new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } });
                    else 
                        _st.Define(new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Variable_ } });
                }
            }
        }
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public bool IsNullStructDeclarationListExtension()
    {
        return true;
    }
}

