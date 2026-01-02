using System.Collections.Generic;
using System;
using Antlr4.Runtime;
using System.IO;
using System.Linq;
using static System.Net.Mime.MediaTypeNames;

public abstract class CParserBase : Parser
{
    SymbolTable _st;
    private bool debug = false;
    private bool no_semantics = true;
    public List<string> _args;

    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        // Get options from process args.
        var args = Environment.GetCommandLineArgs().ToList();
        no_semantics = !(args?.Where(a => a.IndexOf("-semantics", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false);
        debug = args?.Where(a => a.IndexOf("-debug", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        _st = new SymbolTable();
    }

    public bool IsAlignmentSpecifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsAlignmentSpecifier " + text);
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
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsAtomicTypeSpecifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsAtomicTypeSpecifier " + text);
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
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsAttributeDeclaration()
    {
        if (no_semantics) return true;
        return IsAttributeSpecifierSequence();
    }

    public bool IsAttributeSpecifier()
    {
        if (no_semantics) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsAttributeSpecifier " + token);
        var result = token.Type == CLexer.LeftBracket;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsAttributeSpecifierSequence()
    {
        if (no_semantics) return true;
        return IsAttributeSpecifier();
    }

    public bool IsDeclaration()
    {
        if (no_semantics) return true;
        var result = IsDeclarationSpecifiers()
                     || IsAttributeSpecifierSequence()
                     || IsStaticAssertDeclaration()
                     || IsAttributeDeclaration();
        return result;
    }

    public bool IsDeclarationSpecifier()
    {
        if (no_semantics) return true;
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
        if (no_semantics) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsEnumSpecifier " + token);
        var result = token.Type == CLexer.Enum;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsFunctionSpecifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsFunctionSpecifier " + text);
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
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsStatement()
    {
        if (no_semantics) return true;
        var t1 = (this.InputStream as CommonTokenStream).LT(1);
        var t2 = (this.InputStream as CommonTokenStream).LT(2);
        if (this.debug) System.Console.Write("IsStatement1 " + t1);
        if (this.debug) System.Console.Write("IsStatement2 " + t2);
        if (t1.Type == CLexer.Identifier && t2.Type == CLexer.Colon)
        {
            if (this.debug) System.Console.Write("IsStatement3 true");
            return true;
        }
        return !IsDeclaration();
    }

    public bool IsStaticAssertDeclaration()
    {
        if (no_semantics) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsStaticAssertDeclaration " + token);
        var result = token.Type == CLexer.Static_assert;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsStorageClassSpecifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsStorageClassSpecifier " + text);
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
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsStructOrUnionSpecifier()
    {
        if (no_semantics) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsStructOrUnionSpecifier " + token);
        var result = token.Type == CLexer.Struct ||
                     token.Type == CLexer.Union;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }


    public bool IsTypedefName()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsTypedefName " + text);
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

        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsTypeofSpecifier()
    {
        if (no_semantics) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsTypeofSpecifier " + token);
        var result = token.Type == CLexer.Typeof ||
                     token.Type == CLexer.Typeof_unqual;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsTypeQualifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsTypeQualifier " + text);
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
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }


    public bool IsTypeSpecifier()
    {
        if (no_semantics) return true;
        var text = (this.InputStream as CommonTokenStream).LT(1).Text;
        if (this.debug) System.Console.Write("IsTypeSpecifier " + text);
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

        if (result) {
            if (this.debug) System.Console.WriteLine(" " +result);
            return result;
        }
        result = IsAtomicTypeSpecifier() || IsStructOrUnionSpecifier() || IsEnumSpecifier()
            || IsTypedefName() || IsTypeofSpecifier();
        if (this.debug) System.Console.WriteLine(" " +result);
        return result;
    }

    public void EnterDeclaration()
    {
        if (debug) System.Console.WriteLine("EnterDeclaration");
        ParserRuleContext context = this.Context;
        CParser.DeclarationContext declaration_context = context as CParser.DeclarationContext;
        CParser.DeclarationSpecifiersContext declaration_specifiers = declaration_context?.declarationSpecifiers();
        CParser.DeclarationSpecifierContext[] declaration_specifier = declaration_specifiers?.declarationSpecifier();
        CParser.DirectDeclaratorContext direct_declarator = context as CParser.DirectDeclaratorContext;

        // Declare any typeSpecifiers that declare something.
        if (declaration_specifier != null)
        {
            bool is_typedef = declaration_specifier?.Where(ds =>
            {
                return ds.storageClassSpecifier()?.Typedef() != null;
            }).Any() ?? false;
            foreach (var ds in declaration_specifier)
            {
                var sous = ds.typeSpecifier()?.structOrUnionSpecifier();
                if (sous != null)
                {
                    var id = sous.Identifier()?.GetText();
                    if (id != null)
                    {
                        if (debug) System.Console.WriteLine("Declaration1 Declaration " + id);
                        _st.Define(new Symbol() { Name = id, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } });
                    }
                }
            }
        }

        CParser.InitDeclaratorListContext init_declaration_list = declaration_context?.initDeclaratorList();
        CParser.InitDeclaratorContext[] init_declarators = init_declaration_list?.initDeclarator();
        if (init_declarators != null)
        {
            bool is_typedef = declaration_specifier?.Where(ds =>
            {
                return ds.storageClassSpecifier()?.Typedef() != null;
            }).Any() ?? false;
            foreach (var id in init_declarators)
            {
                CParser.DeclaratorContext y = id?.declarator();
                var identifier = y.directDeclarator()?.Identifier();
                if (identifier != null)
                {
                    // If a typedef is used in the declaration, the declarator
                    // itself is a type, not a variable.
                    var text = identifier.GetText();
                    if (is_typedef) {
                        if (debug) System.Console.WriteLine("Declaration2 Declarator " + text);
                        _st.Define(new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } });
                    }
                    else {
                        if (debug) System.Console.WriteLine("Declaration3 Declarator " + text);
                        _st.Define(new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Variable_ } });
                    }
                }
            }
        }

        if (direct_declarator != null)
        {
            RuleContext parent = direct_declarator;
            while (parent != null && parent as CParser.DeclarationContext == null)
            {
                parent = parent.Parent;
            }
            if (parent == null) return;
            var dss = (parent as CParser.DeclarationContext)?.declarationSpecifiers().declarationSpecifier();
            bool is_typedef = dss?.Where(ds =>
            {
                return ds.storageClassSpecifier()?.Typedef() != null;
            }).Any() ?? false;
            var identifier = direct_declarator.Identifier();
            if (identifier != null)
            {
                var text = identifier.GetText();
                if (debug) System.Console.WriteLine("Declaration4 direct_declarator " + text);
                if (is_typedef)
                {
                    if (debug) System.Console.WriteLine("Declaration2 Declarator " + text);
                    _st.Define(new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } });
                }
                else
                {
                    if (debug) System.Console.WriteLine("Declaration3 Declarator " + text);
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

