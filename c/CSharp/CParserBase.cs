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
    private bool no_semantics = false;
    public List<string> _args;

    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        // Get options from process args.
        var args = Environment.GetCommandLineArgs().ToList();
        no_semantics = (args?.Where(a => a.IndexOf("-no-semantics", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false);
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
        if (debug) System.Console.WriteLine("IsDeclaration");
        var result = IsDeclarationSpecifiers()
                     || IsAttributeSpecifierSequence()
                     || IsStaticAssertDeclaration()
                     || IsAttributeDeclaration();
        if (debug) System.Console.WriteLine("IsDeclaration " + result);
        return result;
    }

    public bool IsDeclarationSpecifier()
    {
        if (no_semantics) return true;
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier");
        var result = 
            IsStorageClassSpecifier()
            || IsTypeSpecifier()
            || IsTypeQualifier()
            || IsFunctionSpecifier()
            || IsAlignmentSpecifier();
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier " + result);
        return result;
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
        if (this.debug) System.Console.WriteLine("IsFunctionSpecifier " + result);
        return result;
    }

    public bool IsStatement()
    {
        if (no_semantics) return true;
        var t1 = (this.InputStream as CommonTokenStream).LT(1);
        var t2 = (this.InputStream as CommonTokenStream).LT(2);
        if (this.debug) System.Console.WriteLine("IsStatement1 " + t1);
        if (this.debug) System.Console.WriteLine("IsStatement2 " + t2);
        if (t1.Type == CLexer.Identifier && t2.Type == CLexer.Colon)
        {
            if (this.debug) System.Console.Write("IsStatement3 true");
            return true;
        }
        var result = ! IsDeclaration();
        if (this.debug) System.Console.Write("IsStatement " + result);
        return result;
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
        else if (resolved.Classification.Contains(TypeClassification.Variable_))
            result = false;
        else if (resolved.Classification.Contains(TypeClassification.Function_))
            result = false;
        else
            result = true;

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
        for (; context != null; )
        {
            CParser.DeclarationContext declaration_context = context as CParser.DeclarationContext;
            CParser.DeclarationSpecifiersContext declaration_specifiers = declaration_context?.declarationSpecifiers();
            CParser.DeclarationSpecifierContext[] declaration_specifier = declaration_specifiers?.declarationSpecifier();
            CParser.DeclaratorContext declarator = context as CParser.DeclaratorContext;
            // Declare any typeSpecifiers that declare something.
            //if (declaration_specifier != null)
            //{
            //    bool is_typedef = declaration_specifier?.Where(ds =>
            //    {
            //        return ds.storageClassSpecifier()?.Typedef() != null;
            //    }).Any() ?? false;
            //    foreach (var ds in declaration_specifier)
            //    {
            //        var sous = ds.typeSpecifier()?.structOrUnionSpecifier();
            //        if (sous != null)
            //        {
            //            var id = sous.Identifier()?.GetText();
            //            if (id != null)
            //            {
            //                if (debug) System.Console.WriteLine("New symbol Declaration1 Declaration " + id);
            //                _st.Define(new Symbol() { Name = id, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } });
            //                return;
            //            }
            //        }
            //    }
            //}
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
                    string identifier = GetDeclarationId(y);
                    if (identifier != null)
                    {
                        // If a typedef is used in the declaration, the declarator
                        // itself is a type, not a variable.
                        var text = identifier;
                        if (is_typedef) {
                            var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ } };
                            _st.Define(symbol);
                            if (debug) System.Console.WriteLine("New symbol Declaration2 Declarator " + symbol);
                        } else {
                            var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Variable_ } };
                            _st.Define(symbol);
                            if (debug) System.Console.WriteLine("New symbol Declaration3 Declarator " + symbol);
                        }
                    }
                }
            }
            if (context as CParser.FunctionDefinitionContext != null)
            {
                var fd = (context as CParser.FunctionDefinitionContext);
                var de = fd?.declarator();
                var dd = de.directDeclarator();
                var identifier = dd?.Identifier();
                if (identifier != null) {
                    var text = identifier.GetText();
                    var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Function_ } };
                    _st.Define(symbol);
                    if (debug) System.Console.WriteLine("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
            context = (ParserRuleContext)((ParserRuleContext)context).Parent;
        }
    }

    private string GetDeclarationId(CParser.DeclaratorContext y)
    {
        // Go down the tree and find a declarator with Identifier.
        if (y == null) return null;
        
        // Check if this declarator has a direct declarator with an identifier
        var directDeclarator = y.directDeclarator();
        if (directDeclarator != null)
        {
            var more = directDeclarator.declarator();
            var xxx = GetDeclarationId(more);
            if (xxx != null) return xxx;
            if (directDeclarator.Identifier() != null)
            {
                return directDeclarator.Identifier().GetText();
            }
        }

        return null;
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public bool IsNullStructDeclarationListExtension()
    {
        return true;
    }
}

