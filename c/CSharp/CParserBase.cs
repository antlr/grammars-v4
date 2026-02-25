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
    private bool output_symbol_table = false;
    private bool output_applied_occurrences = false;
    private HashSet<string> no_semantics = new HashSet<string>();
    public List<string> _args;

    // List of all semantic function names
    private static readonly string[] ALL_SEMANTIC_FUNCTIONS = {
        "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
        "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
        "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
        "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
        "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
        "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
        "IsNullStructDeclarationListExtension",
        "IsGnuAttributeBeforeDeclarator",
        "IsSomethingOfTypeName", "IsSpecifierQualifierList", "IsTypeName",
        "IsInitDeclaratorList"
    };

    protected CParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        // Get options from process args.
        var args = Environment.GetCommandLineArgs().ToList();
        no_semantics = ParseNoSemantics(args);
        debug = args?.Where(a => a.IndexOf("--debug", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        output_symbol_table = args?.Where(a => a.IndexOf("--output-symbol-table", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        output_applied_occurrences = args?.Where(a => a.IndexOf("--output-applied-occurrences", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        _st = new SymbolTable();
    }

    private static HashSet<string> ParseNoSemantics(List<string> args)
    {
        var result = new HashSet<string>();
        if (args == null) return result;
        foreach (var a in args)
        {
            if (a.StartsWith("--no-semantics", StringComparison.OrdinalIgnoreCase))
            {
                int eqIndex = a.IndexOf('=');
                if (eqIndex == -1)
                {
                    // --no-semantics without value: disable all semantic functions
                    foreach (var func in ALL_SEMANTIC_FUNCTIONS)
                    {
                        result.Add(func);
                    }
                }
                else
                {
                    // --no-semantics=Func1,Func2,...
                    var value = a.Substring(eqIndex + 1);
                    var funcs = value.Split(',');
                    foreach (var func in funcs)
                    {
                        result.Add(func.Trim());
                    }
                }
            }
        }
        return result;
    }

    public bool IsAlignmentSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsAlignmentSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsAlignmentSpecifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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

    public bool IsAtomicTypeSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsAtomicTypeSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsAtomicTypeSpecifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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
        if (no_semantics.Contains("IsAttributeDeclaration")) return true;
        return IsAttributeSpecifierSequence();
    }

    public bool IsAttributeSpecifier()
    {
        if (no_semantics.Contains("IsAttributeSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsAttributeSpecifier " + lt1);
        var result = lt1.Type == CLexer.LeftBracket;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsAttributeSpecifierSequence()
    {
        if (no_semantics.Contains("IsAttributeSpecifierSequence")) return true;
        return IsAttributeSpecifier();
    }

    public bool IsDeclaration()
    {
        if (no_semantics.Contains("IsDeclaration")) return true;
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
        if (no_semantics.Contains("IsDeclarationSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(1);
        var text = lt1.Text;
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier " + lt1);
        var result =
            IsStorageClassSpecifier()
            || IsTypeSpecifier()
            || IsTypeQualifier()
            || (IsFunctionSpecifier() && !IsGnuAttributeBeforeDeclarator())
            || IsAlignmentSpecifier();
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    public bool IsTypeSpecifierQualifier(int k = 1)
    {
        if (no_semantics.Contains("IsTypeSpecifierQualifier")) return true;
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier");
        var result =
            IsTypeSpecifier(k)
            || IsTypeQualifier(k)
            || IsAlignmentSpecifier(k);
        if (debug) System.Console.WriteLine("IsDeclarationSpecifier " + result);
        return result;
    }

    public bool IsDeclarationSpecifiers()
    {
        if (no_semantics.Contains("IsDeclarationSpecifiers")) return true;
        return IsDeclarationSpecifier();
    }

    public bool IsEnumSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsEnumSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        if (this.debug) System.Console.Write("IsEnumSpecifier " + lt1);
        var result = lt1.Type == CLexer.Enum;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsFunctionSpecifier()
    {
        if (no_semantics.Contains("IsFunctionSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(1);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsFunctionSpecifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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

    public bool IsGnuAttributeBeforeDeclarator(int k = 1)
    {
        if (no_semantics.Contains("IsGnuAttributeBeforeDeclarator")) return true;
        var ts = this.InputStream as CommonTokenStream;
        int i = k;
        if (ts.LT(i).Type != CLexer.Attribute) return false;
        i++;
        int depth = 0;
        while (true)
        {
            var t = ts.LT(i++);
            if (t.Type == TokenConstants.EOF) return false;
            if (t.Type == CLexer.LeftParen) depth++;
            else if (t.Type == CLexer.RightParen) { depth--; if (depth == 0) break; }
        }
        var next = ts.LT(i).Type;
        return next == CLexer.Identifier
            || next == CLexer.Star
            || next == CLexer.LeftParen;
    }

    public bool IsStatement()
    {
        if (no_semantics.Contains("IsStatement")) return true;
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
        if (no_semantics.Contains("IsStaticAssertDeclaration")) return true;
        var token = (this.InputStream as CommonTokenStream).LT(1);
        if (this.debug) System.Console.Write("IsStaticAssertDeclaration " + token);
        var result = token.Type == CLexer.Static_assert;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsStorageClassSpecifier()
    {
        if (no_semantics.Contains("IsStorageClassSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(1);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsStorageClassSpecifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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

    public bool IsStructOrUnionSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsStructOrUnionSpecifier")) return true;
        var token = (this.InputStream as CommonTokenStream).LT(k);
        if (this.debug) System.Console.Write("IsStructOrUnionSpecifier " + token);
        var result = token.Type == CLexer.Struct ||
                     token.Type == CLexer.Union;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }


    public bool IsTypedefName(int k = 1)
    {
        if (no_semantics.Contains("IsTypedefName")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsTypedefName " + lt1);
        var resolved = ResolveWithOutput(lt1);
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

    public bool IsTypeofSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsTypeofSpecifier")) return true;
        var token = (this.InputStream as CommonTokenStream).LT(k);
        if (this.debug) System.Console.Write("IsTypeofSpecifier " + token);
        var result = token.Type == CLexer.Typeof ||
                     token.Type == CLexer.Typeof_unqual;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }

    public bool IsTypeQualifier(int k = 1)
    {
        if (no_semantics.Contains("IsTypeQualifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsTypeQualifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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


    public bool IsTypeSpecifier(int k = 1)
    {
        if (no_semantics.Contains("IsTypeSpecifier")) return true;
        var lt1 = (this.InputStream as CommonTokenStream).LT(k);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsTypeSpecifier " + lt1);
        var resolved = ResolveWithOutput(lt1);
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
        result = IsAtomicTypeSpecifier(k) || IsStructOrUnionSpecifier(k) || IsEnumSpecifier(k)
            || IsTypedefName(k) || IsTypeofSpecifier(k);
        if (this.debug) System.Console.WriteLine(" " +result);
        return result;
    }

    public void EnterDeclaration()
    {
        if (debug) System.Console.WriteLine("EnterDeclaration");
        ParserRuleContext context = this.Context;
        for (; context != null; context = (ParserRuleContext)((ParserRuleContext)context).Parent)
        {
            var declaration_context = context as CParser.DeclarationContext;
            var declaration_specifiers = declaration_context?.declarationSpecifiers();
            var declaration_specifier = declaration_specifiers?.declarationSpecifier();
            var declarator = context as CParser.DeclaratorContext;
            CParser.InitDeclaratorListContext init_declarator_list = declaration_context?.initDeclaratorList();
            CParser.InitDeclaratorContext[] init_declarators = init_declarator_list?.initDeclarator();
            if (init_declarators != null)
            {
                bool is_typedef = declaration_specifier?.Where(ds =>
                {
                    return ds.storageClassSpecifier()?.Typedef() != null;
                }).Any() ?? false;
                foreach (var id in init_declarators)
                {
                    CParser.DeclaratorContext y = id?.declarator();
                    var idToken = GetDeclarationToken(y);
                    if (idToken != null)
                    {
                        // If a typedef is used in the declaration, the declarator
                        // itself is a type, not a variable.
                        var text = idToken.Text;
                        var loc = GetSourceLocation(idToken);
                        if (is_typedef) {
                            var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, DefinedFile = loc.File, DefinedLine = loc.Line, DefinedColumn = loc.Column };
                            _st.Define(symbol);
                            if (debug) System.Console.WriteLine("New symbol Declaration2 Declarator " + symbol);
                        } else {
                            var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Variable_ }, DefinedFile = loc.File, DefinedLine = loc.Line, DefinedColumn = loc.Column };
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
                    var idToken = identifier.Symbol;
                    var text = idToken.Text;
                    var loc = GetSourceLocation(idToken);
                    var symbol = new Symbol() { Name = text, Classification = new HashSet<TypeClassification>() { TypeClassification.Function_ }, DefinedFile = loc.File, DefinedLine = loc.Line, DefinedColumn = loc.Column };
                    _st.Define(symbol);
                    if (debug) System.Console.WriteLine("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
        }
    }

    private string GetDeclarationId(CParser.DeclaratorContext y)
    {
        var token = GetDeclarationToken(y);
        return token?.Text;
    }

    private IToken GetDeclarationToken(CParser.DeclaratorContext y)
    {
        // Go down the tree and find a declarator with Identifier.
        if (y == null) return null;

        // Check if this declarator has a direct declarator with an identifier
        var directDeclarator = y.directDeclarator();
        if (directDeclarator != null)
        {
            var more = directDeclarator.declarator();
            var token = GetDeclarationToken(more);
            if (token != null) return token;
            if (directDeclarator.Identifier() != null)
            {
                return directDeclarator.Identifier().Symbol;
            }
        }

        return null;
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public bool IsNullStructDeclarationListExtension()
    {
        if (no_semantics.Contains("IsNullStructDeclarationListExtension")) return true;
        return true;
    }

    public void EnterScope()
    {
        if (debug) System.Console.WriteLine("EnterScope");
        _st.PushBlockScope();
    }

    public void ExitScope()
    {
        if (debug) System.Console.WriteLine("ExitScope");
        _st.PopBlockScope();
    }

    public void LookupSymbol()
    {
        // Get the token that was just parsed (the Identifier)
        var token = (this.InputStream as CommonTokenStream).LT(-1);
        if (token == null) return;
        var text = token.Text;
        var resolved = _st.Resolve(text);
        if (output_applied_occurrences && resolved != null)
        {
            var appliedLoc = GetSourceLocation(token);
            System.Console.Error.WriteLine($"Applied occurrence: {text} at {appliedLoc.File}:{appliedLoc.Line}:{appliedLoc.Column} -> Defined at {resolved.DefinedFile}:{resolved.DefinedLine}:{resolved.DefinedColumn}");
        }
    }

    public void OutputSymbolTable()
    {
        if (output_symbol_table)
        {
            System.Console.Error.WriteLine(_st.ToString());
        }
    }

    private Symbol ResolveWithOutput(IToken token)
    {
        if (token == null) return null;
        var text = token.Text;
        var resolved = _st.Resolve(text);
        if (output_applied_occurrences && resolved != null)
        {
            var appliedLoc = GetSourceLocation(token);
            System.Console.Error.WriteLine($"Applied occurrence: {text} at {appliedLoc.File}:{appliedLoc.Line}:{appliedLoc.Column} -> Defined at {resolved.DefinedFile}:{resolved.DefinedLine}:{resolved.DefinedColumn}");
        }
        return resolved;
    }

    // Helper class to hold source location information
    private class SourceLocation
    {
        public string File { get; set; }
        public int Line { get; set; }
        public int Column { get; set; }

        public SourceLocation(string file, int line, int column)
        {
            File = file;
            Line = line;
            Column = column;
        }
    }

    // Compute source file, line, and column from a token, accounting for #line directives
    private SourceLocation GetSourceLocation(IToken token)
    {
        if (token == null)
        {
            return new SourceLocation("", 0, 0);
        }

        string fileName = "<unknown>";
        int line = token.Line;
        int column = token.Column;
        int lineAdjusted = line;

        var ts = this.InputStream as CommonTokenStream;
        int ind = token.TokenIndex;

        // Search back from token index to find last LineDirective
        for (int j = ind; j >= 0; j--)
        {
            var t = ts.Get(j);
            if (t == null) break;
            if (t.Type == CLexer.LineDirective)
            {
                // Found it
                var txt = t.Text;
                var parts = txt.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length >= 3)
                {
                    if (int.TryParse(parts[1], out int dirLine))
                    {
                        int lineDirective = t.Line;
                        int lineDiff = line - lineDirective;
                        lineAdjusted = lineDiff + dirLine - 1;
                        fileName = parts[2].Trim();
                        // Remove quotes if present
                        if (fileName.StartsWith("\"") && fileName.EndsWith("\""))
                        {
                            fileName = fileName.Substring(1, fileName.Length - 2);
                        }
                    }
                }
                break;
            }
        }

        return new SourceLocation(fileName, lineAdjusted, column);
    }

    public bool IsInitDeclaratorList()
    {
        // Cannot be initDeclaratorList if the first thing is a type.
        // Types need to go to preceeding declarationSpecifiers.
        if (no_semantics.Contains("IsInitDeclaratorList")) return true;
        var ts = this.InputStream as CommonTokenStream;
        var lt1 = (this.InputStream as CommonTokenStream).LT(1);
        var text = lt1.Text;
        if (this.debug) System.Console.Write("IsInitDeclaratorList " + lt1);
        var resolved = ResolveWithOutput(lt1);
        bool result = false;
        if (resolved == null)
        {
            result = true;
        }
        else if (resolved.Classification.Contains(TypeClassification.TypeQualifier_) || resolved.Classification.Contains(TypeClassification.TypeSpecifier_))
            result = false;
        else
            result = true;
        if (this.debug) System.Console.WriteLine(" " + result);
        return result;
    }
    
    public bool IsSomethingOfTypeName()
    {
        // Returns true if current position looks like "sizeof ( typedefName )"
        // Used to prevent sizeof from entering the ('++' | '--' | 'sizeof')* loop
        // when it is actually a sizeof(type) expression, avoiding ambiguity.
        if (no_semantics.Contains("IsSomethingOfTypeName")) return true;
        var ts = this.InputStream as CommonTokenStream;
        if (!(ts.LT(1).Type == CLexer.Sizeof ||
            ts.LT(1).Type == CLexer.Countof ||
            ts.LT(1).Type == CLexer.Alignof ||
            ts.LT(1).Type == CLexer.Maxof ||
            ts.LT(1).Type == CLexer.Minof)
            ) return false;
        if (ts.LT(2).Type != CLexer.LeftParen) return false;
        if (IsTypeName(3)) return true;
        return false;
    }

    public bool IsTypeName(int k = 1)
    {
        if (no_semantics.Contains("IsTypeName")) return true;
        return IsSpecifierQualifierList(k);
    }

    public bool IsSpecifierQualifierList(int k = 1)
    {
        if (no_semantics.Contains("IsSpecifierQualifierList")) return true;
        if (IsGnuAttributeBeforeDeclarator(k)) return true;
        if (IsTypeSpecifierQualifier(k)) return true;
        return false;
    }
    
    public bool IsCast()
    {
        var result = false;
        // Look for a cast.
        if (no_semantics.Contains("IsCast")) return true;
        var t1 = (this.InputStream as CommonTokenStream).LT(1);
        var t2 = (this.InputStream as CommonTokenStream).LT(2);
        if (this.debug) System.Console.WriteLine("IsCast1 " + t1);
        if (this.debug) System.Console.WriteLine("IsCast2 " + t2);
        if (t1.Type != CLexer.LeftParen)
        {
            if (this.debug) System.Console.Write("IsCast " + result);
        } else if (t2.Type != CLexer.Identifier)
        {
            // Assume typecast until otherwise.
            result = true;
        } else
        {
            // Check id.
            var resolved = ResolveWithOutput(t2);
            if (resolved == null)
            {
                // It's not in symbol table.
                result = false;
            } else if (resolved.Classification.Contains(TypeClassification.TypeSpecifier_))
                result = true;
            else
                result = false;
        }
        if (this.debug) System.Console.Write("IsStatement " + result);
        return result;
    }
}

