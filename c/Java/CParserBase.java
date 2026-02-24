import org.antlr.v4.runtime.*;
import java.util.*;

public abstract class CParserBase extends Parser {
    private SymbolTable _st;
    private boolean debug = false;
    private boolean outputSymbolTable = false;
    private boolean outputAppliedOccurrences = false;
    private Set<String> noSemantics = new HashSet<>();

    // List of all semantic function names
    private static final String[] ALL_SEMANTIC_FUNCTIONS = {
        "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
        "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
        "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
        "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
        "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
        "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
        "IsNullStructDeclarationListExtension",
        "IsGnuAttributeBeforeDeclarator",
        "IsSizeofTypeName"
    };

    protected CParserBase(TokenStream input) {
        super(input);
        // Get options from system property
        String cmdLine = System.getProperty("sun.java.command");
        String[] args = cmdLine != null ? cmdLine.split("\\s+") : new String[0];
        noSemantics = parseNoSemantics(args);
        debug = hasArg(args, "--debug");
        outputSymbolTable = hasArg(args, "--output-symbol-table");
        outputAppliedOccurrences = hasArg(args, "--output-applied-occurrences");
        _st = new SymbolTable();
    }

    private static boolean hasArg(String[] args, String arg) {
        for (String a : args) {
            if (a.toLowerCase().contains(arg.toLowerCase())) {
                return true;
            }
        }
        return false;
    }

    private static Set<String> parseNoSemantics(String[] args) {
        Set<String> result = new HashSet<>();
        for (String a : args) {
            String lower = a.toLowerCase();
            if (lower.startsWith("--no-semantics")) {
                int eqIndex = a.indexOf('=');
                if (eqIndex == -1) {
                    // --no-semantics without value: disable all semantic functions
                    for (String func : ALL_SEMANTIC_FUNCTIONS) {
                        result.add(func);
                    }
                } else {
                    // --no-semantics=Func1,Func2,...
                    String value = a.substring(eqIndex + 1);
                    String[] funcs = value.split(",");
                    for (String func : funcs) {
                        result.add(func.trim());
                    }
                }
            }
        }
        return result;
    }

    public boolean IsAlignmentSpecifier() { return IsAlignmentSpecifier(1); }
    public boolean IsAlignmentSpecifier(int k) {
        if (noSemantics.contains("IsAlignmentSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsAlignmentSpecifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.AlignmentSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsAtomicTypeSpecifier() { return IsAtomicTypeSpecifier(1); }
    public boolean IsAtomicTypeSpecifier(int k) {
        if (noSemantics.contains("IsAtomicTypeSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsAtomicTypeSpecifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.AtomicTypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsAttributeDeclaration() {
        if (noSemantics.contains("IsAttributeDeclaration")) return true;
        return IsAttributeSpecifierSequence();
    }

    public boolean IsAttributeSpecifier() {
        if (noSemantics.contains("IsAttributeSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        if (this.debug) System.out.print("IsAttributeSpecifier " + lt1);
        boolean result = lt1.getType() == CLexer.LeftBracket;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsAttributeSpecifierSequence() {
        if (noSemantics.contains("IsAttributeSpecifierSequence")) return true;
        return IsAttributeSpecifier();
    }

    public boolean IsDeclaration() {
        if (noSemantics.contains("IsDeclaration")) return true;
        if (debug) System.out.println("IsDeclaration");
        boolean result = IsDeclarationSpecifiers()
                || IsAttributeSpecifierSequence()
                || IsStaticAssertDeclaration()
                || IsAttributeDeclaration();
        if (debug) System.out.println("IsDeclaration " + result);
        return result;
    }

    public boolean IsDeclarationSpecifier() {
        if (noSemantics.contains("IsDeclarationSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (debug) System.out.println("IsDeclarationSpecifier " + lt1);
        boolean result = IsStorageClassSpecifier()
                || IsTypeSpecifier()
                || IsTypeQualifier()
                || (IsFunctionSpecifier() && !IsGnuAttributeBeforeDeclarator())
                || IsAlignmentSpecifier();
        if (debug) System.out.println("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    public boolean IsTypeSpecifierQualifier() { return IsTypeSpecifierQualifier(1); }
    public boolean IsTypeSpecifierQualifier(int k) {
        if (noSemantics.contains("IsTypeSpecifierQualifier")) return true;
        if (debug) System.out.println("IsDeclarationSpecifier");
        boolean result = IsTypeSpecifier(k)
                || IsTypeQualifier(k)
                || IsAlignmentSpecifier(k);
        if (debug) System.out.println("IsDeclarationSpecifier " + result);
        return result;
    }

    public boolean IsDeclarationSpecifiers() {
        return IsDeclarationSpecifier();
    }

    public boolean IsEnumSpecifier() { return IsEnumSpecifier(1); }
    public boolean IsEnumSpecifier(int k) {
        if (noSemantics.contains("IsEnumSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        if (this.debug) System.out.print("IsEnumSpecifier " + lt1);
        boolean result = lt1.getType() == CLexer.Enum;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsFunctionSpecifier() {
        if (noSemantics.contains("IsFunctionSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsFunctionSpecifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.FunctionSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) System.out.println("IsFunctionSpecifier " + result);
        return result;
    }

    public boolean IsGnuAttributeBeforeDeclarator() { return IsGnuAttributeBeforeDeclarator(1); }
    public boolean IsGnuAttributeBeforeDeclarator(int k) {
        if (noSemantics.contains("IsGnuAttributeBeforeDeclarator")) return true;
        CommonTokenStream ts = (CommonTokenStream) this.getInputStream();
        int i = k;
        if (ts.LT(i).getType() != CLexer.Attribute) return false;
        i++;
        int depth = 0;
        while (true) {
            Token t = ts.LT(i++);
            if (t.getType() == Token.EOF) return false;
            if (t.getType() == CLexer.LeftParen) depth++;
            else if (t.getType() == CLexer.RightParen) { depth--; if (depth == 0) break; }
        }
        int next = ts.LT(i).getType();
        return next == CLexer.Identifier
            || next == CLexer.Star
            || next == CLexer.LeftParen;
    }

    public boolean IsStatement() {
        if (noSemantics.contains("IsStatement")) return true;
        Token t1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        Token t2 = ((CommonTokenStream) this.getInputStream()).LT(2);
        if (this.debug) System.out.println("IsStatement1 " + t1);
        if (this.debug) System.out.println("IsStatement2 " + t2);
        if (t1.getType() == CLexer.Identifier && t2.getType() == CLexer.Colon) {
            if (this.debug) System.out.print("IsStatement3 true");
            return true;
        }
        boolean result = !IsDeclaration();
        if (this.debug) System.out.print("IsStatement " + result);
        return result;
    }

    public boolean IsStaticAssertDeclaration() {
        if (noSemantics.contains("IsStaticAssertDeclaration")) return true;
        Token token = ((CommonTokenStream) this.getInputStream()).LT(1);
        if (this.debug) System.out.print("IsStaticAssertDeclaration " + token);
        boolean result = token.getType() == CLexer.Static_assert;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsStorageClassSpecifier() {
        if (noSemantics.contains("IsStorageClassSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsStorageClassSpecifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.StorageClassSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsStructOrUnionSpecifier() { return IsStructOrUnionSpecifier(1); }
    public boolean IsStructOrUnionSpecifier(int k) {
        if (noSemantics.contains("IsStructOrUnionSpecifier")) return true;
        Token token = ((CommonTokenStream) this.getInputStream()).LT(k);
        if (this.debug) System.out.print("IsStructOrUnionSpecifier " + token);
        boolean result = token.getType() == CLexer.Struct ||
                token.getType() == CLexer.Union;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypedefName() { return IsTypedefName(1); }
    public boolean IsTypedefName(int k) {
        if (noSemantics.contains("IsTypedefName")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypedefName " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.Variable_)) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.Function_)) {
            result = false;
        } else {
            result = true;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypeofSpecifier() { return IsTypeofSpecifier(1); }
    public boolean IsTypeofSpecifier(int k) {
        if (noSemantics.contains("IsTypeofSpecifier")) return true;
        Token token = ((CommonTokenStream) this.getInputStream()).LT(k);
        if (this.debug) System.out.print("IsTypeofSpecifier " + token);
        boolean result = token.getType() == CLexer.Typeof ||
                token.getType() == CLexer.Typeof_unqual;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypeQualifier() { return IsTypeQualifier(1); }
    public boolean IsTypeQualifier(int k) {
        if (noSemantics.contains("IsTypeQualifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypeQualifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.TypeQualifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypeSpecifier() { return IsTypeSpecifier(1); }
    public boolean IsTypeSpecifier(int k) {
        if (noSemantics.contains("IsTypeSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(k);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypeSpecifier " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = false;
        } else if (resolved.getClassification().contains(TypeClassification.TypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }

        if (result) {
            if (this.debug) System.out.println(" " + result);
            return result;
        }
        result = IsAtomicTypeSpecifier(k) || IsStructOrUnionSpecifier(k) || IsEnumSpecifier(k)
                || IsTypedefName(k) || IsTypeofSpecifier(k);
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public void EnterDeclaration() {
        if (debug) System.out.println("EnterDeclaration");
        ParserRuleContext context = this.getContext();
        while (context != null) {
            if (context instanceof CParser.DeclarationContext) {
                CParser.DeclarationContext declaration_context = (CParser.DeclarationContext) context;
                CParser.DeclarationSpecifiersContext declaration_specifiers = declaration_context.declarationSpecifiers();
                CParser.DeclarationSpecifierContext[] declaration_specifier = declaration_specifiers != null ?
                        declaration_specifiers.declarationSpecifier().toArray(new CParser.DeclarationSpecifierContext[0]) : null;

                // Declare any typeSpecifiers that declare something.
                if (declaration_specifier != null)
                {
                    boolean isTypedef = false;
                    if (declaration_specifier != null) {
                        for (CParser.DeclarationSpecifierContext ds : declaration_specifier) {
                            if (ds.storageClassSpecifier() != null && ds.storageClassSpecifier().Typedef() != null) {
                                isTypedef = true;
                                break;
                            }
                        }
                    }
                    for (CParser.DeclarationSpecifierContext ds : declaration_specifier) {
                        if (ds != null && ds.typeSpecifier() != null) {
                            var sous = ds.typeSpecifier().structOrUnionSpecifier();
                            if (sous != null && sous.Identifier() != null)
                            {
                                var idToken = sous.Identifier().getSymbol();
                                var id = idToken.getText();
                                if (id != null)
                                {
                                    if (debug) System.out.println("New symbol Declaration1 Declarator " + id);
                                    Symbol symbol = new Symbol();
                                    symbol.setName(id);
                                    HashSet<TypeClassification> classSet = new HashSet<>();
                                    classSet.add(TypeClassification.TypeSpecifier_);
                                    symbol.setClassification(classSet);
                                    SourceLocation loc = getSourceLocation(idToken);
                                    symbol.setDefinedFile(loc.file);
                                    symbol.setDefinedLine(loc.line);
                                    symbol.setDefinedColumn(loc.column);
                                    _st.define(symbol);
                                }
                            }
                        }
                    }
                }

                CParser.InitDeclaratorListContext init_declarator_list = declaration_context.initDeclaratorList();
                List<CParser.InitDeclaratorContext> init_declarators = init_declarator_list != null ?
                        init_declarator_list.initDeclarator() : null;

                if (init_declarators != null) {
                    boolean isTypedef = false;
                    if (declaration_specifier != null) {
                        for (CParser.DeclarationSpecifierContext ds : declaration_specifier) {
                            if (ds.storageClassSpecifier() != null && ds.storageClassSpecifier().Typedef() != null) {
                                isTypedef = true;
                                break;
                            }
                        }
                    }
                    for (CParser.InitDeclaratorContext id : init_declarators) {
                        CParser.DeclaratorContext y = id != null ? id.declarator() : null;
                        Token idToken = getDeclarationToken(y);
                        if (idToken != null) {
                            String text = idToken.getText();
                            SourceLocation loc = getSourceLocation(idToken);
                            if (isTypedef) {
                                Symbol symbol = new Symbol();
                                symbol.setName(text);
                                HashSet<TypeClassification> classSet = new HashSet<>();
                                classSet.add(TypeClassification.TypeSpecifier_);
                                symbol.setClassification(classSet);
                                symbol.setDefinedFile(loc.file);
                                symbol.setDefinedLine(loc.line);
                                symbol.setDefinedColumn(loc.column);
                                _st.define(symbol);
                                if (debug) System.out.println("New symbol Declaration2 Declarator " + symbol);
                            } else {
                                Symbol symbol = new Symbol();
                                symbol.setName(text);
                                HashSet<TypeClassification> classSet = new HashSet<>();
                                classSet.add(TypeClassification.Variable_);
                                symbol.setClassification(classSet);
                                symbol.setDefinedFile(loc.file);
                                symbol.setDefinedLine(loc.line);
                                symbol.setDefinedColumn(loc.column);
                                _st.define(symbol);
                                if (debug) System.out.println("New symbol Declaration3 Declarator " + symbol);
                            }
                        }
                    }
                }
            }
            if (context instanceof CParser.FunctionDefinitionContext) {
                CParser.FunctionDefinitionContext fd = (CParser.FunctionDefinitionContext) context;
                CParser.DeclaratorContext de = fd.declarator();
                CParser.DirectDeclaratorContext dd = de != null ? de.directDeclarator() : null;
                if (dd != null && dd.Identifier() != null) {
                    Token idToken = dd.Identifier().getSymbol();
                    String text = idToken.getText();
                    SourceLocation loc = getSourceLocation(idToken);
                    Symbol symbol = new Symbol();
                    symbol.setName(text);
                    HashSet<TypeClassification> classSet = new HashSet<>();
                    classSet.add(TypeClassification.Function_);
                    symbol.setClassification(classSet);
                    symbol.setDefinedFile(loc.file);
                    symbol.setDefinedLine(loc.line);
                    symbol.setDefinedColumn(loc.column);
                    _st.define(symbol);
                    if (debug) System.out.println("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
            context = context.getParent();
        }
    }

    private String getDeclarationId(CParser.DeclaratorContext y) {
        Token token = getDeclarationToken(y);
        return token != null ? token.getText() : null;
    }

    private Token getDeclarationToken(CParser.DeclaratorContext y) {
        // Go down the tree and find a declarator with Identifier.
        if (y == null) return null;

        // Check if this declarator has a direct declarator with an identifier
        CParser.DirectDeclaratorContext directDeclarator = y.directDeclarator();
        if (directDeclarator != null) {
            CParser.DeclaratorContext more = directDeclarator.declarator();
            Token token = getDeclarationToken(more);
            if (token != null) return token;
            if (directDeclarator.Identifier() != null) {
                return directDeclarator.Identifier().getSymbol();
            }
        }

        return null;
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public boolean IsNullStructDeclarationListExtension() {
        if (noSemantics.contains("IsNullStructDeclarationListExtension")) return true;
        return true;
    }

    public void EnterScope() {
        if (debug) System.out.println("EnterScope");
        _st.pushBlockScope();
    }

    public void ExitScope() {
        if (debug) System.out.println("ExitScope");
        _st.popBlockScope();
    }

    public void LookupSymbol() {
        // Get the token that was just parsed (the Identifier)
        Token token = ((CommonTokenStream) this.getInputStream()).LT(-1);
        if (token == null) return;
        String text = token.getText();
        Symbol resolved = _st.resolve(text);
        if (outputAppliedOccurrences && resolved != null) {
            SourceLocation appliedLoc = getSourceLocation(token);
            System.err.println("Applied occurrence: " + text + " at " + appliedLoc.file + ":" + appliedLoc.line + ":" + appliedLoc.column +
                " -> Defined at " + resolved.getDefinedFile() + ":" + resolved.getDefinedLine() + ":" + resolved.getDefinedColumn());
        }
    }

    public void OutputSymbolTable() {
        if (outputSymbolTable) {
            System.err.println(_st.toString());
        }
    }

    private Symbol resolveWithOutput(Token token) {
        if (token == null) return null;
        String text = token.getText();
        Symbol resolved = _st.resolve(text);
        if (outputAppliedOccurrences && resolved != null) {
            SourceLocation appliedLoc = getSourceLocation(token);
            System.err.println("Applied occurrence: " + text + " at " + appliedLoc.file + ":" + appliedLoc.line + ":" + appliedLoc.column +
                " -> Defined at " + resolved.getDefinedFile() + ":" + resolved.getDefinedLine() + ":" + resolved.getDefinedColumn());
        }
        return resolved;
    }

    // Helper class to hold source location information
    private static class SourceLocation {
        String file;
        int line;
        int column;

        SourceLocation(String file, int line, int column) {
            this.file = file;
            this.line = line;
            this.column = column;
        }
    }

    // Compute source file, line, and column from a token, accounting for #line directives
    private SourceLocation getSourceLocation(Token token) {
        if (token == null) {
            return new SourceLocation("", 0, 0);
        }

        String fileName = "<unknown>";
        int line = token.getLine();
        int column = token.getCharPositionInLine();
        int lineAdjusted = line;

        CommonTokenStream ts = (CommonTokenStream) this.getInputStream();
        int ind = token.getTokenIndex();

        // Search back from token index to find last LineDirective
        for (int j = ind; j >= 0; j--) {
            Token t = ts.get(j);
            if (t == null) break;
            if (t.getType() == CLexer.LineDirective) {
                // Found it
                String txt = t.getText();
                String[] parts = txt.split("\\s+");
                if (parts.length >= 3) {
                    try {
                        int dirLine = Integer.parseInt(parts[1]);
                        int lineDirective = t.getLine();
                        int lineDiff = line - lineDirective;
                        lineAdjusted = lineDiff + dirLine - 1;
                        fileName = parts[2].trim();
                        // Remove quotes if present
                        if (fileName.startsWith("\"") && fileName.endsWith("\"")) {
                            fileName = fileName.substring(1, fileName.length() - 1);
                        }
                    } catch (NumberFormatException ex) {
                        // Ignore parse errors
                    }
                }
                break;
            }
        }

        return new SourceLocation(fileName, lineAdjusted, column);
    }

    public boolean IsInitDeclaratorList() {
        // Cannot be initDeclaratorList if the first thing is a type.
        // Types need to go to preceding declarationSpecifiers.
        if (noSemantics.contains("IsInitDeclaratorList")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsInitDeclaratorList " + lt1);
        Symbol resolved = resolveWithOutput(lt1);
        boolean result = false;
        if (resolved == null) {
            result = true;
        } else if (resolved.getClassification().contains(TypeClassification.TypeQualifier_) || resolved.getClassification().contains(TypeClassification.TypeSpecifier_)) {
            result = false;
        } else {
            result = true;
        }
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsSomethingOfTypeName() {
        if (noSemantics.contains("IsSizeofTypeName")) return true;
        CommonTokenStream ts = (CommonTokenStream) this.getInputStream();
        int lt1Type = ts.LT(1).getType();
        if (!(lt1Type == CLexer.Sizeof ||
              lt1Type == CLexer.Countof ||
              lt1Type == CLexer.Alignof ||
              lt1Type == CLexer.Maxof ||
              lt1Type == CLexer.Minof)) return false;
        if (ts.LT(2).getType() != CLexer.LeftParen) return false;
        if (IsTypeName(3)) return true;
        return false;
    }

    public boolean IsTypeName() { return IsTypeName(1); }
    public boolean IsTypeName(int k) {
        return IsSpecifierQualifierList(k);
    }

    public boolean IsSpecifierQualifierList() { return IsSpecifierQualifierList(1); }
    public boolean IsSpecifierQualifierList(int k) {
        if (IsGnuAttributeBeforeDeclarator(k)) return true;
        if (IsTypeSpecifierQualifier(k)) return true;
        return false;
    }

    public boolean IsCast() {
        boolean result = false;
        // Look for a cast.
        if (noSemantics.contains("IsCast")) return true;
        Token t1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        Token t2 = ((CommonTokenStream) this.getInputStream()).LT(2);
        if (this.debug) System.out.println("IsCast1 " + t1);
        if (this.debug) System.out.println("IsCast2 " + t2);
        if (t1.getType() != CLexer.LeftParen) {
            if (this.debug) System.out.print("IsCast " + result);
        } else if (t2.getType() != CLexer.Identifier) {
            // Assume typecast until otherwise.
            result = true;
        } else {
            // Check id.
            Symbol resolved = resolveWithOutput(t2);
            if (resolved == null) {
                // It's not in symbol table.
                result = false;
            } else if (resolved.getClassification().contains(TypeClassification.TypeSpecifier_)) {
                result = true;
            } else {
                result = false;
            }
        }
        if (this.debug) System.out.print("IsStatement " + result);
        return result;
    }
}
