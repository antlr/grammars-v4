import org.antlr.v4.runtime.*;
import java.util.*;

public abstract class CParserBase extends Parser {
    private SymbolTable _st;
    private boolean debug = false;
    private boolean outputSymbolTable = false;
    private Set<String> noSemantics = new HashSet<>();

    // List of all semantic function names
    private static final String[] ALL_SEMANTIC_FUNCTIONS = {
        "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
        "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
        "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
        "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
        "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
        "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
        "IsNullStructDeclarationListExtension"
    };

    protected CParserBase(TokenStream input) {
        super(input);
        // Get options from system property
        String cmdLine = System.getProperty("sun.java.command");
        String[] args = cmdLine != null ? cmdLine.split("\\s+") : new String[0];
        noSemantics = parseNoSemantics(args);
        debug = hasArg(args, "--debug");
        outputSymbolTable = hasArg(args, "--output-symbol-table");
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

    public boolean IsAlignmentSpecifier() {
        if (noSemantics.contains("IsAlignmentSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsAlignmentSpecifier " + lt1);
        Symbol resolved = _st.resolve(text);
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

    public boolean IsAtomicTypeSpecifier() {
        if (noSemantics.contains("IsAtomicTypeSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsAtomicTypeSpecifier " + lt1);
        Symbol resolved = _st.resolve(text);
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
                || IsFunctionSpecifier()
                || IsAlignmentSpecifier();
        if (debug) System.out.println("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    public boolean IsTypeSpecifierQualifier() {
        if (noSemantics.contains("IsTypeSpecifierQualifier")) return true;
        if (debug) System.out.println("IsDeclarationSpecifier");
        boolean result = IsTypeSpecifier()
                || IsTypeQualifier()
                || IsAlignmentSpecifier();
        if (debug) System.out.println("IsDeclarationSpecifier " + result);
        return result;
    }

    public boolean IsDeclarationSpecifiers() {
        return IsDeclarationSpecifier();
    }

    public boolean IsEnumSpecifier() {
        if (noSemantics.contains("IsEnumSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
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
        Symbol resolved = _st.resolve(text);
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
        Symbol resolved = _st.resolve(text);
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

    public boolean IsStructOrUnionSpecifier() {
        if (noSemantics.contains("IsStructOrUnionSpecifier")) return true;
        Token token = ((CommonTokenStream) this.getInputStream()).LT(1);
        if (this.debug) System.out.print("IsStructOrUnionSpecifier " + token);
        boolean result = token.getType() == CLexer.Struct ||
                token.getType() == CLexer.Union;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypedefName() {
        if (noSemantics.contains("IsTypedefName")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypedefName " + lt1);
        Symbol resolved = _st.resolve(text);
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

    public boolean IsTypeofSpecifier() {
        if (noSemantics.contains("IsTypeofSpecifier")) return true;
        Token token = ((CommonTokenStream) this.getInputStream()).LT(1);
        if (this.debug) System.out.print("IsTypeofSpecifier " + token);
        boolean result = token.getType() == CLexer.Typeof ||
                token.getType() == CLexer.Typeof_unqual;
        if (this.debug) System.out.println(" " + result);
        return result;
    }

    public boolean IsTypeQualifier() {
        if (noSemantics.contains("IsTypeQualifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypeQualifier " + lt1);
        Symbol resolved = _st.resolve(text);
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

    public boolean IsTypeSpecifier() {
        if (noSemantics.contains("IsTypeSpecifier")) return true;
        Token lt1 = ((CommonTokenStream) this.getInputStream()).LT(1);
        String text = lt1.getText();
        if (this.debug) System.out.print("IsTypeSpecifier " + lt1);
        Symbol resolved = _st.resolve(text);
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
        result = IsAtomicTypeSpecifier() || IsStructOrUnionSpecifier() || IsEnumSpecifier()
                || IsTypedefName() || IsTypeofSpecifier();
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
                                var id = sous.Identifier().getText();
                                if (id != null)
                                {
                                    if (debug) System.out.println("New symbol Declaration1 Declarator " + id);
                                    Symbol symbol = new Symbol();
                                    symbol.setName(id);
                                    HashSet<TypeClassification> classSet = new HashSet<>();
                                    classSet.add(TypeClassification.TypeSpecifier_);
                                    symbol.setClassification(classSet);
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
                        String identifier = getDeclarationId(y);
                        if (identifier != null) {
                            String text = identifier;
                            if (isTypedef) {
                                Symbol symbol = new Symbol();
                                symbol.setName(text);
                                HashSet<TypeClassification> classSet = new HashSet<>();
                                classSet.add(TypeClassification.TypeSpecifier_);
                                symbol.setClassification(classSet);
                                _st.define(symbol);
                                if (debug) System.out.println("New symbol Declaration2 Declarator " + symbol);
                            } else {
                                Symbol symbol = new Symbol();
                                symbol.setName(text);
                                HashSet<TypeClassification> classSet = new HashSet<>();
                                classSet.add(TypeClassification.Variable_);
                                symbol.setClassification(classSet);
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
                    String text = dd.Identifier().getText();
                    Symbol symbol = new Symbol();
                    symbol.setName(text);
                    HashSet<TypeClassification> classSet = new HashSet<>();
                    classSet.add(TypeClassification.Function_);
                    symbol.setClassification(classSet);
                    _st.define(symbol);
                    if (debug) System.out.println("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
            context = context.getParent();
        }
    }

    private String getDeclarationId(CParser.DeclaratorContext y) {
        // Go down the tree and find a declarator with Identifier.
        if (y == null) return null;

        // Check if this declarator has a direct declarator with an identifier
        CParser.DirectDeclaratorContext directDeclarator = y.directDeclarator();
        if (directDeclarator != null) {
            CParser.DeclaratorContext more = directDeclarator.declarator();
            String xxx = getDeclarationId(more);
            if (xxx != null) return xxx;
            if (directDeclarator.Identifier() != null) {
                return directDeclarator.Identifier().getText();
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

    public void OutputSymbolTable() {
        if (outputSymbolTable) {
            System.err.println(_st.toString());
        }
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
            String text = t2.getText();
            Symbol resolved = _st.resolve(text);
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
