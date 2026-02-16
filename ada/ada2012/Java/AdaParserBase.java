import org.antlr.v4.runtime.*;
import java.util.*;

public abstract class AdaParserBase extends Parser {

    private SymbolTable _st;
    private Deque<Symbol> _expectedTypeStack = new ArrayDeque<>();
    private boolean debug = false;
    private boolean outputSymbolTable = false;
    private boolean outputAppliedOccurrences = false;
    private Set<String> noSemantics = new HashSet<>();

    private static final String[] ALL_SEMANTIC_FUNCTIONS = {
        "IsAggregate", "IsTypeName"
    };

    protected AdaParserBase(TokenStream input) {
        super(input);
        initOptions();
    }

    private void initOptions() {
        List<String> args = null;
        try {
            String prop = System.getProperty("sun.java.command");
            if (prop != null) args = Arrays.asList(prop.split("\\s+"));
        } catch (Exception e) { /* ignore */ }
        if (args == null) args = new ArrayList<>();
        noSemantics = parseNoSemantics(args);
        debug = args.stream().anyMatch(a -> a.toLowerCase().contains("--debug"));
        outputSymbolTable = args.stream().anyMatch(a -> a.toLowerCase().contains("--output-symbol-table"));
        outputAppliedOccurrences = args.stream().anyMatch(a -> a.toLowerCase().contains("--output-applied-occurrences"));
        _st = new SymbolTable();
    }

    private static Set<String> parseNoSemantics(List<String> args) {
        Set<String> result = new HashSet<>();
        if (args == null) return result;
        for (String a : args) {
            if (a.toLowerCase().startsWith("--no-semantics")) {
                int eqIndex = a.indexOf('=');
                if (eqIndex == -1) {
                    Collections.addAll(result, ALL_SEMANTIC_FUNCTIONS);
                } else {
                    String value = a.substring(eqIndex + 1);
                    for (String func : value.split(",")) {
                        result.add(func.trim());
                    }
                }
            }
        }
        return result;
    }

    public boolean IsAggregate() {
        if (noSemantics.contains("IsAggregate")) return true;
        CommonTokenStream stream = (CommonTokenStream) getTokenStream();
        Token lt1 = stream.LT(1);
        if (lt1.getType() != AdaLexer.LP) return false;
        int depth = 0;
        for (int i = 2; ; i++) {
            Token t = stream.LT(i);
            if (t == null || t.getType() == Token.EOF) break;
            if (t.getType() == AdaLexer.LP) {
                depth++;
            } else if (t.getType() == AdaLexer.RP) {
                if (depth == 0) break;
                depth--;
            } else if (depth == 0) {
                if (t.getType() == AdaLexer.COMMA) return true;
                if (t.getType() == AdaLexer.ARROW) return true;
                if (t.getType() == AdaLexer.WITH) return true;
                if (t.getType() == AdaLexer.NULL_) {
                    Token next = stream.LT(i + 1);
                    if (next != null && next.getType() == AdaLexer.RECORD) return true;
                }
            }
        }
        if (!_expectedTypeStack.isEmpty()) {
            Symbol expected = _expectedTypeStack.peek();
            if (expected != null && expected.isComposite()) return true;
        }
        return false;
    }

    public boolean IsTypeName() {
        if (noSemantics.contains("IsTypeName")) return true;
        CommonTokenStream stream = (CommonTokenStream) getTokenStream();
        Token lt1 = stream.LT(1);
        if (lt1.getType() != AdaLexer.IDENTIFIER_) return false;
        String firstName = lt1.getText();
        Symbol resolved = _st.resolve(firstName);
        return resolved != null && resolved.getClassification().contains(TypeClassification.TypeName_);
    }

    public void EnterDeclaration() {
        ParserRuleContext context = getContext();
        for (; context != null; context = (ParserRuleContext) context.getParent()) {
            if (context instanceof AdaParser.Full_type_declarationContext) {
                AdaParser.Full_type_declarationContext ftd = (AdaParser.Full_type_declarationContext) context;
                AdaParser.Defining_identifierContext defId = ftd.defining_identifier();
                if (defId != null) {
                    boolean isComposite = false;
                    AdaParser.Type_definitionContext typeDef = ftd.type_definition();
                    if (typeDef != null) {
                        isComposite = typeDef.record_type_definition() != null || typeDef.array_type_definition() != null;
                    }
                    defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart(), isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Subtype_declarationContext) {
                AdaParser.Subtype_declarationContext std = (AdaParser.Subtype_declarationContext) context;
                AdaParser.Defining_identifierContext defId = std.defining_identifier();
                if (defId != null) {
                    boolean isComposite = false;
                    AdaParser.Subtype_indicationContext si = std.subtype_indication();
                    if (si != null && si.subtype_mark() != null) {
                        Symbol baseSym = _st.resolve(si.subtype_mark().getText());
                        if (baseSym != null) isComposite = baseSym.isComposite();
                    }
                    defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart(), isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Object_declarationContext) {
                AdaParser.Object_declarationContext od = (AdaParser.Object_declarationContext) context;
                AdaParser.Defining_identifier_listContext defIdList = od.defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Number_declarationContext) {
                AdaParser.Number_declarationContext nd = (AdaParser.Number_declarationContext) context;
                AdaParser.Defining_identifier_listContext defIdList = nd.defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Subprogram_declarationContext) {
                defineSubprogramFromSpec(((AdaParser.Subprogram_declarationContext) context).subprogram_specification());
                return;
            }
            if (context instanceof AdaParser.Subprogram_bodyContext) {
                defineSubprogramFromSpec(((AdaParser.Subprogram_bodyContext) context).subprogram_specification());
                return;
            }
            if (context instanceof AdaParser.Package_declarationContext) {
                AdaParser.Package_specificationContext pkgSpec = ((AdaParser.Package_declarationContext) context).package_specification();
                if (pkgSpec != null) {
                    AdaParser.Defining_program_unit_nameContext dpun = pkgSpec.defining_program_unit_name();
                    if (dpun != null && dpun.defining_identifier() != null)
                        defineSymbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().getStart());
                }
                return;
            }
            if (context instanceof AdaParser.Package_bodyContext) {
                AdaParser.Defining_program_unit_nameContext dpun = ((AdaParser.Package_bodyContext) context).defining_program_unit_name();
                if (dpun != null && dpun.defining_identifier() != null)
                    defineSymbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().getStart());
                return;
            }
            if (context instanceof AdaParser.Exception_declarationContext) {
                AdaParser.Defining_identifier_listContext defIdList = ((AdaParser.Exception_declarationContext) context).defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ExceptionName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Task_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Task_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Single_task_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Single_task_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Protected_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Protected_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Single_protected_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Single_protected_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Entry_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Entry_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Component_declarationContext) {
                AdaParser.Defining_identifier_listContext defIdList = ((AdaParser.Component_declarationContext) context).defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ComponentName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Incomplete_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Incomplete_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Private_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Private_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Private_extension_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Private_extension_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart(), true);
                return;
            }
            if (context instanceof AdaParser.Generic_instantiationContext) {
                AdaParser.Generic_instantiationContext gi = (AdaParser.Generic_instantiationContext) context;
                List<AdaParser.Defining_program_unit_nameContext> dpuns = gi.defining_program_unit_name();
                if (dpuns != null && !dpuns.isEmpty()) {
                    AdaParser.Defining_identifierContext defId = dpuns.get(0).defining_identifier();
                    if (defId != null) {
                        TypeClassification tc = TypeClassification.PackageName_;
                        if (gi.PROCEDURE() != null || gi.FUNCTION() != null) tc = TypeClassification.SubprogramName_;
                        defineSymbol(defId.getText(), tc, defId.getStart());
                    }
                }
                List<AdaParser.Defining_designatorContext> dds = gi.defining_designator();
                if (dds != null && !dds.isEmpty()) {
                    AdaParser.Defining_program_unit_nameContext dpun = dds.get(0).defining_program_unit_name();
                    if (dpun != null && dpun.defining_identifier() != null)
                        defineSymbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().getStart());
                }
                return;
            }
            if (context instanceof AdaParser.Object_renaming_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Object_renaming_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Exception_renaming_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Exception_renaming_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ExceptionName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Package_renaming_declarationContext) {
                AdaParser.Defining_program_unit_nameContext dpun = ((AdaParser.Package_renaming_declarationContext) context).defining_program_unit_name();
                if (dpun != null && dpun.defining_identifier() != null)
                    defineSymbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().getStart());
                return;
            }
            if (context instanceof AdaParser.Formal_complete_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Formal_complete_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Formal_incomplete_type_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Formal_incomplete_type_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Formal_object_declarationContext) {
                AdaParser.Defining_identifier_listContext defIdList = ((AdaParser.Formal_object_declarationContext) context).defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Formal_package_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Formal_package_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Parameter_specificationContext) {
                AdaParser.Defining_identifier_listContext defIdList = ((AdaParser.Parameter_specificationContext) context).defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Loop_parameter_specificationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Loop_parameter_specificationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Iterator_specificationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Iterator_specificationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Enumeration_literal_specificationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Enumeration_literal_specificationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.EnumerationLiteral_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Choice_parameter_specificationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Choice_parameter_specificationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Entry_index_specificationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Entry_index_specificationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Extended_return_object_declarationContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Extended_return_object_declarationContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Entry_bodyContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Entry_bodyContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Task_bodyContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Task_bodyContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Protected_bodyContext) {
                AdaParser.Defining_identifierContext defId = ((AdaParser.Protected_bodyContext) context).defining_identifier();
                if (defId != null) defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                return;
            }
            if (context instanceof AdaParser.Discriminant_specificationContext) {
                AdaParser.Defining_identifier_listContext defIdList = ((AdaParser.Discriminant_specificationContext) context).defining_identifier_list();
                if (defIdList != null) {
                    for (AdaParser.Defining_identifierContext defId : defIdList.defining_identifier()) {
                        defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.getStart());
                    }
                }
                return;
            }
        }
    }

    private void defineSubprogramFromSpec(AdaParser.Subprogram_specificationContext spec) {
        if (spec == null) return;
        AdaParser.Procedure_specificationContext procSpec = spec.procedure_specification();
        if (procSpec != null) {
            AdaParser.Defining_program_unit_nameContext dpun = procSpec.defining_program_unit_name();
            if (dpun != null && dpun.defining_identifier() != null)
                defineSymbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().getStart());
            return;
        }
        AdaParser.Function_specificationContext funcSpec = spec.function_specification();
        if (funcSpec != null) {
            AdaParser.Defining_designatorContext dd = funcSpec.defining_designator();
            if (dd != null) {
                AdaParser.Defining_program_unit_nameContext dpun = dd.defining_program_unit_name();
                if (dpun != null && dpun.defining_identifier() != null)
                    defineSymbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().getStart());
            }
        }
    }

    private void defineSymbol(String name, TypeClassification classification, Token token) {
        defineSymbol(name, classification, token, false);
    }

    private void defineSymbol(String name, TypeClassification classification, Token token, boolean isComposite) {
        Symbol symbol = new Symbol();
        symbol.setName(name);
        symbol.setClassification(new HashSet<>(Arrays.asList(classification)));
        symbol.setComposite(isComposite);
        symbol.setDefinedFile(token != null && token.getTokenSource() != null ? token.getTokenSource().getSourceName() : "");
        symbol.setDefinedLine(token != null ? token.getLine() : 0);
        symbol.setDefinedColumn(token != null ? token.getCharPositionInLine() : 0);
        _st.define(symbol);
    }

    public void EnterScope() {
        _st.pushBlockScope();
    }

    public void ExitScope() {
        _st.popBlockScope();
    }

    public void PushExpectedType() {
        _expectedTypeStack.push(null);
    }

    public void PopExpectedType() {
        if (!_expectedTypeStack.isEmpty()) {
            _expectedTypeStack.pop();
        }
    }

    public void OutputSymbolTable() {
        if (outputSymbolTable) {
            System.err.println(_st.toString());
        }
    }

    protected void ParsePragmas() {
        BufferedTokenStream stream = (BufferedTokenStream) getTokenStream();
        stream.fill();
        List<Token> allTokens = stream.getTokens();
        final int PRAGMA_CHANNEL = 2;
        List<Token> currentPragma = null;
        List<List<Token>> pragmas = new ArrayList<>();
        for (Token token : allTokens) {
            if (token.getChannel() != PRAGMA_CHANNEL)
                continue;
            if (token.getType() == AdaLexer.PRAGMA) {
                currentPragma = new ArrayList<>();
                currentPragma.add(token);
            } else if (currentPragma != null) {
                currentPragma.add(token);
                if (token.getType() == AdaLexer.SEMI) {
                    pragmas.add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (List<Token> pragmaTokens : pragmas) {
            List<Token> defaultChannelTokens = new ArrayList<>();
            for (Token t : pragmaTokens) {
                CommonToken ct = new CommonToken(t);
                ct.setChannel(Token.DEFAULT_CHANNEL);
                defaultChannelTokens.add(ct);
            }
            CommonToken eof = new CommonToken(Token.EOF);
            eof.setChannel(Token.DEFAULT_CHANNEL);
            defaultChannelTokens.add(eof);
            ListTokenSource tokenSource = new ListTokenSource(defaultChannelTokens);
            CommonTokenStream tokenStream = new CommonTokenStream(tokenSource);
            AdaParser parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            for (ANTLRErrorListener listener : this.getErrorListeners()) {
                parser.addErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
