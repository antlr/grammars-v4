import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.Token;

public abstract class Protobuf3ParserBase extends Parser {
    private final TokenStream input;
    private boolean debug = false;
    private static final String prefix = "   ";
    private SymbolTable symbolTable = new SymbolTable();
    private TypeClassification default_type = TypeClassification.Message_;

    public Protobuf3ParserBase(TokenStream input) {
        super(input);
        this.input = input;
    }

    public void DoMessageNameDef_() {
        var ctx = this.getContext();
        var tctx = (Protobuf3Parser.DoMessageNameDefContext) ctx;
        var identifier = ((Protobuf3Parser.MessageDefContext) tctx.getParent()).messageName().ident();
        var name = identifier.getText();
        var current = symbolTable.currentScope();
        var sym = new Symbol(name, current, TypeClassification.Message_);
        symbolTable.define(sym);
        if (debug) System.out.println(prefix + "defined Message " + sym);
    }

    public void DoEnumNameDef_() {
        var ctx = this.getContext();
        var tctx = (Protobuf3Parser.DoEnumNameDefContext) ctx;
        var identifier = ((Protobuf3Parser.EnumDefContext) tctx.getParent()).enumName().ident();
        var name = identifier.getText();
        var current = symbolTable.currentScope();
        var sym = new Symbol(name, current, TypeClassification.Enum_);
        symbolTable.define(sym);
        if (debug) System.out.println(prefix + "defined Enum " + sym);
    }

    public void DoServiceNameDef_() {
        var ctx = this.getContext();
        var tctx = (Protobuf3Parser.DoServiceNameDefContext) ctx;
        var identifier = ((Protobuf3Parser.ServiceDefContext) tctx.getParent()).serviceName().ident();
        var name = identifier.getText();
        var current = symbolTable.currentScope();
        var sym = new Symbol(name, current, TypeClassification.Service_);
        symbolTable.define(sym);
        if (debug) System.out.println(prefix + "defined Service " + sym);
    }

    public void DoEnterBlock_() {
        var newScope = new Symbol("<block>", null, TypeClassification.Block_);
        if (debug) System.out.println(prefix + "EnterBlock " + newScope);
        symbolTable.enterScope(newScope);
    }

    public void DoExitBlock_() {
        if (debug) System.out.println(prefix + "ExitBlock " + symbolTable.currentScope());
        symbolTable.exitScope();
    }

    public boolean IsMessageType_() {
        Token la = tokenStream().LT(1);
        String id = la.getText();
        var symbol = symbolTable.resolve(id);
        if (symbol != null) {
            if (symbol.getClassification() == TypeClassification.Message_) {
                if (debug) System.out.println("IsMessageType_ found " + id + " true");
                return true;
            } else {
                if (debug) System.out.println("IsMessageType_ found " + id + " false");
                return false;
            }
        }
        if (debug) System.out.println("IsMessageType_ not found " + id + " " + (this.default_type == TypeClassification.Message_));
        return this.default_type == TypeClassification.Message_;
    }

    public boolean IsEnumType_() {
        Token la = tokenStream().LT(1);
        String id = la.getText();
        var symbol = symbolTable.resolve(id);
        if (symbol != null) {
            if (symbol.getClassification() == TypeClassification.Enum_) {
                if (debug) System.out.println("IsEnumType found " + id + " true");
                return true;
            } else {
                if (debug) System.out.println("IsEnumType found " + id + " false");
                return false;
            }
        }
        if (debug) System.out.println("IsEnumType not found " + id + " " + (this.default_type == TypeClassification.Enum_));
        return this.default_type == TypeClassification.Enum_;
    }

    private TokenStream tokenStream() {
        return getTokenStream();
    }
}
