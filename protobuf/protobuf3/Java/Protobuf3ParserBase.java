import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.util.List;
import java.util.ArrayList;

public abstract class Protobuf3ParserBase extends Parser {
    private final TokenStream input;
    private boolean debug = false;
    private static final String prefix = "   ";
    private SymbolTable symbolTable = new SymbolTable();
    private TypeClassification default_type = TypeClassification.Message_;
    private List<String> importedFiles = new ArrayList<>();

    public Protobuf3ParserBase(TokenStream input) {
        super(input);
        this.input = input;
    }

    private TokenStream tokenStream() {
        return getTokenStream();
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

    public void DoRewind()
    {
        // Rewind for symbol table use.
        var parser = (Protobuf3Parser)this;
        var _ctx = parser.getContext();
        parser.reset();
        _ctx.removeLastChild();
        parser.setContext(_ctx);
    }

    public void DoImportStatement_() {
        String save = null;
        try {
            var ctx = this.getContext();
            assert ctx instanceof Protobuf3Parser.ImportStatementContext;
            var tctx = (Protobuf3Parser.ImportStatementContext) ctx;
            String importFileName = trimQuotes(tctx.strLit().getText());
            String currentFile = this.getTokenStream().getTokenSource().getSourceName();
            save = System.getProperty("user.dir").replace("\\", "/");
            Path current = Paths.get(currentFile).getParent();
            String fpDir = current.toAbsolutePath().toString();
            System.setProperty("user.dir", fpDir);
            String fp = Paths.get(importFileName).toAbsolutePath().toString();
            if (importedFiles.contains(fp)) return;
            importedFiles.add(fp);
            CharStream str = CharStreams.fromPath(Paths.get(fp));
            var lexer = new Protobuf3Lexer(str);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            var parser = new Protobuf3Parser(tokens);
            parser.twoPassParse();
        } catch (Exception e)
        {
        }
        finally {
            if (save != null) System.setProperty("user.dir", save);
        }
    }

    private String trimQuotes(String s) {
        return (s == null || s.isEmpty()) ? s : s.substring(1, s.length() - 1);
    }

    public boolean IsNotKeyword() {
        Token la = tokenStream().LT(1);
        switch (la.getType()) {
            case Protobuf3Parser.DOUBLE:
            case Protobuf3Parser.FLOAT:
            case Protobuf3Parser.INT32:
            case Protobuf3Parser.INT64:
            case Protobuf3Parser.UINT32:
            case Protobuf3Parser.UINT64:
            case Protobuf3Parser.SINT32:
            case Protobuf3Parser.SINT64:
            case Protobuf3Parser.FIXED32:
            case Protobuf3Parser.FIXED64:
            case Protobuf3Parser.SFIXED32:
            case Protobuf3Parser.SFIXED64:
            case Protobuf3Parser.BOOL:
            case Protobuf3Parser.STRING:
            case Protobuf3Parser.BYTES:
                return false;
            default:
                break;
        }
        return true;
    }
}
