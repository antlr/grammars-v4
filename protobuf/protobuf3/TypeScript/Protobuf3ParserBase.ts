import { Parser, TokenStream, CommonTokenStream, Token, CharStream, FileStream } from 'antlr4';
import { CharStreams } from 'antlr4';
import Protobuf3Parser  from './Protobuf3Parser.js';
import { DoMessageNameDefContext, MessageDefContext, DoEnumNameDefContext, EnumDefContext, DoServiceNameDefContext, ServiceDefContext, ImportStatementContext } from './Protobuf3Parser.js';
import Protobuf3Lexer from './Protobuf3Lexer.js';
import { Symbol } from './Symbol.js';
import { SymbolTable } from './SymbolTable.js';
import { TypeClassification } from './TypeClassification.js';
import * as fs from 'fs';
import * as path from 'path';
import { readFileSync } from 'fs';

export default abstract class Protobuf3ParserBase extends Parser {
    private debug = false;
    private readonly prefix = '   ';
    protected symbolTable = new SymbolTable();
    protected default_type = TypeClassification.Message_;
    private static imported_files: Set<string> = new Set<string>();

    constructor(input: TokenStream) {
        super(input);
    }

    DoMessageNameDef_(): void {
        const ctx = this._ctx;
        const tctx = ctx as DoMessageNameDefContext;
        const identifier = (tctx.parentCtx as MessageDefContext).messageName().ident();
        const name = identifier.getText();
        const sym = new Symbol(name, TypeClassification.Message_);
        this.symbolTable.define(sym);
        if (this.debug) console.log(this.prefix + "defined Message " + sym.toString());
    }

    DoEnumNameDef_(): void {
        const ctx = this._ctx;
        const tctx = ctx as DoEnumNameDefContext;
        const identifier = (tctx.parentCtx as EnumDefContext).enumName().ident();
        const name = identifier.getText();
        const sym = new Symbol(name, TypeClassification.Enum_);
        this.symbolTable.define(sym);
        if (this.debug) console.log(this.prefix + "defined Enum " + sym.toString());
    }

    DoServiceNameDef_(): void {
        const ctx = this._ctx;
        const tctx = ctx as DoServiceNameDefContext;
        const identifier = (tctx.parentCtx as ServiceDefContext).serviceName().ident();
        const name = identifier.getText();
        const sym = new Symbol(name, TypeClassification.Service_);
        this.symbolTable.define(sym);
        if (this.debug) console.log(this.prefix + "defined Service " + sym.toString());
    }

    DoEnterBlock_(): void {
        var ctx = null;
        const vvv = this._ctx?.parentCtx?.parentCtx;
        const vvvt = vvv?.constructor.name;
        const vvvsdc = this._ctx?.parentCtx;
        const vvvsdct = vvvsdc?.constructor.name;
        if (vvvt === "MessageDefContext") {
            const v = vvv as MessageDefContext;
            ctx = v.messageName().getText();
        } else if (vvvt === "EnumDefContext") {
            const v = vvv as EnumDefContext;
            ctx = v.enumName().getText();
        } else if (vvvsdct === "ServiceDefContext") {
            const v = vvvsdc as ServiceDefContext;
            ctx = v.serviceName().getText();
        }
        if (!ctx) throw new Error();
        const newScope = this.symbolTable.resolve(ctx);
        if (this.debug) console.log(this.prefix + "EnterBlock " + newScope?.toString());
        this.symbolTable.enterScope(newScope);
    }

    DoExitBlock_(): void {
        if (this.debug) console.log(this.prefix + "ExitBlock " + this.symbolTable.currentScope()?.toString());
        this.symbolTable.exitScope();
    }

    IsMessageType_(): boolean {
        let i = 1;
        let scope: Symbol | null = null;
        let symbol: Symbol | null = null;
        let first = true;

        while (true) {
            const la = this.getTokenStream().LT(i);
            const id = la.text;
            if (this.debug) process.stderr.write(id);

            if (la.type === Protobuf3Parser.DOT) {
                if (first) first = false;
                if (this.getTokenStream().LT(i + 1).type !== Protobuf3Parser.IDENTIFIER) break;
            } else if (la.type === Protobuf3Parser.IDENTIFIER) {
                symbol = this.symbolTable.resolve(id, scope ?? undefined);
                if (symbol) {
                    scope = symbol;
                } else break;
                if (this.getTokenStream().LT(i + 1).type !== Protobuf3Parser.DOT) break;
            } else break;

            i++;
        }

        if (symbol?.classification === TypeClassification.Message_) return true;
        if (symbol) return false;
        return this.default_type === TypeClassification.Message_;
    }

    IsEnumType_(): boolean {
        let i = 1;
        let scope: Symbol | null = null;
        let symbol: Symbol | null = null;
        let first = true;

        while (true) {
            const la = this.getTokenStream().LT(i);
            const id = la.text;
            if (this.debug) process.stderr.write(id);

            if (la.type === Protobuf3Parser.DOT) {
                if (first) first = false;
                if (this.getTokenStream().LT(i + 1).type !== Protobuf3Parser.IDENTIFIER) break;
            } else if (la.type === Protobuf3Parser.IDENTIFIER) {
                symbol = this.symbolTable.resolve(id, scope ?? undefined);
                if (symbol) {
                    scope = symbol;
                } else break;
                if (this.getTokenStream().LT(i + 1).type !== Protobuf3Parser.DOT) break;
            } else break;

            i++;
        }

        if (symbol?.classification === TypeClassification.Enum_) return true;
        if (symbol) return false;
        return this.default_type === TypeClassification.Enum_;
    }

    DoRewind(): void {
        const parser = this as unknown as Protobuf3Parser;
        const _ctx = parser._ctx;
        parser.proto();
        parser.reset();
	_ctx.children.shift();
//        _ctx.removeLastChild();  // Warning: assumes removeLastChild() exists
        parser._ctx = _ctx;
    }

    DoImportStatement_(): void {
        try {
            const ctx = this._ctx;
            const tctx = ctx as ImportStatementContext;
            const import_file_name = this.trimQuotes(tctx.strLit().getText());
	    const cts = this.getTokenStream() as CommonTokenStream;
	    const l = cts.tokenSource as Protobuf3Lexer;
	    const input = l._input as CharStream;
	    const filestream = input as FileStream;
	    const current_file = filestream.fileName;
            if (this.debug) {
                console.error("current file = " + current_file);
                console.error("imported file = " + import_file_name);
            }
            const current = path.dirname(current_file);
            const fp_dir = path.resolve(current);
            const fp = path.resolve(import_file_name);
            if (Protobuf3ParserBase.imported_files.has(fp)) return;

            Protobuf3ParserBase.imported_files.add(fp);
            var enc = 'utf8';
            var buffer = readFileSync(fp, { encoding: enc as BufferEncoding });
            var str = CharStreams.fromString(buffer);
            const lexer = new Protobuf3Lexer(str);
            const tokens = new CommonTokenStream(lexer);
            const parser = new Protobuf3Parser(tokens);
            parser.twoPassParse(); // You must define this
        } catch (e) {
            // fail silently as in C#
        }
    }

    protected trimQuotes(s: string): string {
        return s.length < 2 ? s : s.substring(1, s.length - 1);
    }

    IsNotKeyword(): boolean {
        const la = this.getTokenStream().LT(1);
        switch (la.type) {
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
            case Protobuf3Parser.BOOL_LIT:
                return false;
        }
        return true;
    }
}
