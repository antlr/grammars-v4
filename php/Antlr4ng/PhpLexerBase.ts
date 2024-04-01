import {CommonToken, Lexer, Token, CharStream} from "antlr4ng";
import { PhpParser } from "./PhpParser.js";
import { PhpLexer } from "./PhpLexer.js";

export default abstract class PhpLexerBase extends Lexer {
    private AspTags: boolean;
    protected _scriptTag: boolean;
    protected _styleTag: boolean;
    private _heredocIdentifier: string | undefined;
    private _prevTokenType: number;
    private _htmlNameText: string | undefined;
    private _phpScript: boolean;
    private _insideString: boolean;

    protected static MIN_CHAR_VALUE = 0x0000;
    protected static MAX_CHAR_VALUE = 0x10FFFF;

    constructor(input: CharStream) {
        super(input);
        this.AspTags = true;
        this._scriptTag = false;
        this._styleTag = false;
        this._heredocIdentifier = undefined;
        this._prevTokenType = 0;
        this._htmlNameText = undefined;
        this._phpScript = false;
        this._insideString = false;
    }

    nextToken() {
        let token = super.nextToken()

        if (token.type === PhpParser.PHPEnd || token.type === PhpLexer.PHPEndSingleLineComment) {
            if (this.mode === PhpLexer.SingleLineCommentMode) {
                // SingleLineCommentMode for such allowed syntax:
                // // <?php echo "Hello world"; // comment ?>
                this.popMode();
            }
            this.popMode();

            if (token.text === "</script>") {
                this._phpScript = false;
                token.type = PhpLexer.HtmlScriptClose;
            } else {
                // Add semicolon to the end of statement if it is absent.
                // For example: <?php echo "Hello world" ?>
                if (this._prevTokenType === PhpLexer.SemiColon || this._prevTokenType === PhpLexer.Colon || this._prevTokenType === PhpLexer.OpenCurlyBracket || this._prevTokenType === PhpLexer.CloseCurlyBracket) {
                    token.channel = 4; // Damn tool does not generate constants for declared channels.
                } else {
                    token.type = PhpLexer.SemiColon;
                }
            }
        }

        else if (token.type === PhpLexer.HtmlName) {
            this._htmlNameText = token.text
        }

        else if (token.type === PhpLexer.HtmlDoubleQuoteString) {
            if (token.text === "php" && this._htmlNameText === "language") {
                this._phpScript = true;
            }
        }

        else if (this.mode === PhpLexer.HereDoc) {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            if (token.type === PhpLexer.StartHereDoc || token.type === PhpLexer.StartNowDoc) {
                this._heredocIdentifier = token.text.slice(3).trim().replace(/\'/, '').replace(/\'/, '');
            }

            else if (token.type === PhpLexer.HereDocText) {
                if (this.CheckHeredocEnd(token.text)) {
                    this.popMode()
                    const heredocIdentifier = this.GetHeredocIdentifier(token.text)
                    if (token.text.trim().endsWith(';')) {
                        token.text = `${heredocIdentifier};\n`;
                        token.type = PhpLexer.SemiColon;
                    } else {
                        token = super.nextToken()
                        token.text = `${heredocIdentifier}\n;`;
                    }
                }
            }
        }

        else if (this.mode === PhpLexer.PHP) {
            if (!(this.channel === PhpLexer.HIDDEN)) {
                this._prevTokenType = token.type;
            }
        }

        return token;
    }

    GetHeredocIdentifier(text: string): string {
        return text.trim().replace(/\;$/, "");
    }

    CheckHeredocEnd(text: string): boolean {
        return this.GetHeredocIdentifier(text) === this._heredocIdentifier;
    }

    IsNewLineOrStart(pos: number): boolean {
        return this.inputStream.LA(pos) <= 0 || this.inputStream.LA(pos) == '\r'.charCodeAt(0) ||
            this.inputStream.LA(pos) == '\n'.charCodeAt(0)
    }

    PushModeOnHtmlClose() {
        this.popMode();
        if (this._scriptTag) {
            if (!this._phpScript) {
                this.pushMode(PhpLexer.SCRIPT);
            } else {
                this.pushMode(PhpLexer.PHP);
            }
            this._scriptTag = false;
        } else if (this._styleTag) {
            this.pushMode(PhpLexer.STYLE);
            this._styleTag = false;
        }
    }

    HasAspTags(): boolean {
        return this.AspTags;
    }

    HasPhpScriptTag(): boolean {
        return this._phpScript;
    }

    PopModeOnCurlyBracketClose() {
        if (this._insideString) {
            this._insideString = false;
	    this.channel = 4; // Tool does not generate a constant for declared channels.
            this.popMode();
        }
    }

    ShouldPushHereDocMode(pos: number): boolean {
        return this.inputStream.LA(pos) === '\r'.charCodeAt(0) || this.inputStream.LA(pos) === '\n'.charCodeAt(0);
    }

    IsCurlyDollar(pos: number): boolean {
        return this.inputStream.LA(pos) === '$'.charCodeAt(0);
    }

    SetInsideString() {
        this._insideString = true
    }
}
