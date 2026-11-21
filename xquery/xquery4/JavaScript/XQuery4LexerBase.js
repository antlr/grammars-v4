import antlr4 from 'antlr4';

export default class XQuery4LexerBase extends antlr4.Lexer {

    constructor(input) {
        super(input);
    }

    // Returns true when the next character is an NCName start character.
    // Used in: '<' { IsNCNameStart() }? -> pushMode(IN_ELEMENT_TAG)
    IsNCNameStart() {
        const c = this._input.LA(1);
        return this._isNCNameStartChar(c);
    }

    _isNCNameStartChar(c) {
        return (c >= 0x41 && c <= 0x5A)   // A-Z
            || c === 0x5F                   // _
            || (c >= 0x61 && c <= 0x7A)    // a-z
            || (c >= 0x00C0 && c <= 0x00D6)
            || (c >= 0x00D8 && c <= 0x00F6)
            || (c >= 0x00F8 && c <= 0x02FF)
            || (c >= 0x0370 && c <= 0x037D)
            || (c >= 0x037F && c <= 0x1FFF)
            || (c >= 0x200C && c <= 0x200D)
            || (c >= 0x2070 && c <= 0x218F)
            || (c >= 0x2C00 && c <= 0x2FEF)
            || (c >= 0x3001 && c <= 0xD7FF)
            || (c >= 0xF900 && c <= 0xFDCF)
            || (c >= 0xFDF0 && c <= 0xFFFD)
            || (c >= 0x10000 && c <= 0xEFFFF);
    }

    // Pops the mode stack when inside an embedded expression in element/attribute content.
    // Called by: '}' { PopModeIfNeeded(); }
    // When in root DEFAULT_MODE (stack empty), this is a no-op.
    PopModeIfNeeded() {
        if (this._modeStack.length > 0) {
            this.popMode();
        }
    }
}
