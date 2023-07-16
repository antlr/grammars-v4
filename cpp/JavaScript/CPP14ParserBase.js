import antlr4 from 'antlr4';
import CPP14Parser from './CPP14Parser.js';

export default class CPP14ParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
    }

    IsPureSpecifierAllowed() {
        try {
            var x = this._ctx; // memberDeclarator
            var c = x.getChild(0).getChild(0);
            var c2 = c.getChild(0);
            var p = c2.getChild(1);
            return p.constructor === CPP14Parser.ParametersAndQualifiersContext;
        } catch (e) {
        }
        return false;
    }
}
