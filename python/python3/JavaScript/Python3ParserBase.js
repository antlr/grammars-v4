import antlr4 from 'antlr4';
import Python3Parser from './Python3Parser.js';

export default class Python3ParserBase extends antlr4.Parser {

    constructor(input) {
        super(input);
    }

    CannotBePlusMinus() {
        return true;
    }   

    CannotBeDotLpEq() {
        return true;
    }   
}
