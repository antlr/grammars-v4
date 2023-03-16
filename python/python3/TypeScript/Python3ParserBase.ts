import { Parser } from "antlr4ts/Parser";

export default abstract class Python3ParserBase extends Parser {

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
