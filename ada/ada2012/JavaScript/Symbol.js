export default class Symbol {
    constructor() {
        this.name = "";
        this.classification = new Set();
        this.members = new Map();
        this.parent = null;
        this.predefined = false;
        this.isComposite = false;
        this.definedFile = "";
        this.definedLine = 0;
        this.definedColumn = 0;
    }
    toString() {
        let result = this.name;
        result += " (with classification " + Array.from(this.classification).join(", ") + ")";
        if (this.isComposite) result += " [composite]";
        if (this.definedFile) result += " defined at " + this.definedFile + ":" + this.definedLine + ":" + this.definedColumn;
        return result;
    }
}
