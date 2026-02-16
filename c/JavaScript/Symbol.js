import { TypeClassification, TypeClassificationNames } from "./TypeClassification.js";

export class Symbol {
    constructor() {
        this.name = "";
        this.classification = new Set();
        this.members = new Map();
        this.parent = null;
        this.predefined = false;
        this.definedFile = "";
        this.definedLine = 0;
        this.definedColumn = 0;
    }

    toString() {
        let result = this.name;
        const classificationStr = Array.from(this.classification)
            .map(c => TypeClassificationNames[c] || String(c))
            .join(", ");
        result += " (with classification " + classificationStr + ")";
        if (this.definedFile && this.definedFile.length > 0) {
            result += " defined at " + this.definedFile + ":" + this.definedLine + ":" + this.definedColumn;
        }
        return result;
    }
}

export default Symbol;
