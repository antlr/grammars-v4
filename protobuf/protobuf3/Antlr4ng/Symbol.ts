import { TypeClassification } from './TypeClassification.js';

export class Symbol {
    name: string;
    classification: TypeClassification;
    members: Map<string, Symbol> = new Map();
    parent?: Symbol;

    constructor(name: string, classification: TypeClassification) {
        this.name = name;
        this.classification = classification;
    }

    toString(): string {
        let result = this.name;
        const classificationStr = TypeClassification[this.classification];
        result += " (with classification " + classificationStr;
        result += ")";
        if (this.parent) result += " of " + this.parent.toString();
        return result;
    }
}
