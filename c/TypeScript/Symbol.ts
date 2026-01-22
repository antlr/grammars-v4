import { TypeClassification } from "./TypeClassification.js";

export class Symbol {
    public name: string = "";
    public classification: Set<TypeClassification> = new Set();
    public members: Map<string, Symbol> = new Map();
    public parent: Symbol | null = null;

    public toString(): string {
        let result = this.name;
        const classificationStr = Array.from(this.classification)
            .map(c => TypeClassification[c])
            .join(", ");
        result += " (with classification " + classificationStr + ")";
        return result;
    }
}
