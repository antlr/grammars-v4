import { TypeClassification } from "./TypeClassification.js";

export class Symbol {
    public name: string = "";
    public classification: Set<TypeClassification> = new Set();
    public members: Map<string, Symbol> = new Map();
    public parent: Symbol | null = null;
    public predefined: boolean = false;
    public definedFile: string = "";
    public definedLine: number = 0;
    public definedColumn: number = 0;

    public toString(): string {
        let result = this.name;
        const classificationStr = Array.from(this.classification)
            .map(c => TypeClassification[c])
            .join(", ");
        result += " (with classification " + classificationStr + ")";
        if (this.definedFile && this.definedFile.length > 0) {
            result += " defined at " + this.definedFile + ":" + this.definedLine + ":" + this.definedColumn;
        }
        return result;
    }
}
