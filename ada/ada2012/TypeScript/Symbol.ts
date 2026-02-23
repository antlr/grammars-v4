import { TypeClassification } from './TypeClassification';

export class Symbol {
    name: string;
    classification: Set<TypeClassification>;
    members: Map<string, Symbol> = new Map();
    parent: Symbol | null = null;
    predefined: boolean = false;
    isComposite: boolean = false;
    definedFile: string = "";
    definedLine: number = 0;
    definedColumn: number = 0;

    constructor(name: string, classification: Set<TypeClassification>) {
        this.name = name;
        this.classification = classification;
    }

    toString(): string {
        let result = this.name;
        result += " (with classification " + Array.from(this.classification).join(", ") + ")";
        if (this.isComposite) {
            result += " [composite]";
        }
        if (this.definedFile !== "") {
            result += " defined at " + this.definedFile + ":" + this.definedLine + ":" + this.definedColumn;
        }
        return result;
    }
}
