import Symbol from './Symbol.js';
import TypeClassification from './TypeClassification.js';

export default class SymbolTable {
    constructor() {
        this.scopeStack = [];
        this.blockCounter = 0;

        var globalScope = new Symbol();
        globalScope.name = "global";
        globalScope.classification = new Set([TypeClassification.Global_]);
        this.scopeStack.push(globalScope);

        // Predefined scalar types
        this._defineType("integer", false);
        this._defineType("natural", false);
        this._defineType("positive", false);
        this._defineType("float", false);
        this._defineType("long_float", false);
        this._defineType("character", false);
        this._defineType("wide_character", false);
        this._defineType("wide_wide_character", false);
        this._defineType("boolean", false);
        this._defineType("duration", false);

        // Predefined composite types
        this._defineType("string", true);
        this._defineType("wide_string", true);
        this._defineType("wide_wide_string", true);

        // Predefined exceptions
        this._definePredefined("constraint_error", TypeClassification.ExceptionName_);
        this._definePredefined("program_error", TypeClassification.ExceptionName_);
        this._definePredefined("storage_error", TypeClassification.ExceptionName_);
        this._definePredefined("tasking_error", TypeClassification.ExceptionName_);
        this._definePredefined("numeric_error", TypeClassification.ExceptionName_);

        // Predefined enumeration literals
        this._definePredefined("true", TypeClassification.EnumerationLiteral_);
        this._definePredefined("false", TypeClassification.EnumerationLiteral_);
    }

    _defineType(name, isComposite) {
        var sym = new Symbol();
        sym.name = name;
        sym.classification = new Set([TypeClassification.TypeName_]);
        sym.predefined = true;
        sym.isComposite = isComposite;
        this.define(sym);
    }

    _definePredefined(name, classification) {
        var sym = new Symbol();
        sym.name = name;
        sym.classification = new Set([classification]);
        sym.predefined = true;
        this.define(sym);
    }

    currentScope() {
        if (this.scopeStack.length === 0) return null;
        return this.scopeStack[this.scopeStack.length - 1];
    }

    define(symbol) {
        var currentScope = this.currentScope();
        if (currentScope === null) return false;
        // Case-insensitive: normalize name to lowercase
        symbol.name = symbol.name.toLowerCase();
        if (currentScope.members.has(symbol.name)) {
            return false;
        }
        symbol.parent = currentScope;
        currentScope.members.set(symbol.name, symbol);
        return true;
    }

    resolve(name) {
        // Case-insensitive: normalize to lowercase
        name = name.toLowerCase();
        for (var i = this.scopeStack.length - 1; i >= 0; i--) {
            var scope = this.scopeStack[i];
            if (scope.members.has(name)) {
                return scope.members.get(name);
            }
        }
        return null;
    }

    pushBlockScope() {
        var blockScope = new Symbol();
        blockScope.name = "block" + (++this.blockCounter);
        blockScope.classification = new Set([TypeClassification.Block_]);
        blockScope.predefined = true;
        var current = this.currentScope();
        if (current !== null && blockScope !== current) {
            this.scopeStack.push(blockScope);
        }
        return blockScope;
    }

    popBlockScope() {
        if (this.scopeStack.length <= 1) {
            throw new Error("SymbolTable: scope stack underflow");
        }
        this.scopeStack.pop();
    }

    toString() {
        var result = "";
        if (this.scopeStack.length > 0) {
            result = this._toStringHelper(this.scopeStack[0], 0);
        }
        return result;
    }

    _toStringHelper(scope, depth) {
        var result = "";
        var indent = "  ".repeat(depth);
        for (var [key, sym] of scope.members) {
            if (!sym.predefined) {
                result += indent + sym.toString() + "\n";
            }
            if (sym.classification.has(TypeClassification.Block_) ||
                sym.classification.has(TypeClassification.SubprogramName_) ||
                sym.classification.has(TypeClassification.PackageName_)) {
                result += this._toStringHelper(sym, depth + 1);
            }
        }
        return result;
    }
}
