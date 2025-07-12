using System.Collections.Generic;

public class Symbol {
    public string Name { get; set; }
    public Symbol Type { get; set; }
    public TypeClassification Classification { get; set; }
    public Dictionary<string, Symbol> Members { get; } = new();
    public Symbol Parent { get; set; }

    public override string ToString()
    {
        var result = Name;
        var classification = Classification.ToString();
        result += " (with classification " + classification;
        if (Type != null) result += ", type " + Type.ToString();
        result += ")";
        if (Parent != null) result += " of " + Parent.ToString();
        return result;
    }
}
