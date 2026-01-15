using System.Collections.Generic;

public class Symbol {
    public string Name { get; set; }
    public HashSet<TypeClassification> Classification { get; set; }
    public Dictionary<string, Symbol> Members { get; } = new();
    public Symbol Parent { get; set; }

    public override string ToString()
    {
        var result = Name;
        var classification = Classification.ToString();
        result += " (with classification " + string.Join(", ", Classification) + ")";
        //if (Parent != null) result += " of " + Parent.ToString();
        return result;
    }
}
