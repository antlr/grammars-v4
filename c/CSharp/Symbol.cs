using System.Collections.Generic;

public class Symbol {
    public string Name { get; set; }
    public HashSet<TypeClassification> Classification { get; set; }
    public Dictionary<string, Symbol> Members { get; } = new();
    public Symbol Parent { get; set; }
    public bool Predefined { get; set; } = false;
    public string DefinedFile { get; set; } = "";
    public int DefinedLine { get; set; } = 0;
    public int DefinedColumn { get; set; } = 0;

    public override string ToString()
    {
        var result = Name;
        var classification = Classification.ToString();
        result += " (with classification " + string.Join(", ", Classification) + ")";
        if (!string.IsNullOrEmpty(DefinedFile))
        {
            result += " defined at " + DefinedFile + ":" + DefinedLine + ":" + DefinedColumn;
        }
        return result;
    }
}
