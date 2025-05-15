
using System.Collections.Generic;

public class Symbol {
    public string Name { get; set; }
    public Symbol Type { get; set; }
    // Additional properties as needed
    public GoClassification Classification { get; set; }
    public Dictionary<string, Symbol> Members { get; } = new();
    public Symbol Parent { get; set; }

    public override string ToString()
    {
	    return Name
		+ (Type != null ? (" (with type " + Type.ToString() + ")") : "")
		+ (Parent != null ? (" of " + Parent.ToString()) : "");
    }

    public static Symbol Go_bool = new Symbol() { Name = "bool", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uint8 = new Symbol() { Name = "uint8", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uint16 = new Symbol() { Name = "uint16", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uint32 = new Symbol() { Name = "uint32", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uint64 = new Symbol() { Name = "uint64", Classification = GoClassification.GoBasicType };
    public static Symbol Go_int8 = new Symbol() { Name = "int8", Classification = GoClassification.GoBasicType };
    public static Symbol Go_int16 = new Symbol() { Name = "int16", Classification = GoClassification.GoBasicType };
    public static Symbol Go_int32 = new Symbol() { Name = "int32", Classification = GoClassification.GoBasicType };
    public static Symbol Go_int64 = new Symbol() { Name = "int64", Classification = GoClassification.GoBasicType };
    public static Symbol Go_float32 = new Symbol() { Name = "float32", Classification = GoClassification.GoBasicType };
    public static Symbol Go_float64 = new Symbol() { Name = "float64", Classification = GoClassification.GoBasicType };
    public static Symbol Go_complex64 = new Symbol() { Name = "complex64", Classification = GoClassification.GoBasicType };
    public static Symbol Go_complex128 = new Symbol() { Name = "complex128", Classification = GoClassification.GoBasicType };
    public static Symbol Go_byte = new Symbol() { Name = "byte", Classification = GoClassification.GoBasicType };
    public static Symbol Go_rune = new Symbol() { Name = "rune", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uint = new Symbol() { Name = "uint", Classification = GoClassification.GoBasicType };
    public static Symbol Go_int = new Symbol() { Name = "int", Classification = GoClassification.GoBasicType };
    public static Symbol Go_uintptr = new Symbol() { Name = "uintptr", Classification = GoClassification.GoBasicType };
}
