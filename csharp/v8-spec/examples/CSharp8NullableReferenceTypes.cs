// CSharp8NullableReferenceTypes.cs
// Tests: nullable reference types, #nullable enable/disable, null-forgiving operator

#nullable enable

using System;
using System.Collections.Generic;

public class CSharp8NullableReferenceTypes
{
    // Non-nullable: compiler warns if null is assigned
    public string Name { get; set; }

    // Nullable reference type
    public string? MiddleName { get; set; }

    public CSharp8NullableReferenceTypes(string name)
    {
        Name = name;
    }

    static int GetLength(string? s)
    {
        if (s is null) return 0;
        return s.Length; // no warning: null checked above
    }

    static string FirstOrEmpty(IList<string?> list)
    {
        if (list.Count == 0) return string.Empty;
        return list[0]!; // null-forgiving: caller guarantees non-null at [0]
    }

    // Late-initialized field suppressed with null-forgiving
    static string _config = null!;

    static void Initialize(string config)
    {
        _config = config;
    }

#nullable disable

    // This region opts out: nullable warnings suppressed
    static string LegacyMethod(string s)
    {
        return s.ToUpper(); // no warning even if s could be null
    }

#nullable restore

    static void Main()
    {
        Initialize("production");

        var obj = new CSharp8NullableReferenceTypes("Alice");
        obj.MiddleName = null; // allowed: nullable

        Console.WriteLine(GetLength(null));
        Console.WriteLine(GetLength("hello"));
        Console.WriteLine(FirstOrEmpty(new List<string?> { "first", null }));
        Console.WriteLine(LegacyMethod("test"));
        Console.WriteLine(_config);
    }
}
