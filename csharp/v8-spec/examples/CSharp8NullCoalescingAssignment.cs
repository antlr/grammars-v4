// CSharp8NullCoalescingAssignment.cs
// Tests: ??= null-coalescing assignment operator

using System;
using System.Collections.Generic;

public class CSharp8NullCoalescingAssignment
{
    static string? _cache;

    static string GetOrCreate(string value)
    {
        // Assigns only if _cache is null
        _cache ??= value;
        return _cache;
    }

    static void DictionaryLazyInit()
    {
        var map = new Dictionary<string, List<int>?>();

        void Add(string key, int val)
        {
            // ??= on a local variable holding the looked-up list
            map.TryGetValue(key, out List<int>? list);
            list ??= new List<int>();
            map[key] = list;
            list.Add(val);
        }

        Add("a", 1);
        Add("a", 2);
        Add("b", 3);

        foreach (var kv in map)
            Console.WriteLine($"{kv.Key}: [{string.Join(",", kv.Value!)}]");
    }

    static void ChainedAssignment()
    {
        string? a = null;
        string? b = null;
        string  c = "found";

        a ??= b ??= c;
        Console.WriteLine($"a={a} b={b}");
    }

    static void NullableValueType()
    {
        int? x = null;
        x ??= 42;
        Console.WriteLine($"x={x}");

        x ??= 99;  // x is already non-null; no assignment
        Console.WriteLine($"x={x}");
    }

    static void Main()
    {
        Console.WriteLine(GetOrCreate("first"));
        Console.WriteLine(GetOrCreate("second")); // still "first"

        DictionaryLazyInit();
        ChainedAssignment();
        NullableValueType();
    }
}
