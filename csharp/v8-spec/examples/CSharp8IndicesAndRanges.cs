// CSharp8IndicesAndRanges.cs
// Tests: Index (^), Range (..), Index/Range types, slicing arrays and spans

using System;

public class CSharp8IndicesAndRanges
{
    static void Indices()
    {
        string[] words = { "the", "quick", "brown", "fox", "jumps" };

        // From-end index
        string last    = words[^1]; // "jumps"
        string second  = words[^2]; // "fox"

        Console.WriteLine($"^1={last}  ^2={second}");

        // Index variables
        Index first = 0;
        Index fromEnd = ^1;
        Console.WriteLine($"words[{first}]={words[first]}  words[{fromEnd}]={words[fromEnd]}");
    }

    static void Ranges()
    {
        int[] nums = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        int[] all    = nums[..];      // full copy
        int[] first3 = nums[..3];     // 0,1,2
        int[] last3  = nums[^3..];    // 7,8,9
        int[] middle = nums[2..7];    // 2,3,4,5,6

        Console.WriteLine($"all:    [{string.Join(",", all)}]");
        Console.WriteLine($"first3: [{string.Join(",", first3)}]");
        Console.WriteLine($"last3:  [{string.Join(",", last3)}]");
        Console.WriteLine($"middle: [{string.Join(",", middle)}]");
    }

    static void RangeVariables()
    {
        string text = "Hello, World!";

        Range r = 7..12;             // "World"
        string sub = text[r];
        Console.WriteLine(sub);

        // Span slicing
        ReadOnlySpan<char> span = text.AsSpan();
        ReadOnlySpan<char> slice = span[7..12];
        Console.WriteLine(slice.ToString());
    }

    static void StringSlicing()
    {
        string s = "abcdefghij";
        Console.WriteLine(s[2..5]);   // "cde"
        Console.WriteLine(s[^4..]);   // "ghij"
        Console.WriteLine(s[..^4]);   // "abcdef"
    }

    static void Main()
    {
        Indices();
        Ranges();
        RangeVariables();
        StringSlicing();
    }
}
