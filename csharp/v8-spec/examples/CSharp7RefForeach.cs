// C# 7.3: ref iteration variable in foreach over Span<T>.
// Grammar rule exercised: simple_embedded_statement foreach with (REF READONLY?).
using System;

class CSharp7RefForeach
{
    static void Main()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };

        // C# 7.3: ref iteration variable — modifies the array elements in-place.
        foreach (ref int n in numbers.AsSpan())
            n *= n;

        Console.WriteLine(string.Join(", ", numbers));  // 1, 4, 9, 16, 25

        // C# 7.3: readonly ref — read without copying (no mutation).
        int sum = 0;
        foreach (ref readonly int n in numbers.AsSpan())
            sum += n;

        Console.WriteLine(sum);  // 55
    }
}
