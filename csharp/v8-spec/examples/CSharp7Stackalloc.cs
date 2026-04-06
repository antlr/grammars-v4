// C# 7.2: stackalloc as a general expression (not just in variable initializers).
// Grammar rule exercised: primary_expression_start stackalloc_initializer.
using System;

class CSharp7Stackalloc
{
    static void Main()
    {
        // C# 7.2: stackalloc assigned to Span<T> — no unsafe block required.
        Span<int> squares = stackalloc int[8];
        for (int i = 0; i < squares.Length; i++)
            squares[i] = (i + 1) * (i + 1);

        Console.WriteLine(string.Join(", ", squares.ToArray()));  // 1, 4, 9, 16, 25, 36, 49, 64

        // C# 7.2: stackalloc with initializer syntax in expression context.
        Span<byte> flags = stackalloc byte[] { 0xFF, 0x0F, 0xF0, 0x00 };
        foreach (byte b in flags)
            Console.Write($"0x{b:X2} ");
        Console.WriteLine();  // 0xFF 0x0F 0xF0 0x00
    }
}
