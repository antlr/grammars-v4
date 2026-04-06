// CSharp8MiscFeatures.cs
// Tests: stackalloc in nested expressions, $@"..." interpolated verbatim strings,
//        unmanaged constructed types

using System;

public class CSharp8MiscFeatures
{
    // ------------------------------------------------------------------
    // Stackalloc in nested expressions (C# 8 allows it outside unsafe)
    // ------------------------------------------------------------------
    static void StackallocNested()
    {
        // stackalloc used directly as a Span<T> initialiser in an expression
        Span<int> buf = stackalloc int[] { 10, 20, 30, 40, 50 };
        int sum = 0;
        foreach (int v in buf) sum += v;
        Console.WriteLine($"stackalloc sum = {sum}");

        // stackalloc in a conditional expression
        int n = 4;
        Span<byte> data = n <= 8 ? stackalloc byte[n] : new byte[n];
        data[0] = 0xFF;
        Console.WriteLine($"data[0] = 0x{data[0]:X2}");
    }

    // ------------------------------------------------------------------
    // Interpolated verbatim strings: both $@"..." and @$"..." are valid
    // ------------------------------------------------------------------
    static void InterpolatedVerbatim()
    {
        string folder = @"C:\Users\test";
        string file   = "report.csv";

        // $@ form (existed before C# 8 but @$ form is new in C# 8)
        string pathA = $@"{folder}\{file}";

        // @$ form (new token order, C# 8)
        string pathB = @$"{folder}\{file}";

        Console.WriteLine(pathA);
        Console.WriteLine(pathB);
        Console.WriteLine(pathA == pathB);

        // Multi-line interpolated verbatim
        string name = "World";
        string msg = @$"Hello,
{name}!
Goodbye,
{name}.";
        Console.WriteLine(msg);
    }

    // ------------------------------------------------------------------
    // Unmanaged constructed types
    // Generic struct is unmanaged when type arg is unmanaged
    // ------------------------------------------------------------------
    struct Pair<T> where T : unmanaged
    {
        public T First;
        public T Second;
        public Pair(T a, T b) { First = a; Second = b; }
        public override string ToString() => $"({First}, {Second})";
    }

    static unsafe void UnmanagedConstructed()
    {
        // C# 8: Pair<int> is unmanaged — can take its address, use stackalloc, etc.
        Pair<int>   pi = new Pair<int>(1, 2);
        Pair<double> pd = new Pair<double>(3.14, 2.71);

        Console.WriteLine(pi);
        Console.WriteLine(pd);

        // stackalloc of a constructed unmanaged type
        Span<Pair<int>> pairs = stackalloc Pair<int>[]
        {
            new Pair<int>(10, 20),
            new Pair<int>(30, 40),
        };
        foreach (var p in pairs)
            Console.WriteLine(p);
    }

    static void Main()
    {
        StackallocNested();
        InterpolatedVerbatim();

        unsafe { UnmanagedConstructed(); }
    }
}
