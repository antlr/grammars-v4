// CSharp8DisposableRefStructs.cs
// Tests: disposable ref structs (C# 8)
//
// Ref structs cannot implement interfaces, but C# 8 allows them to be used in
// 'using' statements via pattern-based disposal: if the ref struct has a public
// void Dispose() method the compiler treats it as disposable.
//
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/ref-struct

using System;

public class CSharp8DisposableRefStructs
{
    // A ref struct with a Dispose() method.
    // Cannot implement IDisposable (ref structs cannot implement interfaces),
    // but the Dispose() pattern is recognised by the compiler for 'using'.
    ref struct StackBuffer
    {
        private bool _disposed;
        public readonly int Capacity;

        public StackBuffer(int capacity)
        {
            Capacity  = capacity;
            _disposed = false;
            Console.WriteLine($"StackBuffer({capacity}) created");
        }

        public void CheckAlive()
        {
            if (_disposed) throw new InvalidOperationException("StackBuffer is disposed");
        }

        public void Dispose()
        {
            if (!_disposed)
            {
                _disposed = true;
                Console.WriteLine($"StackBuffer({Capacity}) disposed");
            }
        }
    }

    // readonly ref struct — all members are implicitly readonly.
    // Still supports pattern-based Dispose().
    readonly ref struct ReadOnlySlice
    {
        private readonly string _source;
        private readonly int    _start;
        private readonly int    _length;

        public ReadOnlySlice(string source, int start, int length)
        {
            _source = source;
            _start  = start;
            _length = length;
            Console.WriteLine($"ReadOnlySlice [{start}..{start + length}) created");
        }

        public int  Length      => _length;
        public char this[int i] => _source[_start + i];

        public override string ToString() => _source.Substring(_start, _length);

        public void Dispose() =>
            Console.WriteLine($"ReadOnlySlice \"{ToString()}\" disposed");
    }

    static void UseBlock()
    {
        // using block — Dispose() called at closing brace
        using (var buf = new StackBuffer(64))
        {
            buf.CheckAlive();
            Console.WriteLine($"  capacity = {buf.Capacity}");
        }
    }

    static void UseDeclaration()
    {
        // using declaration (also C# 8) — Dispose() called at end of scope
        using var buf = new StackBuffer(128);
        buf.CheckAlive();
        Console.WriteLine($"  capacity = {buf.Capacity}");
    }

    static void UseReadOnly()
    {
        string text = "Hello, C# 8!";
        using var slice = new ReadOnlySlice(text, 7, 4);  // "C# 8"
        Console.WriteLine($"  slice    = \"{slice.ToString()}\"");
        Console.WriteLine($"  length   = {slice.Length}");
        Console.WriteLine($"  slice[0] = '{slice[0]}'");
    }

    static void Main()
    {
        Console.WriteLine("-- using block --");
        UseBlock();

        Console.WriteLine("-- using declaration --");
        UseDeclaration();

        Console.WriteLine("-- readonly ref struct --");
        UseReadOnly();
    }
}
