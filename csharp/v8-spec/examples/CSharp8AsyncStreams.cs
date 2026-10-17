// CSharp8AsyncStreams.cs
// Tests: IAsyncEnumerable<T>, await foreach, async yield return, IAsyncDisposable

using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

public class CSharp8AsyncStreams
{
    // Async iterator: yield return inside an async method
    static async IAsyncEnumerable<int> CountAsync(
        int start, int count,
        [System.Runtime.CompilerServices.EnumeratorCancellation]
        CancellationToken cancellationToken = default)
    {
        for (int i = start; i < start + count; i++)
        {
            await Task.Delay(1, cancellationToken);
            yield return i;
        }
    }

    static async IAsyncEnumerable<string> ReadLinesAsync(string[] lines)
    {
        foreach (var line in lines)
        {
            await Task.Yield();
            yield return line;
        }
    }

    // IAsyncDisposable
    class AsyncResource : IAsyncDisposable
    {
        public string Name { get; }
        public AsyncResource(string name) { Name = name; }

        public async ValueTask DisposeAsync()
        {
            await Task.Delay(1);
            Console.WriteLine($"AsyncResource '{Name}' disposed.");
        }
    }

    static async Task Main()
    {
        // await foreach over async stream
        Console.Write("Counts: ");
        await foreach (var n in CountAsync(1, 5))
            Console.Write($"{n} ");
        Console.WriteLine();

        // await foreach over string lines
        await foreach (var line in ReadLinesAsync(new[] { "alpha", "beta", "gamma" }))
            Console.WriteLine(line);

        // await using (IAsyncDisposable)
        await using (var res = new AsyncResource("db-connection"))
        {
            Console.WriteLine($"Using '{res.Name}'");
        }

        // await using declaration
        await using var res2 = new AsyncResource("file-handle");
        Console.WriteLine($"Using '{res2.Name}'");
    }
}
