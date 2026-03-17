// CSharp8StaticLocalFunctions.cs
// Tests: static local functions (cannot capture enclosing variables)

using System;
using System.Collections.Generic;

public class CSharp8StaticLocalFunctions
{
    static long Fibonacci(int n)
    {
        if (n < 0) throw new ArgumentOutOfRangeException(nameof(n));
        return Fib(n);

        static long Fib(int k) => k <= 1 ? k : Fib(k - 1) + Fib(k - 2);
    }

    static IEnumerable<int> Sieve(int limit)
    {
        bool[] composite = new bool[limit + 1];
        Mark(composite, limit);

        for (int i = 2; i <= limit; i++)
            if (!composite[i]) yield return i;

        static void Mark(bool[] sieve, int max)
        {
            for (int i = 2; (long)i * i <= max; i++)
                if (!sieve[i])
                    for (int j = i * i; j <= max; j += i)
                        sieve[j] = true;
        }
    }

    static double NewtonSqrt(double x)
    {
        if (x < 0) throw new ArgumentException("negative");
        double guess = x / 2.0;
        while (!GoodEnough(guess, x))
            guess = Improve(guess, x);
        return guess;

        static bool GoodEnough(double g, double v) => Math.Abs(g * g - v) < 1e-10;
        static double Improve(double g, double v) => (g + v / g) / 2.0;
    }

    static void Main()
    {
        Console.WriteLine($"Fib(10) = {Fibonacci(10)}");

        Console.Write("Primes up to 30: ");
        Console.WriteLine(string.Join(", ", Sieve(30)));

        Console.WriteLine($"sqrt(2) = {NewtonSqrt(2):F10}");
    }
}
