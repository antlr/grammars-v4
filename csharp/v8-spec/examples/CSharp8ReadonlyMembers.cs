// CSharp8ReadonlyMembers.cs
// Tests: readonly instance members in structs

using System;

public struct Vector2D
{
    public double X;
    public double Y;

    public Vector2D(double x, double y) { X = x; Y = y; }

    // readonly property — does not mutate state
    public readonly double Length => Math.Sqrt(X * X + Y * Y);

    // readonly method
    public readonly Vector2D Normalized()
    {
        double len = Length;
        return new Vector2D(X / len, Y / len);
    }

    // readonly override
    public readonly override string ToString() => $"({X}, {Y})";

    // Non-readonly method — mutates
    public void Scale(double factor)
    {
        X *= factor;
        Y *= factor;
    }
}

public readonly struct ImmutablePoint
{
    public double X { get; }
    public double Y { get; }

    public ImmutablePoint(double x, double y) { X = x; Y = y; }

    public double DistanceTo(ImmutablePoint other)
    {
        double dx = X - other.X;
        double dy = Y - other.Y;
        return Math.Sqrt(dx * dx + dy * dy);
    }

    public override string ToString() => $"({X}, {Y})";
}

public class CSharp8ReadonlyMembers
{
    static void Main()
    {
        var v = new Vector2D(3, 4);
        Console.WriteLine($"Length: {v.Length}");
        Console.WriteLine($"Normalized: {v.Normalized()}");

        v.Scale(2);
        Console.WriteLine($"Scaled: {v}");

        var a = new ImmutablePoint(0, 0);
        var b = new ImmutablePoint(3, 4);
        Console.WriteLine($"Distance: {a.DistanceTo(b)}");
    }
}
