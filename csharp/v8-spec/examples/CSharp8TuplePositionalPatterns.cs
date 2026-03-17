// CSharp8TuplePositionalPatterns.cs
// Tests: tuple patterns, positional patterns, deconstruction in switch

using System;

public class CSharp8TuplePositionalPatterns
{
    // Tuple pattern
    static string RockPaperScissors(string first, string second) =>
        (first, second) switch
        {
            ("rock",     "scissors") => "first wins",
            ("scissors", "paper")   => "first wins",
            ("paper",    "rock")    => "first wins",
            (var a,      var b) when a == b => "tie",
            _                               => "second wins",
        };

    // Positional pattern via Deconstruct
    readonly struct Size
    {
        public int Width  { get; }
        public int Height { get; }
        public Size(int w, int h) { Width = w; Height = h; }
        public void Deconstruct(out int w, out int h) { w = Width; h = Height; }
    }

    static string Classify(Size s) => s switch
    {
        (0, 0)           => "point",
        (var w, 0)       => $"horizontal line {w}",
        (0, var h)       => $"vertical line {h}",
        (var w, var h) when w == h => "square",
        _                => "rectangle",
    };

    // Nested tuple pattern
    static string Direction(int dx, int dy) => (Math.Sign(dx), Math.Sign(dy)) switch
    {
        ( 1,  0) => "right",
        (-1,  0) => "left",
        ( 0,  1) => "down",
        ( 0, -1) => "up",
        ( 1,  1) => "down-right",
        (-1, -1) => "up-left",
        _        => "diagonal",
    };

    static void Main()
    {
        Console.WriteLine(RockPaperScissors("rock", "scissors"));
        Console.WriteLine(Classify(new Size(5, 5)));
        Console.WriteLine(Direction(-3, 0));
    }
}
