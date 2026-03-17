// CSharp8SwitchExpressions.cs
// Tests: switch expressions, property patterns, when guards
//
// Property patterns: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns#property-pattern
// Switch expressions: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/switch-expression

using System;

public class CSharp8SwitchExpressions
{
    enum Season { Spring, Summer, Autumn, Winter }

    static string DescribeSeason(Season s) => s switch
    {
        Season.Spring => "warm and wet",
        Season.Summer => "hot and dry",
        Season.Autumn => "cool and windy",
        Season.Winter => "cold and snowy",
        _             => "unknown",
    };

    class Point { public double X; public double Y; public Point(double x, double y) { X = x; Y = y; } }

    static string Quadrant(Point p) => p switch
    {
        Point q when q.X > 0 && q.Y > 0 => "I",
        Point q when q.X < 0 && q.Y > 0 => "II",
        Point q when q.X < 0 && q.Y < 0 => "III",
        Point q when q.X > 0 && q.Y < 0 => "IV",
        Point q when q.X == 0 || q.Y == 0 => "on axis",
        _ => "origin",
    };

    static decimal Discount(object item) => item switch
    {
        string s when s.Length > 10 => 0.15m,
        string _                    => 0.05m,
        int n    when n > 100       => 0.20m,
        int _                       => 0.10m,
        null                        => 0m,
        _                           => 0m,
    };

    // -----------------------------------------------------------------------
    // Property patterns (C# 8) — match on the values of properties/fields
    // -----------------------------------------------------------------------

    class Color
    {
        public int R; public int G; public int B;
        public Color(int r, int g, int b) { R = r; G = g; B = b; }
    }

    static string NameColor(Color c) => c switch
    {
        { R: 255, G:   0, B:   0 } => "red",
        { R:   0, G: 255, B:   0 } => "green",
        { R:   0, G:   0, B: 255 } => "blue",
        { R:   0, G:   0, B:   0 } => "black",
        { R: 255, G: 255, B: 255 } => "white",
        { R: 255, G: 255, B:   0 } => "yellow",
        _                          => "other",
    };

    // Property pattern in an 'is' expression
    static bool IsBlack(Color c) => c is { R: 0, G: 0, B: 0 };

    // Nested property pattern
    class Rectangle
    {
        public Point TopLeft;
        public Point BottomRight;
        public Rectangle(Point tl, Point br) { TopLeft = tl; BottomRight = br; }
    }

    static string ClassifyRect(Rectangle r) => r switch
    {
        { TopLeft: { X: 0, Y: 0 } }    => "anchored at origin",
        { BottomRight: { X: 0, Y: 0 } } => "bottom-right at origin",
        _                               => "general rectangle",
    };

    static void Main()
    {
        Console.WriteLine(DescribeSeason(Season.Summer));
        Console.WriteLine(Quadrant(new Point(1, 2)));
        Console.WriteLine(Discount("hello world!"));

        Console.WriteLine(NameColor(new Color(255, 0, 0)));
        Console.WriteLine(NameColor(new Color(0, 0, 0)));
        Console.WriteLine(NameColor(new Color(255, 255, 0)));
        Console.WriteLine(IsBlack(new Color(0, 0, 0)));
        Console.WriteLine(IsBlack(new Color(1, 0, 0)));

        var rect = new Rectangle(new Point(0, 0), new Point(10, 5));
        Console.WriteLine(ClassifyRect(rect));
    }
}
