// CSharp8SwitchExpressions.cs
// Tests: switch expressions, property patterns, when guards

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

    static void Main()
    {
        Console.WriteLine(DescribeSeason(Season.Summer));
        Console.WriteLine(Quadrant(new Point(1, 2)));
        Console.WriteLine(Discount("hello world!"));
    }
}
