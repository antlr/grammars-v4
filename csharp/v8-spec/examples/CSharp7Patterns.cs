// C# 7.0: pattern matching — is-type patterns and switch with type patterns and when guards.
// Grammar rules exercised: isType in relational_expression, switch_label with case_guard.
using System;

abstract class Shape { }
class Circle    : Shape { public double Radius;          }
class Rectangle : Shape { public double Width, Height;   }
class Triangle  : Shape { public double Base, HeightVal; }

class CSharp7Patterns
{
    static double Area(Shape s)
    {
        switch (s)
        {
            case Circle c when c.Radius > 0:
                return Math.PI * c.Radius * c.Radius;
            case Rectangle r when r.Width > 0 && r.Height > 0:
                return r.Width * r.Height;
            case Triangle t when t.Base > 0 && t.HeightVal > 0:
                return 0.5 * t.Base * t.HeightVal;
            default:
                return 0;
        }
    }

    static string Classify(object obj)
    {
        // C# 7.0: is-type pattern with capture variable
        if (obj is int n && n > 0)
            return $"positive int {n}";
        if (obj is string s && s.Length > 0)
            return $"non-empty string \"{s}\"";
        if (obj is double d)
            return $"double {d:F2}";
        return "other";
    }

    static void Main()
    {
        Console.WriteLine(Area(new Circle    { Radius = 3 })                    .ToString("F4"));  // 28.2743
        Console.WriteLine(Area(new Rectangle { Width = 4,  Height = 5 })        .ToString("F4"));  // 20.0000
        Console.WriteLine(Area(new Triangle  { Base = 6,   HeightVal = 4 })     .ToString("F4"));  // 12.0000

        Console.WriteLine(Classify(42));       // positive int 42
        Console.WriteLine(Classify("hello"));  // non-empty string "hello"
        Console.WriteLine(Classify(3.14));     // double 3.14
        Console.WriteLine(Classify(-5));       // other
    }
}
