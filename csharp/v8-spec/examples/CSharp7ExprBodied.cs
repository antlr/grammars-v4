// C# 7.0: expression-bodied constructors, destructors, and get/set accessors.
// Grammar rules exercised: accessor_body (=> expr), body (=> expr).
using System;

class Temperature
{
    private double _celsius;

    // C# 7.0: expression-bodied constructor
    public Temperature(double celsius) => _celsius = celsius;

    // C# 7.0: expression-bodied destructor
    ~Temperature() => _celsius = double.NaN;

    // C# 7.0: expression-bodied get and set accessors
    public double Celsius
    {
        get => _celsius;
        set => _celsius = value;
    }

    // C# 7.0: throw expression in accessor body
    public double CelsiusChecked
    {
        get => _celsius;
        set => _celsius = value >= -273.15
            ? value
            : throw new ArgumentOutOfRangeException(nameof(value), "Below absolute zero");
    }

    public double Fahrenheit => _celsius * 9.0 / 5.0 + 32.0;

    public override string ToString() => $"{_celsius:F1}°C / {Fahrenheit:F1}°F";
}

class CSharp7ExprBodied
{
    static void Main()
    {
        var t = new Temperature(100.0);
        Console.WriteLine(t);       // 100.0°C / 212.0°F
        t.Celsius = 0.0;
        Console.WriteLine(t);       // 0.0°C / 32.0°F
        t.CelsiusChecked = 37.0;
        Console.WriteLine(t);       // 37.0°C / 98.6°F
    }
}
