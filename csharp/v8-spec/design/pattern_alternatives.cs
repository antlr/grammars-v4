// Compile:
//   cd csharp/v8-spec/design
//   dotnet run --project pattern_alternatives.csproj
//
// Demonstrates all six alternatives of the ANTLR4 rule:
//
//   pattern
//       : {IsDeclarationPatternAhead()}? declaration_pattern
//       | {IsConstantPatternAhead()}?    constant_pattern
//       | var_pattern
//       | positional_pattern
//       | property_pattern
//       | discard_pattern
//       ;
//
// NOTE on disambiguation:
//   IsDeclarationPatternAhead() performs a speculative parse of type_() over
//   the current token stream using a BailErrorStrategy, then checks whether
//   the next token is a Simple_Identifier or '_'.  If the speculative parse
//   succeeds and a designation follows, the pattern is a declaration_pattern.
//   IsConstantPatternAhead() is the complement (!IsDeclarationPatternAhead()).
//
//   var_pattern, positional_pattern, property_pattern, and discard_pattern
//   are syntactically unambiguous and need no predicates.

using System;

// ── Supporting declarations ───────────────────────────────────────────────────

class Shape
{
    public string Kind;
    public double Area;
    public Shape(string kind, double area) { Kind = kind; Area = area; }
}

class Circle : Shape
{
    public double Radius;
    public Circle(double r) : base("circle", Math.PI * r * r) { Radius = r; }
}

class Rectangle : Shape
{
    public double Width, Height;
    public Rectangle(double w, double h) : base("rect", w * h) { Width = w; Height = h; }
}

// ── Main program ──────────────────────────────────────────────────────────────

class Program
{
    static string Describe(object o)
    {
        // ── alternative: declaration_pattern ─────────────────────────────────
        // IsDeclarationPatternAhead(): speculative type_() parse succeeds and
        // is followed by a Simple_Identifier designation.
        if (o is double d)
            // pattern → declaration_pattern
            //   type_ → value_type (keyword double)
            //   simple_designation → single_variable_designation: d
            return string.Format("double:{0}", d);

        if (o is Circle c)
            // pattern → declaration_pattern
            //   type_ → reference_type → non_nullable_reference_type → class_type (Circle)
            //   simple_designation → single_variable_designation: c
            return string.Format("circle radius:{0}", c.Radius);

        if (o is Shape s)
            // pattern → declaration_pattern
            //   type_ → reference_type → non_nullable_reference_type → class_type (Shape)
            //   simple_designation → single_variable_designation: s
            return string.Format("shape kind:{0}", s.Kind);

        return "other";
    }

    static string ClassifyInt(int n)
    {
        switch (n)
        {
            // ── alternative: constant_pattern ────────────────────────────────
            // IsConstantPatternAhead(): speculative type_() either fails to
            // parse or is NOT followed by a designation — falls back to
            // constant_expression.
            case 0:
                // pattern → constant_pattern
                //   constant_expression → literal: 0
                return "zero";
            case 1:
                // pattern → constant_pattern
                //   constant_expression → literal: 1
                return "one";
            default:
                return "other";
        }
    }

    static string ClassifyObject(object o)
    {
        switch (o)
        {
            // ── constant_pattern: null literal ────────────────────────────────
            case null:
                // pattern → constant_pattern
                //   constant_expression → null_literal
                return "null";

            // ── constant_pattern: boolean literal ─────────────────────────────
            case true:
                // pattern → constant_pattern
                //   constant_expression → boolean_literal: true
                return "true";

            case false:
                // pattern → constant_pattern
                //   constant_expression → boolean_literal: false
                return "false";

            // ── constant_pattern: string literal ──────────────────────────────
            case "hello":
                // pattern → constant_pattern
                //   constant_expression → string_literal: "hello"
                return "greeting";

            // ── alternative: declaration_pattern ──────────────────────────────
            case int i:
                // pattern → declaration_pattern
                //   type_ → value_type (keyword int)
                //   simple_designation → single_variable_designation: i
                return string.Format("int:{0}", i);

            case double d:
                // pattern → declaration_pattern
                //   type_ → value_type (keyword double)
                //   simple_designation → single_variable_designation: d
                return string.Format("double:{0}", d);

            case Circle c:
                // pattern → declaration_pattern
                //   type_ → reference_type → non_nullable_reference_type → class_type (Circle)
                //   simple_designation → single_variable_designation: c
                return string.Format("circle r:{0}", c.Radius);

            case Shape sh:
                // pattern → declaration_pattern
                //   type_ → reference_type → non_nullable_reference_type → class_type (Shape)
                //   simple_designation → single_variable_designation: sh
                return string.Format("shape kind:{0}", sh.Kind);

            // ── alternative: discard_pattern ──────────────────────────────────
            // '_' token, standalone, matched as discard_pattern (not designation).
            case string _:
                // pattern → discard_pattern
                //   '_' (discard)
                // NOTE: in switch, 'case string _:' uses a discard_pattern;
                //       the type_ keyword string selects the case arm but the
                //       binding is discarded.
                return "some string";

            default:
                return "unknown";
        }
    }

    static string VarAndProperty(object o)
    {
        // ── alternative: var_pattern ──────────────────────────────────────────
        // 'var' keyword is unambiguous; no predicate needed.
        if (o is var x)
            // pattern → var_pattern
            //   'var' designation → simple_designation → single_variable_designation: x
            return string.Format("var:{0}", x);

        return "unreachable";
    }

    static string PropertyMatch(Shape s)
    {
        // ── alternative: property_pattern ────────────────────────────────────
        // type_? property_subpattern simple_designation?
        // property_subpattern: '{' subpatterns '}'
        // No predicate needed: '{' after optional type_ is unambiguous.
        if (s is Shape { Kind: "circle" })
            // pattern → property_pattern
            //   type_? → class_type (Shape)
            //   property_subpattern → { subpattern: identifier 'Kind' ':' constant_pattern "circle" }
            return "matched circle shape";

        if (s is { Area: var a })
            // pattern → property_pattern
            //   type_? → (absent)
            //   property_subpattern → { subpattern: identifier 'Area' ':' var_pattern a }
            return string.Format("area:{0:F4}", a);

        return "other shape";
    }

    static string PositionalMatch(object o)
    {
        // ── alternative: positional_pattern ──────────────────────────────────
        // type_? '(' subpatterns? ')' property_subpattern? simple_designation?
        // No predicate needed: '(' (with optional preceding type_) is unambiguous.
        if (o is (int, int))
            // pattern → positional_pattern
            //   type_? → (absent)
            //   '(' subpatterns ')' : two declaration_patterns (int _)
            return "int pair";

        return "other";
    }

    static void Main()
    {
        // declaration_pattern
        Console.WriteLine(Describe(3.14));
        Console.WriteLine(Describe(new Circle(2.0)));
        Console.WriteLine(Describe(new Rectangle(3.0, 4.0)));

        // constant_pattern (switch on int)
        Console.WriteLine(ClassifyInt(0));
        Console.WriteLine(ClassifyInt(1));
        Console.WriteLine(ClassifyInt(99));

        // constant_pattern (switch on object) + declaration_pattern + discard_pattern
        Console.WriteLine(ClassifyObject(null));
        Console.WriteLine(ClassifyObject(true));
        Console.WriteLine(ClassifyObject(false));
        Console.WriteLine(ClassifyObject("hello"));
        Console.WriteLine(ClassifyObject("world"));
        Console.WriteLine(ClassifyObject(42));
        Console.WriteLine(ClassifyObject(2.71));
        Console.WriteLine(ClassifyObject(new Circle(1.0)));
        Console.WriteLine(ClassifyObject(new Rectangle(2.0, 3.0)));

        // var_pattern
        Console.WriteLine(VarAndProperty(99));

        // property_pattern
        Console.WriteLine(PropertyMatch(new Circle(5.0)));
        Console.WriteLine(PropertyMatch(new Rectangle(2.0, 3.0)));

        // positional_pattern
        object pair = (1, 2);
        Console.WriteLine(PositionalMatch(pair));
    }
}
