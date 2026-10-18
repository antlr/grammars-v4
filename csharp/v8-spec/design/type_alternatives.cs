// Compile (from cmd.exe or PowerShell, not MSYS2 bash):
//   "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\csc.exe" /unsafe type_alternatives.cs
//
// Demonstrates all four alternatives of the ANTLR4 rule:
//
//   type_
//       : {IsTypeParameterName()}? type_parameter
//       | {IsValueTypeName()}?     value_type
//       | {IsReferenceTypeName()}? reference_type
//       | pointer_type
//       ;
//
// NOTE on symbol-table predicates:
//   IsTypeParameterName() fires for names registered when the enclosing
//   type_parameter_list is parsed (e.g. T in Box<T>).
//   IsValueTypeName() fires for simple-type keywords (int, bool, …) and
//   for identifiers registered as struct/enum — but only if the symbol table
//   was populated with that declaration first.
//   IsDelegateTypeName() / IsInterfaceTypeName() similarly require prior
//   registration.
//   IsClassTypeName() / IsReferenceTypeName() are the open-world defaults:
//   an unknown identifier is assumed to be a reference_type → class_type.
//
//   In this test harness top-level declarations (struct Point, enum Color,
//   interface IShape, delegate Handler) are NOT pre-registered, so all
//   user-defined type names that are not type-parameters fall through to
//   the class_type default.

using System;

// ── Supporting declarations ───────────────────────────────────────────────────

delegate void Handler();          // declared but not registered in symbol table here

interface IShape                  // same
{
    double Area();                // return: type_ → value_type (keyword double)
}

enum Color { Red, Green, Blue }   // same

struct Point { public float X, Y; }  // field: type_ → value_type (keyword float)

class Animal                      // same
{
    public string Name = "";      // field: type_ → reference_type → class_type (keyword string)
}

class Circle : IShape
{
    private readonly double _r;               // type_ → value_type (keyword double)
    public Circle(double r) { _r = r; }       // param r: type_ → value_type (keyword double)
    public double Area() { return Math.PI * _r * _r; }  // return: type_ → value_type (keyword double)
}

// ── type_parameter ────────────────────────────────────────────────────────────
// T is registered when Box<T>'s type_parameter_list is parsed, so every
// occurrence of T inside the class body resolves to type_ → type_parameter.

class Box<T>
{
    private T _v;                  // type_ → type_parameter
    public Box(T v) { _v = v; }   // param v: type_ → type_parameter

    public T    Get()    { return _v; }  // return: type_ → type_parameter
    public void Set(T v) { _v = v; }    // param v: type_ → type_parameter
}

// ── Main program ──────────────────────────────────────────────────────────────

class Program
{
    // ── pointer_type  (requires /unsafe) ─────────────────────────────────────
    static unsafe void PointerExamples()
    {
        int   x  = 42;        // type_ → value_type (keyword int)
        int*  p  = &x;        // type_ → pointer_type
        char  c  = 'A';       // type_ → value_type (keyword char)
        char* cs = &c;        // type_ → pointer_type
        void* raw = p;        // type_ → pointer_type
        Console.WriteLine("pointer: *p = {0}", *p);
    }

    static void Main()
    {
        // ── value_type: simple-type keywords ─────────────────────────────────
        // IsValueTypeName() catches these by token type, no symbol table needed.
        int    i = 1;            // type_ → value_type (keyword int)
        bool   b = true;         // type_ → value_type (keyword bool)
        double d = 3.14;         // type_ → value_type (keyword double)

        // ── reference_type → class_type: named struct / enum ─────────────────
        // Point and Color are NOT in the symbol table → IsValueTypeName() returns
        // false for an unregistered identifier → falls through to
        // IsReferenceTypeName() → true (open-world) → class_type default.
        // Would be value_type if the symbol table had registered them as Struct/Enum.
        Point pt;                // type_ → reference_type → class_type  (Point unregistered)
        pt.X = 1f; pt.Y = 2f;
        Color col = Color.Red;   // type_ → reference_type → class_type  (Color unregistered)

        // ── reference_type → class_type: keywords ────────────────────────────
        string s = "hello";      // type_ → reference_type → class_type (keyword string)
        object o = 42;           // type_ → reference_type → class_type (keyword object)

        // ── reference_type → array_type ──────────────────────────────────────
        int[] arr = new int[3];  // type_ → reference_type → array_type
                                 //   (new int[3] uses non_array_type, not type_, for int)

        // ── reference_type → class_type: named class / interface / delegate ──
        // Animal, IShape, Handler are all unregistered → class_type default.
        // IShape would be interface_type if registered as Interface;
        // Handler would be delegate_type if registered as Delegate.
        Animal a = new Animal(); // var: type_ → reference_type → class_type (Animal unregistered)
                                 // new: type_ → reference_type → class_type (Animal)

        IShape sh = new Circle(5.0);
                                 // var: type_ → reference_type → class_type (IShape unregistered)
                                 // new: type_ → reference_type → class_type (Circle)

        Handler h = delegate { Console.WriteLine("delegate fired"); };
                                 // var: type_ → reference_type → class_type (Handler unregistered)

        // ── reference_type → class_type: instantiated generic ────────────────
        // Box<T> variable types and new-expression types → reference_type → class_type.
        // Type arguments in angle brackets are also type_ nodes:
        //   <int>    → type_ → value_type (keyword int)
        //   <string> → type_ → reference_type → class_type (keyword string)
        // The type_parameter alternative fires for T *inside* Box<T> — see above.
        Box<int>    bi = new Box<int>(99);     // var/new: type_ → reference_type → class_type
                                               // <int>:   type_ → value_type (keyword int)
        Box<string> bs = new Box<string>("hi"); // var/new: type_ → reference_type → class_type
                                               // <string>: type_ → reference_type → class_type

        // ── print results ─────────────────────────────────────────────────────
        Console.WriteLine("int={0} bool={1} double={2}", i, b, d);
        Console.WriteLine("Point=({0},{1})  Color={2}", pt.X, pt.Y, col);
        Console.WriteLine("string={0}  object={1}", s, o);
        Console.WriteLine("arr.Length={0}  Animal={1}", arr.Length, a.Name);
        Console.WriteLine("IShape.Area={0:F4}", sh.Area());
        h();
        Console.WriteLine("Box<int>={0}  Box<string>={1}", bi.Get(), bs.Get());
        PointerExamples();
    }
}
