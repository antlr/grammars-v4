// Compile (from cmd.exe or PowerShell, not MSYS2 bash):
//   "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\csc.exe" /unsafe non_nullable_reference_type_alternatives.cs
//
// Demonstrates all five alternatives of the ANTLR4 rule:
//
//   non_nullable_reference_type
//       : {IsDelegateTypeName()}?  delegate_type
//       | {IsInterfaceTypeName()}? interface_type
//       | {IsClassTypeName()}?     class_type
//       | array_type
//       | 'dynamic'
//       ;
//
// NOTE on symbol-table predicates:
//   IsDelegateTypeName() and IsInterfaceTypeName() fire only for names
//   positively registered in the symbol table as Delegate or Interface.
//   IsClassTypeName() is the open-world default: it returns true for the
//   keywords 'object' and 'string', and for any unknown identifier.
//
//   In this test harness the top-level declarations (delegate Notifier,
//   delegate Transformer, interface IShape, interface ILogger, class Animal,
//   class Dog) are NOT pre-registered, so all user-defined type names fall
//   through to the class_type default.
//   Would classify correctly if the symbol table were populated.

using System;

// ── Supporting declarations ───────────────────────────────────────────────────

delegate void Notifier();                    // unregistered → class_type default
delegate int  Transformer(int x);            // unregistered → class_type default

interface IShape  { double Area(); }         // unregistered → class_type default
interface ILogger { void Log(string msg); }  // unregistered → class_type default

class Animal { public string Name = ""; }    // unregistered → class_type default

class Dog : Animal
{
    public Dog() { Name = "Rex"; }
}

class Circle : IShape
{
    public double Area() { return 3.14159; }
}

class ConsoleLogger : ILogger
{
    public void Log(string msg) { Console.WriteLine(msg); }
}

// ── Main program ──────────────────────────────────────────────────────────────

class Program
{
    static void Main()
    {
        // ── alternative: 'dynamic' keyword — no predicate ────────────────────
        dynamic dyn = "anything";
                                 // type_ → reference_type → non_nullable_reference_type: dynamic

        // ── alternative: class_type — keywords ───────────────────────────────
        // IsClassTypeName() fires on token type; no symbol-table lookup.
        object obj = 42;
                                 // type_ → reference_type → non_nullable_reference_type → class_type (keyword object)
        string str = "hello";
                                 // type_ → reference_type → non_nullable_reference_type → class_type (keyword string)

        // ── alternative: array_type — rank_specifier makes it unambiguous ─────
        int[]    ia = new int[3];
                                 // type_ → reference_type → non_nullable_reference_type → array_type
        string[] sa = new string[2];
                                 // type_ → reference_type → non_nullable_reference_type → array_type
        object[] oa = new object[1];
                                 // type_ → reference_type → non_nullable_reference_type → array_type
        Animal[] aa = new Animal[2];
                                 // type_ → reference_type → non_nullable_reference_type → array_type
        int[,]   ma = new int[2, 3];
                                 // type_ → reference_type → non_nullable_reference_type → array_type (rank 2)

        // ── alternative: class_type — user-defined class (open-world default) ─
        Animal ani = new Dog();
                                 // type_ → reference_type → non_nullable_reference_type → class_type (Animal unregistered)
        Dog    dog = new Dog();
                                 // type_ → reference_type → non_nullable_reference_type → class_type (Dog unregistered)

        // ── class_type default for unregistered interface ─────────────────────
        // Would be non_nullable_reference_type → interface_type
        // if IShape / ILogger were registered as Interface.
        IShape  sh = new Circle();
                                 // type_ → reference_type → non_nullable_reference_type → class_type (IShape unregistered)
        ILogger lg = new ConsoleLogger();
                                 // type_ → reference_type → non_nullable_reference_type → class_type (ILogger unregistered)

        // ── class_type default for unregistered delegate ──────────────────────
        // Would be non_nullable_reference_type → delegate_type
        // if Notifier / Transformer were registered as Delegate.
        Notifier    n  = delegate { Console.WriteLine("notified"); };
                                 // type_ → reference_type → non_nullable_reference_type → class_type (Notifier unregistered)
        Transformer tf = x => x * 2;
                                 // type_ → reference_type → non_nullable_reference_type → class_type (Transformer unregistered)

        // ── new-expression types also carry non_nullable_reference_type nodes ──
        Animal ani2 = new Animal();
                                 // new: type_ → reference_type → non_nullable_reference_type → class_type (Animal)
        IShape  sh2 = new Circle();
                                 // new: type_ → reference_type → non_nullable_reference_type → class_type (Circle)

        // ── print results ─────────────────────────────────────────────────────
        Console.WriteLine("dynamic={0}", dyn);
        Console.WriteLine("object={0}  string={1}", obj, str);
        Console.WriteLine("ia.Length={0}  sa.Length={1}  oa.Length={2}", ia.Length, sa.Length, oa.Length);
        Console.WriteLine("aa.Length={0}  ma rank={1}", aa.Length, ma.Rank);
        Console.WriteLine("Animal.Name={0}  Dog.Name={1}", ani.Name, dog.Name);
        Console.WriteLine("IShape.Area={0:F5}", sh.Area());
        lg.Log("ILogger works");
        n();
        Console.WriteLine("Transformer(5)={0}", tf(5));
        Console.WriteLine("ani2.Name={0}  sh2.Area={1:F5}", ani2.Name, sh2.Area());
    }
}
