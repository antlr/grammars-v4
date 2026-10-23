// Compile:
//   cd csharp/v8-spec/design
//   dotnet run --project class_base_alternatives.csproj
//
// Demonstrates all three alternatives of the ANTLR4 rule:
//
//   class_base
//       : {IsClassBaseInterfaceList()}? ':' interface_type_list
//       | {IsClassBaseClassType()}?     ':' class_type
//       | ':' class_type ',' interface_type_list
//       ;
//
// NOTE on symbol-table predicates:
//   IsClassBaseInterfaceList() fires when LT(2) — the type name immediately
//   after ':' — is positively registered in the symbol table as an Interface.
//   IsClassBaseClassType() is the open-world default: it fires when LT(2) is
//   NOT a known interface (unknown, object, string, or a registered class /
//   struct / enum / delegate).
//   Once IsClassBaseClassType() is true, ANTLR4's LL(*) engine chooses
//   between alt 2 (':' class_type) and alt 3 (':' class_type ',' interface_type_list)
//   by looking for ',' after class_type.
//
//   In this test harness ALL top-level type declarations below are NOT
//   pre-registered in the symbol table, so every name falls through to the
//   IsClassBaseClassType() default.  Predictions with a pre-populated symbol
//   table would route IFoo/IBar/ILogger declarations to alt 1.

using System;

// ── Interfaces ────────────────────────────────────────────────────────────────

interface IAnimal       // unregistered → IsClassBaseClassType default
{
    string Name { get; }
    string Speak();
}

interface ILogger       // unregistered → IsClassBaseClassType default
{
    void Log(string msg);
}

interface IFormattable2 // unregistered → IsClassBaseClassType default
{
    string Format(string spec);
}

// ── Base classes ──────────────────────────────────────────────────────────────

class LivingThing       // unregistered → IsClassBaseClassType default
{
    public bool IsAlive = true;
}

class NamedThing : LivingThing
// class_base → alt 2 — ':' class_type
//   IsClassBaseClassType() fires: LT(2)=LivingThing, unregistered → default class
//   No ',' after class_type → alt 2 chosen by LL(*)
{
    public string Name { get; set; }
    public NamedThing(string name) { Name = name; }
}

// ── alt 2: ':' class_type  (class inheriting a class, no interfaces) ──────────

class Animal : NamedThing
// class_base → alt 2 — ':' class_type
//   IsClassBaseClassType() fires: LT(2)=NamedThing, unregistered → default class
//   No ',' after class_type → alt 2 chosen by LL(*)
{
    public Animal(string name) : base(name) { }
    public virtual string Speak() { return "..."; }
}

class Dog : Animal
// class_base → alt 2 — ':' class_type
//   IsClassBaseClassType() fires: LT(2)=Animal, unregistered → default class
//   No ',' after class_type → alt 2 chosen by LL(*)
{
    public Dog(string name) : base(name) { }
    public override string Speak() { return "Woof"; }
}

// ── alt 2: ':' class_type with keyword class type (object / string) ───────────

class StringWrapper : object
// class_base → alt 2 — ':' class_type
//   IsClassBaseClassType() fires: LT(2)=object keyword → always class
//   No ',' after class_type → alt 2 chosen by LL(*)
{
    private readonly string _val;
    public StringWrapper(string v) { _val = v; }
    public override string ToString() { return _val; }
}

// ── alt 1: ':' interface_type_list  (implements interfaces only) ──────────────
//
// With a pre-populated symbol table:
//   IsClassBaseInterfaceList() would fire when LT(2) is a registered Interface,
//   routing the entire base list to alt 1.
//
// In this harness IAnimal/ILogger/IFormattable2 are unregistered, so
// IsClassBaseClassType() fires instead.  The parse tree differs from a
// fully-registered symbol table:
//
// Registered symbol table parse:
//   class_base → alt 1 — ':' interface_type_list (IAnimal, ILogger)
//
// Unregistered (this harness) parse:
//   class_base → alt 3 — ':' class_type (IAnimal) ',' interface_type_list (ILogger)
//   (first name is treated as class; rest as interface list)
//
// The class compiles correctly in both cases; only the parse-tree node differs.

class ConsoleAnimal : IAnimal, ILogger
// Registered: class_base → alt 1 — ':' interface_type_list
//               IsClassBaseInterfaceList(): LT(2)=IAnimal registered as Interface → true
//               interface_type_list = IAnimal, ILogger
// Unregistered: class_base → alt 3 — ':' class_type ',' interface_type_list
//               IsClassBaseClassType(): LT(2)=IAnimal unregistered → default class → true
//               class_type = IAnimal, interface_type_list = ILogger
{
    public string Name { get; }
    public ConsoleAnimal(string name) { Name = name; }
    public string Speak()    { return "Hello from " + Name; }
    public void   Log(string msg) { Console.WriteLine("[LOG] " + msg); }
}

// ── alt 3: ':' class_type ',' interface_type_list  (extends a class + interfaces)

class LoggingDog : Dog, ILogger
// Registered: class_base → alt 3 — ':' class_type ',' interface_type_list
//               IsClassBaseClassType(): LT(2)=Dog, registered as Class → true
//               No predicate on alt 3; LL(*) sees ',' after class_type → alt 3
//               class_type = Dog, interface_type_list = ILogger
// Unregistered: class_base → alt 3 — ':' class_type ',' interface_type_list
//               IsClassBaseClassType(): LT(2)=Dog, unregistered → default class → true
//               LL(*) sees ',' → alt 3
//               class_type = Dog, interface_type_list = ILogger
{
    public LoggingDog(string name) : base(name) { }
    public void Log(string msg) { Console.WriteLine("[DOG:" + Name + "] " + msg); }
}

class FormattingAnimal : Animal, ILogger, IFormattable2
// Registered: class_base → alt 3 — ':' class_type ',' interface_type_list
//               IsClassBaseClassType(): LT(2)=Animal, registered as Class → true
//               LL(*) sees ',' → alt 3
//               class_type = Animal, interface_type_list = ILogger, IFormattable2
// Unregistered: class_base → alt 3 (same reasoning — LL(*) sees ',')
{
    public FormattingAnimal(string name) : base(name) { }
    public void   Log(string msg)         { Console.WriteLine("[FA] " + msg); }
    public string Format(string spec)     { return string.Format("[{0}:{1}]", spec, Name); }
    public override string Speak()        { return "Formatted speak"; }
}

// ── Main program ──────────────────────────────────────────────────────────────

class Program
{
    static void Main()
    {
        // alt 2: class inheriting a class only
        var dog = new Dog("Rex");
        Console.WriteLine("Dog.Name={0}  Dog.Speak={1}", dog.Name, dog.Speak());

        var sw = new StringWrapper("wrapped");
        Console.WriteLine("StringWrapper={0}", sw);

        // alt 1 (registered) / alt 3 (unregistered): implements interfaces only
        IAnimal ca = new ConsoleAnimal("Buddy");
        Console.WriteLine("ConsoleAnimal.Name={0}  Speak={1}", ca.Name, ca.Speak());
        ((ILogger)ca).Log("ConsoleAnimal log test");

        // alt 3: extends a class and implements interfaces
        var ld = new LoggingDog("Max");
        ld.Log("LoggingDog log test");
        Console.WriteLine("LoggingDog.Speak={0}", ld.Speak());

        var fa = new FormattingAnimal("Leo");
        fa.Log("FormattingAnimal log test");
        Console.WriteLine("FormattingAnimal.Format={0}", fa.Format("X"));
    }
}
