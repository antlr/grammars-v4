// CSharp7ClassMemberDeclarations.cs
// Exercises all 11 class_member_declaration alternatives from ECMA-334 7th edition §15.3:
//
//   class_member_declaration (grammar):
//     attributes? all_member_modifiers?
//       ( common_member_declaration | destructor_definition )
//
//   1.  constant_declaration
//   2.  field_declaration
//   3.  method_declaration
//   4.  property_declaration
//   5.  event_declaration
//   6.  indexer_declaration
//   7.  operator_declaration  (+ conversion_operator_declarator)
//   8.  constructor_declaration
//   9.  destructor_declaration
//  10.  static_constructor_declaration
//  11.  type_declaration  (nested class / struct / interface / enum / delegate)

using System;

namespace CSharp7ClassMembers
{
    // ── 1. constant_declaration ──────────────────────────────────────────────
    //   grammar: common_member_declaration → constant_declaration
    //            → CONST type_ constant_declarators ';'
    //   (shown both at namespace-scope and inside Showcase for completeness;
    //    the class-level ones are the primary test)

    public class Showcase
    {
        // 1. constant_declaration
        //    grammar: CONST type_ constant_declarators ';'
        public  const int    Capacity     = 64;
        private const string DefaultLabel = "item";

        // ── 2. field_declaration ─────────────────────────────────────────────
        //    grammar: typed_member_declaration → field_declaration
        //             → variable_declarators ';'
        private int    _value;
        private static int    _instanceCount;
        private int[]  _items         = new int[Capacity];
        private protected int _secret;          // C# 7.2: private protected

        // ── 10. static_constructor_declaration ──────────────────────────────
        //    grammar: STATIC modifier + constructor_declaration
        //             body extended to => expr in C# 7.0
        static Showcase() => _instanceCount = 0;

        // ── 8. constructor_declaration ───────────────────────────────────────
        //    grammar: common_member_declaration → constructor_declaration
        //             → identifier '(' params? ')' constructor_initializer? body
        //    (a) block body with this-initializer
        public Showcase() : this(0) { }
        //    (b) C# 7.0 expression-bodied body
        public Showcase(int value) => _value = value;

        // ── 9. destructor_declaration ────────────────────────────────────────
        //    grammar: class_member_declaration → destructor_definition
        //             → '~' identifier '(' ')' body
        //             body extended to => expr in C# 7.0
        ~Showcase() => _instanceCount--;

        // ── 3. method_declaration ────────────────────────────────────────────
        //    grammar: VOID method_declaration
        //           | typed_member_declaration → method_declaration
        //    (a) void method, expression-bodied (C# 7.0)
        public void Reset() => _value = 0;
        //    (b) non-void method, expression-bodied
        public int GetValue() => _value;
        //    (c) static method
        public static int Count() => _instanceCount;
        //    (d) generic method
        public T Identity<T>(T x) => x;
        //    (e) C# 7.2 private protected method
        private protected void Touch() => _secret++;

        // ── 4. property_declaration ──────────────────────────────────────────
        //    grammar: typed_member_declaration → property_declaration
        //             → member_name ( accessor_declarations | => expr )
        //    (a) full get/set with C# 7.0 expression-bodied accessors
        public int Value
        {
            get => _value;
            set => _value = value;
        }
        //    (b) auto-property with initializer (C# 6)
        public string Label { get; private set; } = DefaultLabel;
        //    (c) expression-bodied read-only property (C# 6)
        public bool IsPositive => _value > 0;

        // ── 5. event_declaration ─────────────────────────────────────────────
        //    grammar: common_member_declaration → event_declaration
        //    (a) field-like: EVENT type_ variable_declarators ';'
        public event EventHandler ValueChanged;
        //    (b) accessor form: EVENT type_ member_name '{' add/remove '}'
        public event EventHandler LabelChanged
        {
            add    { ValueChanged += value; }
            remove { ValueChanged -= value; }
        }

        // ── 6. indexer_declaration ───────────────────────────────────────────
        //    grammar: typed_member_declaration → indexer_declaration
        //             → THIS '[' formal_parameter_list ']' ( accessors | => )
        public int this[int i]
        {
            get => _items[i];
            set => _items[i] = value;
        }

        // ── 7. operator_declaration ──────────────────────────────────────────
        //    grammar: typed_member_declaration → operator_declaration
        //             → OPERATOR overloadable_operator '(' params ')' body
        //    (a) binary operator
        public static Showcase operator +(Showcase a, Showcase b)
            => new Showcase(a._value + b._value);
        //    (b) equality operators (pair required by compiler)
        public static bool operator ==(Showcase a, Showcase b)
            => a._value == b._value;
        public static bool operator !=(Showcase a, Showcase b)
            => !(a == b);

        //    conversion operators
        //    grammar: common_member_declaration → conversion_operator_declarator body
        //    (c) implicit conversion
        public static implicit operator int(Showcase s) => s._value;
        //    (d) explicit conversion
        public static explicit operator Showcase(int i) => new Showcase(i);

        // required when overloading == / !=
        public override bool Equals(object obj)
            => obj is Showcase s && s._value == _value;
        public override int GetHashCode() => _value;

        // ── 11. type_declaration: five nested-type forms ─────────────────────
        //    grammar: common_member_declaration →
        //       class_definition | struct_definition | interface_definition
        //     | enum_definition  | delegate_definition

        // (a) nested class
        public class Nested
        {
            public int X;
        }

        // (b) nested struct
        public struct Point
        {
            public int X, Y;
        }

        // (c) nested interface
        public interface IWorker
        {
            void Work();
        }

        // (d) nested enum
        public enum State { Idle, Active, Done }

        // (e) nested delegate
        public delegate string Formatter(int value);
    }

    class CSharp7ClassMemberDeclarations
    {
        static void Main()
        {
            Console.WriteLine(Showcase.Capacity);               // 1:  constant
            var a = new Showcase(5);                            // 8:  constructor (expr-bodied)
            var b = new Showcase(3);                            // 8:  constructor
            Console.WriteLine(a.Value);                         // 4:  property get
            a.Value = 10;                                       // 4:  property set
            Console.WriteLine(a.IsPositive);                    // 4:  expression-bodied property
            a[0] = 42;                                          // 6:  indexer set
            Console.WriteLine(a[0]);                            // 6:  indexer get
            var c = a + b;                                      // 7:  operator +
            Console.WriteLine((int)c);                          // 7:  implicit → explicit cast
            Console.WriteLine(a == b);                          // 7:  operator ==
            a.ValueChanged += (o, e) => { };                    // 5:  event (field-like)
            a.Reset();                                          // 3:  void method
            Console.WriteLine(a.GetValue());                    // 3:  non-void method
            Console.WriteLine(a.Identity("ok"));                // 3:  generic method
            var p = new Showcase.Point { X = 1, Y = 2 };       // 11b: nested struct
            Console.WriteLine(p.X + p.Y);
            var s = Showcase.State.Active;                      // 11d: nested enum
            Console.WriteLine(s);
            Showcase.Formatter fmt = v => v.ToString("X");      // 11e: nested delegate
            Console.WriteLine(fmt(255));
        }
    }
}
