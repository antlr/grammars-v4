extern alias Foo;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using M = System.Math;

using ConsoleApplication2.Test;

/**/
/* the previous comment is an empty delimited comment and not a document comment */
/** this is a document comment */
// this one is a single line comment

using X = int1;
using Y = ABC.X<int>;

using static System.Math;
using static System.DayOfWeek;
using static System.Linq.Enumerable;

[assembly: System.Copyright(@"(C)"" 

2009")]
[module: System.Copyright("\n\t\u0123(C) \"2009" + "\u0123")]

class TopLevelType : IDisposable
{
    void IDisposable.Dispose() { }
}

namespace My
{
    using A.B;

    interface CoContra<out T, in K> { }
    delegate void CoContra2<[System.Obsolete()] out T, in K> () where T : struct;

    public unsafe partial class A : C, I
    {
        [DllImport("kernel32", SetLastError = true)]
        static extern bool CreateDirectory(string name, SecurityAttribute sa);

        private const int global = int.MinValue - 1;

        static A() 
        { 
        }

        [method: Obsolete]
        public A([param: Obsolete] int foo) :
            base(1)
        {
        L:
            {
                int i = sizeof(int);
                ++i;
                var s1 = $"x {1 , -2 :d}";
                var s2 = $@"x {1 , -2 :d}";
            }


      Console.WriteLine(export.iefSupplied.command);

            const int? local = int.MaxValue;
            const Guid? local0 = new Guid(r.ToString());

            var привет = local;
            var мир = local;
            int local3 = 0, local4 = 1;
            local3 = local4 = 1;
            var local5 = null as Action ?? null;
            var local6 = local5 is Action;

            var u = 1u;
            var U = 1U;
            long hex = 0xBADC0DE, Hex = 0XDEADBEEF, l = -1L, L = 1L, l2 = 2l;
            ulong ul = 1ul, Ul = 1Ul, uL = 1uL, UL = 1UL,
                lu = 1lu, Lu = 1Lu, lU = 1lU, LU = 1LU;
            int minInt32Value = -2147483648;
            int minInt64Value = -9223372036854775808L;

            bool @bool;
            byte @byte;
            char @char = 'c', \u0066 = '\u0066', hexchar = '\x0130', hexchar2 = (char)0xBAD;
            string \U00000065 = "\U00000065";
            decimal @decimal = 1.44M;
            @decimal = 1.2m;
            dynamic @dynamic;
            double @double = M.PI;
            @double = 1d;
            @double = 1D;
            @double = -1.2e3;
            float @float = 1.2f;
            @float = 1.44F;
            int @int = local ?? -1;
            long @long;
            object @object;
            sbyte @sbyte;
            short @short;
            string @string = @"""/*";
            uint @uint;
            ulong @ulong;
            ushort @ushort;
            
            dynamic dynamic = local5;
            var add = 0;
            var alias = 0;
            var arglist = 0;
            var ascending = 0;
            var async = 0;
            var await = 0;
            var by = 0;
            var descending = 0;
            var dynamic = 0;
            var equals = 0;
            var from = 0;
            var get = 0;
            var group = 0;
            var into = 0;
            var join = 0;
            var let = 0;
            var nameof = 0;
            var on = 0;
            var orderby = 0;
            var partial = 0;
            var remove = 0;
            var select = 0;
            var set = 0;
			var var = 0;
            var when = 0;
            var where = 0;
            var yield = 0;
            var __ = 0;
            where = yield = 0;

            if (i > 0)
            {
                return;
            }
            else if (i == 0)
            {
                throw new Exception();
            }
            var o1 = new MyObject();
            var o2 = new MyObject(var);
            var o3 = new MyObject { A = i };
            var o4 = new MyObject(@dynamic)
            {
                A = 0,
                B = 0,
                C = 0
            };
            var o5 = new { A = 0 };
            var dictionaryInitializer = new Dictionary<int, string> 
            { 
                {1, ""}, 
                {2, "a"} 
            };
            float[] a = new float[] 
            {
                0f,
                1.1f
            };
            int[, ,] cube = { { { 111, 112, }, { 121, 122 } }, { { 211, 212 }, { 221, 222 } } };
            int[][] jagged = { { 111 }, { 121, 122 } };
            int[][,] arr = new int[5][,]; // as opposed to new int[][5,5]
            arr[0] = new int[5,5];  // as opposed to arr[0,0] = new int[5];
            arr[0][0,0] = 47;
            int[] arrayTypeInference = new[] { 0, 1, };
            switch (3) { }
            switch (i)
            {
                case 0:
                case 1:
                    {
                        goto case 2;
                    }
                case 2 + 3:
                    {
                        goto default;
                        break;
                    }
                default:
                    {
                        return;
                    }
            }
            while (i < 10)
            {
                ++i;
                if (true) continue;
                break;
            }
            do
            {
                ++i;
                if (true) continue;
                break;
            }
            while (i < 10);
            for (int j = 0; j < 100; ++j)
            {
                for(;;) 
                {
                    for (int i = 0, j = 0; i < length; i++, j++) { }
                    if (true) continue;
                    break;
                }
            }
            label:
            goto label;
            label2: ;
            foreach (var i in Items())
            {
                if (i == 7)
                    return;
                else
                    continue;
            }
            checked
            {
                checked(++i);
            }
            unchecked
            {
                unchecked(++i);
            }
            lock (sync)
                process();
            using (var v = BeginScope())
            using (A a = new A())
            using (A a = new A(), b = new A())
            using (BeginScope())
                return;
            yield return this.items[3];
            yield break;
            fixed (int* p = stackalloc int[100], q = &y)
            {
                *intref = 1;
            }
            fixed (int* p = stackalloc int[100])
            {
                *intref = 1;
            }
            unsafe
            {
                int* p = null;
            }
            try
            {
                throw null;
            }
            catch (System.AccessViolationException av)
            {
                throw av;
            }
            catch (Exception)
            {
                throw;
            }
            finally
            {
                try { } catch { }
            }
            var anonymous = 
            {
                A = 1,
                B = 2,
                C = 3,
            };
            var query = from c in customers
                        let d = c
                        where d != null
                        join c1 in customers on c1.GetHashCode() equals c.GetHashCode()
                        join c1 in customers on c1.GetHashCode() equals c.GetHashCode() into e
                        group c by c.Country
                            into g
                            orderby g.Count() ascending
                            orderby g.Key descending
                            select new { Country = g.Key, CustCount = g.Count() };
            query = from c in customers
                    select c into d
                    select d;
        }
        ~A()
        {
        }
        private readonly int f1;
        [Obsolete]
        [NonExisting]
        [Foo::NonExisting(var, 5)]
        [CLSCompliant(false)]
        [Obsolete, System.NonSerialized, NonSerialized, CLSCompliant(true || false & true)]
        private volatile int f2;
        [return: Obsolete]
        [method: Obsolete]
        public void Handler(object value)
        {
        }
        public int m<T>(T t)
          where T : class, new()
        {
            base.m(t);
            return 1;
        }
        public string P
        {
            get
            {
                return "A";
            }
            set;
        }
        public abstract string P
        {
            get;
        }
        public abstract int this[int index]
        {
            protected internal get;
            internal protected set;
        }
        [method: Obsolete]
        [field: Obsolete]
        [event: Obsolete]
        public readonly event Event E;
        [event: Test]
        public event Action E1
        {
            [Obsolete]
            add { value = value; }
            [Obsolete]
            [return: Obsolete]
            remove { E += Handler; E -= Handler; }
        }
        public static A operator +(A first, A second)
        {
            Delegate handler = new Delegate(Handler);
            return first.Add(second);
        }
        [method: Obsolete]
        [return: Obsolete]
        public static bool operator true(A a)
        {
            return true;
        }
        public static bool operator false(A a)
        {
            return false;
        }
        class C
        {
        }
    }
    public struct S : I
    {
        public S()
        {
        }
        private int f1;
        [Obsolete("Use Script instead", error: false)]
        private volatile int f2;
        public abstract int m<T>(T t)
          where T : struct
        {
            return 1;
        }
        public string P
        {
            get
            {
                int value = 0;
                return "A";
            }
            set;
        }
        public abstract string P
        {
            get;
        }
        public abstract int this[int index]
        {
            get;
            internal protected set;
        }
        public event Event E;
        public static A operator +(A first, A second)
        {
            return first.Add(second);
        }
        fixed int field[10];
        class C
        {
        }
    }
    public interface I
    {
        void A(int value);
        string Value
        {
            get;
            set;
        }
        unsafe void UpdateSignatureByHashingContent([In]byte* buffer, int size);
    }
    [type: Flags]
    public enum E
    {
        A,
        B = A,
        C = 2 + A,
        D,
    }
    
    public delegate void Delegate(object P);
    namespace Test
    {
        using System;
        using System.Collections;
        public class Список
        {
            public static IEnumerable Power(int number, int exponent)
            {
                Список Список = new Список();
                Список.Main();
                int counter = (0 + 0);
                int אתר = 0;
                while (++counter++ < --exponent--)
                {
                    result = result * number + +number+++++number;
                    yield return result;
                }
            }
            static void Main()
            {
                foreach (int i in Power(2, 8))
                {
                    Console.Write("{0} ", i);
                }
            }
            async void Wait()
            {
                await System.Threading.Tasks.Task.Delay(0);
            }
            void AsyncAnonymous() // C # 5 feature
            {
                var task = Task.Factory.StartNew(async () =>
                {
                    return await new WebClient().DownloadStringTaskAsync("http://example.com");
                });
            }
        }
    }
}

namespace ConsoleApplication1
{
    namespace RecursiveGenericBaseType
    {
        class A<T> : B<A<T>, A<T>> where T : A<T>
        {
            protected virtual A<T> M() { }
            protected abstract B<A<T>, A<T>> N() { }
            static B<A<T>, A<T>> O() { }
        }

        sealed class B<T1, T2> : A<B<T1, T2>>
        {
            protected override A<T> M() { }
            protected sealed override B<A<T>, A<T>> N() { }
            new static A<T> O() { }
        }
    }

    namespace Boo
    {
        public class Bar<T> where T : IComparable
        {
            public T f;
            public class Foo<U> : IEnumerable<T>
            {
                public void Method<K, V>(K k, T t, U u)
                    where K : IList<V>, IList<T>, IList<U>
                    where V : IList<K>
                {
                    A<int> a;
                    M(A<B, C>(5));
                }
            };
        };
    };

    class Test
    {
        void Bar3()
        {
            var x = new Boo.Bar<int>.Foo<object>();
            x.Method<string, string>(" ", 5, new object());

            var q = from i in new int[] { 1, 2, 3, 4 }
                    where i > 5
                    select i;
        }

        public static implicit operator Test(string s)
        {
            return new ConsoleApplication1.Test();
        }
        public static explicit operator Test(string s = "")
        {
            return new Test();
        }

        public int foo = 5;
        void Bar2()
        {
            foo = 6;
            this.Foo = 5.GetType(); Test t = "sss";
        }

        public event EventHandler MyEvent = delegate { };

        void Blah()
        {
            int i = 5;
            int? j = 6;

            Expression<Func<int>> e = () => i;
            Expression<Func<bool, Action>> e2 = b => () => { return; };
            Func<bool, bool> f = async delegate (bool a)
            {
                return await !a;
            };
            Func<int, int, int> f2 = (a, b) => 0;
            f2 = (int a, int b) => 1;
            Action a = Blah;
            f2 = () => {};
            f2 = () => {;};
        }

        delegate Recursive Recursive(Recursive r);
        delegate Recursive Recursive<A,R>(Recursive<A,R> r);

        public Type Foo
        {
            [Obsolete("Name", error = false)]
            get
            {
                var result = typeof(IEnumerable<int>);
                var t = typeof(int?) == typeof(Nullable<int>);
                t = typeof(IEnumerable<int?[][][]>);
                return typeof(IEnumerable<>);
            }
            set
            {
                var t = typeof(System.Int32);
                t.ToString();
                t = value;
            }
        }

        public void Constants()
        {
            int i = 1 + 2 + 3 + 5;
            global::System.String s = "a" + (System.String)"a" + "a" + "a" + "a" + "A";
        }

        public void ConstructedType()
        {
            List<int> i = null;
            int c = i.Count;
        }
    }
}

namespace Comments.XmlComments.UndocumentedKeywords
{
    /// <summary>
    /// Whatever 
    /// </summary>
    /// <!-- c -->
    /// <![CDATA[c]]> //
    /// <c></c> /* */
    /// <code></code>
    /// <example></example>
    /// <exception cref="bla"></exception>
    /// <include file='' path='[@name=""]'/>
    /// <permission cref=" "></permission>
    /// <remarks></remarks>
    /// <see cref=""/>
    /// <seealso cref=" "/>
    /// <value></value>
    /// <typeparam name="T"></typeparam>
    class /*///*/C<T>
    {
        void M<U>(T t, U u)
        {
            // comment
            /* *** / */
            /* //
             */
            /*s*///comment
            // /***/
            /*s*/int /*s*/intValue = 0;
            intValue = intValue /*s*/+ 1;
            string strValue = /*s*/"hello";
            /*s*/MyClass c = new MyClass();
            string verbatimStr = /*s*/@"\\\\";
        }
    }

    //General Test F. Type a very long class name, verify colorization happens correctly only upto the correct size (118324)
    class TestClassXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX/*Scen8*/{ }

    class TestClassXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX22/*Scen9*/{ }

    class yield
    {
        void Foo<U>(__arglist)
        {
            C<U> c = null;
            c.M<int>(5, default(U));
            TypedReference tr = __makeref(c);
            Type t = __reftype(tr);
            int j = __refvalue(tr, int);
            Params(a: t, b: t);
            Params(ref c, out c);
        }
        void Params(ref dynamic a, out dynamic b, params dynamic[] c) {}
        void Params(out dynamic a = 2, ref dynamic c = default(dynamic), params dynamic[][] c) {}

        public override string ToString() { return base.ToString(); } 

        public partial void OnError();

        public partial void method()
        {
            int?[] a = new int?[5];/*[] bug*/ // YES []
            int[] var = { 1, 2, 3, 4, 5 };/*,;*/
            int i = a[i];/*[]*/
            Foo<T> f = new Foo<int>();/*<> ()*/
            f.method();/*().*/
            i = i + i - i * i / i % i & i | i ^ i;/*+ - * / % & | ^*/
            bool b = true & false | true ^ false;/*& | ^*/
            b = !b;/*!*/
            i = ~i;/*~i*/
            b = i < i && i > i;/*< && >*/
            int? ii = 5;/*? bug*/ // NO ?
            int f = true ? 1 : 0;/*? :*/   // YES :
            i++;/*++*/
            i--;/*--*/
            b = true && false || true;/*&& ||*/
            i << 5;/*<<*/
            i >> 5;/*>>*/
            b = i == i && i != i && i <= i && i >= i;/*= == && != <= >=*/
            i += 5.0;/*+=*/
            i -= i;/*-=*/
            i *= i;/**=*/
            i /= i;/*/=*/
            i %= i;/*%=*/
            i &= i;/*&=*/
            i |= i;/*|=*/
            i ^= i;/*^=*/
            i <<= i;/*<<=*/
            i >>= i;/*>>=*/
            object s = x => x + 1;/*=>*/
            double d = .3;
            Point point;
            unsafe
            {
                Point* p = &point;/** &*/
                p->x = 10;/*->*/
            }
            IO::BinaryReader br = null;
            x[i: 1] = 3;
            x[i: 1, j: 5] = "str";
        }

        struct Point { public int X; public int Y; public void ThisAccess() { this = this; } }
    }
    
    // From here:https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6
    class CSharp6Features
    {
        // Initializers for auto-properties
        public string First { get; set; } = "Jane";
        public string Last { get; set; } = "Doe";
    
        // Getter-only auto-properties
        public string Third { get; } = "Jane";
        public string Fourth { get; } = "Doe";
        
        // Expression bodies on method-like members
        public Point Move(int dx, int dy) => new Point(x + dx, y + dy); 
        public static Complex operator +(Complex a, Complex b) => a.Add(b);
        public static implicit operator string(Person p) => p.First + " " + p.Last;
        public void Print() => Console.WriteLine(First + " " + Last);
        
        // Expression bodies on property-like function members
        public string Name => First + " " + Last;
        public int this[long id] => id;
        
        async void Test()
        {
            // Using static
            WriteLine(Sqrt(3*3 + 4*4));
            WriteLine(Friday - Monday);            
            var range = Range(5, 17);                // Ok: not extension
            var even = range.Where(i => i % 2 == 0); // Ok
            
            // Null-conditional operators
            int? length = customers?.Length; // null if customers is null
            Customer first = customers?[0];  // null if customers is null            
            int length = customers?.Length ?? 0; // 0 if customers is null
            int? first = customers?[0]?.Orders?.Count();
            PropertyChanged?.Invoke(this, args);
            
            // String interpolation
            string s = $"{p.Name, 20} is {p.Age:D3} year{{s}} old #";
            s = $"{p.Name} is \"{p.Age} year{(p.Age == 1 ? "" : "s")} old";
            s = $"{(p.Age == 2 ? $"{new Person { } }" : "")}";
            s = $@"\{p.Name}
                                   ""\";
            s = $"Color [ R={func(b: 3):#0.##}, G={G:#0.##}, B={B:#0.##}, A={A:#0.##} ]";
            
            // nameof expressions
            if (x == null)
                throw new ArgumentNullException(nameof(x));
            WriteLine(nameof(person.Address.ZipCode)); // prints "ZipCode"
            
            // Index initializers
            var numbers = new Dictionary<int, string> {
                [7] = "seven",
                [9] = "nine",
                [13] = "thirteen"
            };
            
            // Exception filters
            try {}
            catch (MyException e) when (myfilter(e))
            { }
            
            // Await in catch and finally blocks
            Resource res = null;
            try
            {
                res = await Resource.OpenAsync();       // You could do this.
            } 
            catch(ResourceException e)
            {
                await Resource.LogAsync(res, e);         // Now you can do this …
            }
            finally
            {
                if (res != null)
                    await res.CloseAsync(); // … and this.
            }
        }
    }
}

class CSharp70
{
    void PatternMatching(string arg, int b)
    {
        switch (arg)
        {
            case "A" when b > 50:
            case "B" when b < 50:
            default:
                break;
        }

        (A<B,C> D, E<F,G> H) = e;

        if (x?.y?.z is Type value2)
        {
            // code using value
        }

        if (expr is Type v) { Hello(); }
    }

	public static async Task LocalFunctions(string[] args)
	{
		string Hello2(int i)
        {
            return args[i];
        }

		async Task<string> Hello<T>(T i) => await Task.FromResult(args[i]);
		await Hello(1);
	}

	public static void OutVar(string[] args)
	{
		int.TryParse(Hello(1), out var item);
		int.TryParse(Hello(1), out int item);
	}

    public void ThrowExpression()
    {
        var result = nullableResult ?? throw new NullReferenceException();
    }

    public void BinaryLiterals()
    {
        int nineteen = 0b10011;
    }

    public void DigitSeparators()
    {
        int bin = 0b1001_1010_0001_0100;
        int hex = 0x1b_a0_44_fe;
        int dec = 33_554_432;
        int weird = 1_2__3___4____5_____6______7_______8________9;
        double real = 1_000.111_1e-1_000;
    }
}

class CSharp71
{
    void DefaultWithoutTypeName(string content = default)
    {
        DefaultWithoutTypeName(default);
    }

    void TupleRecognize(int a, (int, int) b, (int, int, int)? c)
    {
        var result = list.Select(c => (c.f1, f3: c.f2)).Where(t => t.f2 == 1);
    }
}

class CSharp72
{
    readonly struct ReadonlyRef1
    {    
        Func<int, int> s = (in int x) => x;
        ref TValue this[in TKey index] => null;
        public static Vector3 operator+(in Vector3 x, in Vector3 y) => null;

        static readonly ref Vector3 M1_Trace()
        {
            // OK
            ref readonly var r1 = ref M1();

            // Not valid. Need an LValue
            ref readonly Vector3 r2 = ref default(Vector3);

            // Not valid. r1 is readonly.
            Mutate(ref r1);

            // OK.
            Print(in r1);

            // OK.
            return ref r1;
        }
    }

    ref struct ReadonlyRef2
    {
        ref readonly Guid Test(in Vector3 v1, in Vector3 v2)
        {
            // not OK!!
            v1 = default(Vector3);

            // not OK!!
            v1.X = 0;

            // not OK!!
            foo(ref v1.X);

            return ref (arr != null ? ref arr[0]: ref otherArr[0]);

            Span<int> span = stackalloc int[1];

            // OK
            return new Vector3(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z);
        }

        ref T Choice(bool condition, ref T consequence, ref T alternative)
        {
            if (condition)
            {
                 return ref consequence;
            }
            else
            {
                 return ref alternative;
            }
        }
    }

    public void DoSomething(bool isEmployed, string personName, int personAge) { }

    public void NonTrailingNamedArguments()
    {
        DoSomething(isEmployed:true, name, age); // currently CS1738, but would become legal
        DoSomething(true, personName:name, age); // currently CS1738, but would become legal
        DoSomething(name, isEmployed:true, age); // remains illegal
        DoSomething(name, age, isEmployed:true); // remains illegal
        DoSomething(true, personAge:age, personName:name); // already legal
    }

    public void ConditionalRef()
    {
        ref var r = ref (arr != null ? ref arr[0]: ref otherArr[0]);
    }

    public void LeadingSeparator()
    {
        var res = 0
        + 123      // permitted in C# 1.0 and later
        + 1_2_3    // permitted in C# 7.0 and later
        + 0x1_2_3  // permitted in C# 7.0 and later
        + 0b101    // binary literals added in C# 7.0
        + 0b1_0_1  // permitted in C# 7.0 and later

        // in C# 7.2, _ is permitted after the `0x` or `0b`
        + 0x_1_2   // permitted in C# 7.2 and later
        + 0b_1_0_1 // permitted in C# 7.2 and later
        ;
    }
}

class CSharp73
{
    void Blittable<T>(T value) where T : unmanaged
    {
        var unmanaged = 666;
    }

    unsafe struct IndexingMovableFixed
    {
        public fixed int myFixedField[10];
    }

    static IndexingMovableFixed s;

    public unsafe void IndexingMovableFixedFields()
    {
        int* ptr = s.myFixedField;
        int t = s.myFixedField[5];
    }

    public void PatternBasedFixed()
    {
        fixed(byte* ptr = byteArray)
        {
           // ptr is a native pointer to the first element of the array
           // byteArray is protected from being moved/collected by the GC for the duration of this block 
        }
    }

    public void StackallocArrayInitializer()
    {
        Span<int> a = stackalloc int[3];               // currently allowed
        Span<int> a = stackalloc int[3] { 1, 2, 3 };
        Span<int> a = stackalloc int[] { 1, 2, 3 };
        Span<int> a = stackalloc[] { 1, 2, 3 };
    }

    public void TupleEquality()
    {
        (int, (int, int)) t1, t2;
        var res = t1 == (1, (2, 3));
    }
}

namespace CSharp80
{
	class CSharp80ExceptInterfaceDefaultImplement
	{
		void ReferenceNullable()
		{
			var? x = E;
			x!.ToString();
			string? wtf = null;
			int?[]? hello;
		}

		void Patterns()
		{
			if (o is string { Length: 5 } s) Do();
			
			return lang.CountOfTokens switch
			{
				1 => 100,
				2 => 200,
				_ => throw new global::System.Exception()
			};

			var newState = (GetState(), action, hasKey) switch
			{
				(DoorState.Closed, Action.Open, _) => DoorState.Opened,
				(DoorState.Opened, Action.Close, _) => DoorState.Closed,
				(DoorState.Closed, Action.Lock, true) => DoorState.Locked,
				(DoorState.Locked, Action.Unlock, true) => DoorState.Closed,
				(var state, _, _) => state
			};
		}

		async Task AsyncStreams()
		{
			await foreach (var item in asyncEnumerables)
			{
			}
		}

		void Ranges()
		{
			var thirdItem = list[2];                // list[2]
			var lastItem = list[^1];                // list[Index.CreateFromEnd(1)]
			var multiDimensional = list[3, ^2];     // list[3, Index.CreateFromEnd(2)]

			var slice1 = list[2..^3];               // list[Range.Create(2, Index.CreateFromEnd(3))]
			var slice2 = list[..^3];                // list[Range.ToEnd(Index.CreateFromEnd(3))]
			var slice3 = list[2..];                 // list[Range.FromStart(2)]
			var slice4 = list[..];                  // list[Range.All]
			var multiDimensional = list[1..2, ..];  // list[Range.Create(1, 2), Range.All]
		}

		void UsingDeclarators()
		{
			using var item = new FileStream("./.f");
			fixed char* ch = "hell";
			item.Dispose(); // no!
		}

		void StaticLocalFunction()
		{
			static unsafe void Func1() {}
			static unsafe void Func1() {}
			async static void Func2() {}
			static async void Func2() {}
		}

		void NullCoalescingAssignment()
		{
			var item = a ??= b ??= c ??= d ??= throw new Exception();
		}

		public readonly float Hello()
		{
			return 0.1f;
		}
	}

	interface IA
	{
		void M() { WriteLine("IA.M"); }
	}

	interface IA
	{
		void M() { WriteLine("IA.M"); }
	}

	interface IB : IA
	{
		override void IA.M() { WriteLine("IB.M"); } // explicitly named
	}

	interface IC : IA
	{
		override void M() { WriteLine("IC.M"); } // implicitly named
	}
}
