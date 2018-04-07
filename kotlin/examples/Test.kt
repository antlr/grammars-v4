/**
 * Made of test data from https://github.com/JetBrains/kotlin/tree/master/compiler/testData/psi
 */

//AnonymousInitializer.kt
class Foo {

    init {
        foo()
        val c = f
    }

}


//BabySteps.kt
class Runnable<a,a>(a : doo = 0) : foo(d=0), bar by x, bar {

}


//ByClauses.kt
class A : b by a {
    companion object {}
}
class A : b by a + b() * 5 {
    companion object {}
}
class A : b by (a) {
    companion object {}
}
class A : b by (a {}) {
    companion object {}
}
class A : b by a[a {}] {
    companion object {}
}
class A : b by a(a {}) {
    companion object {}
}
class A : b by object {
    fun f() = a {}
} {
    companion object {}
}


//CallsInWhen.kt
fun foo() {
    when (a) {
        a.foo -> a
        a.foo() -> a
        a.foo<T> -> a
        a.foo<T>(a) -> a
        a.foo<T>(a, d) -> a
        a.{bar} -> a
        a.{!bar} -> a
        a.{ -> !bar} -> a
        else -> a
    }
}


//CallWithManyClosures.kt
val a = f() {} {} {}
val a = f {} {} {}
val a = f {}
val a = f() {}
val a = (f) {} {} {}
val a = (f)() {} {} {}
val a = (f)<A>() {} {} {}


//CollectionLiterals.kt
fun test() {
    []
    [1]
    [1, 2]
    [[]]
    [[1]]
    [1, []]
    [[], 1]
    [[], [1, []]]
    [1,
    2]
    [1,
    [2]]
}


//CommentsBindingInLambda.kt
val la1 = {
    // start
    // start 1
    foo()

    // middle

    foo()

    // end
}

val la2 = {
    /**/
}

val la3 = {
    /** */
}

val la4 = {
    /** Should be under block */

    /** Should be under property */
    val some = 1
}

val la5 = {
    /** */
    /** */
}

val la6 = /*1*/ {/*2*/ a /*3*/ -> /*4*/
}

val la7 = {/**/}

fun foo() {}


//CommentsBindingInStatementBlock.kt
fun test() {
    if (true) {/*start-end*/}

    if (true) {
        /*start-end*/
    }

    if (true) {
        /*start*/
        /*end*/
    }

    if (true) {
        /*start*/

        /** doc */
        val a = 12

        /*end*/
    }
}


//Constructors.kt
class foo {
}

public class foo() : Bar
protected class foo private () : Bar
private class foo<T>() : Bar
internal class foo<T> private () : Bar


//destructuringInLambdas.kt
fun foo() {
    a1.filter { (x, y) -> }
    a2.filter { (x) -> }
    a3.filter { z, (x, y) -> }
    a4.filter { (x, y), z -> }
    a5.filter { q, (x, y), z -> }
    a6.filter { (x, y), (z, w) -> }

    a7.filter { (x, y): Type, (z: Type), (w, u: T) : V -> foo7() }
}


//DoubleColonWhitespaces.kt
fun tests() {
    a:: b
    a ::b
    a :: b

    a?:: b
    a ?::b
    a ?:: b
    a? ::b
    a ? :: b
    a ? ? :: b
}

fun breakLine() {
    a?
    ::b
}


//DynamicReceiver.kt
fun dynamic.foo()
fun dynamic?.foo()
val dynamic.foo: Int
val dynamic?.foo: Int

val foo: dynamic.() -> Unit

// testing look-ahead with comments and whitespace

fun dynamic . foo()
fun dynamic
        .foo()
fun dynamic// line-comment
        .foo()
fun dynamic/*
*/.foo()


//DynamicTypes.kt
fun foo(
        p1: dynamic,
        p2: @a dynamic,
        p3: foo.dynamic,
        p4: dynamic.foo,
        p5: dynamic<T>,
        p6: Foo<dynamic>,
        p7: dynamic?,
        p8: (dynamic) -> dynamic
): dynamic


//EnumCommas.kt
enum class Color {
    NORTH,
    SOUTH,
    WEST,
    EAST,
    ;
}


//EnumEntrySemicolonInlineMember.kt
enum class My {
    FIRST;

    inline fun foo() {}
}


//EnumEntrySemicolonMember.kt
enum class My {
    FIRST;

    fun foo() {}
}


//EnumIn.kt
enum class Foo {
    `in`
}


//EnumInline.kt
enum class My {
    inline
}


//Enums.kt
enum class Color(val rgb : Int) {
    RED(0xFF000),
    GREEN(0x00FF00),
    BLUE(0x0000FF)

    // the end
}


//EnumShortCommas.kt
enum class Color(val rgb : Int) {
    RED(0xFF000),
    GREEN(0x00FF00),
    BLUE(0x0000FF),
    ;
}


//EnumShortWithOverload.kt
enum class Color(val rgb : Int) {
    RED(0xFF000) {
        override fun foo(): Int { return 1 }
    },
    GREEN(0x00FF00) {
        override fun foo(): Int { return 2 }
    },
    BLUE(0x0000FF) {
        override fun foo(): Int { return 3 }
    };

    abstract fun foo(): Int
}


//EOLsInComments.kt
fun foo() {
    a
    + b
    a
    /** */+ b
    a
    /* */+ b
    a /*
  */  + b
    a
    /*
    */  + b
    a /**
     */  + b
    a //
    + b
    a //
    + b
}


//EOLsOnRollback.kt
fun foo() {
    class foo
    fun foo()
    class foo

    typealias x = t
    var r

    @a var foo = 4

    1
    @a val f
}


//ExtensionsWithQNReceiver.kt
val java.util.Map<*,*>.size : Int

fun java.util.Map<*,*>.size() : Int = 1


//FloatingPointLiteral.kt
val array = array<Any>(
        1,
        1.0,
        1e1,
        1.0e1,
        1e-1,
        1.0e-1,
        1F,
        1.0F,
        1e1F,
        1.0e1F,
        1e-1F,
        1.0e-1F,
        1f,
        1.0f,
        1e1f,
        1.0e1f,
        1e-1f,
        1.0e-1f,
        .1_1,
        3.141_592,
        1e1__3_7,
        1_0f,
        1e1_2f,
        2_2.0f,
        .3_3f,
        3.14_16f,
        6.022___137e+2_3f
)


//FunctionCalls.kt
fun foo() {
    f(a)
    g<bar>(a)
    h<baz>
    (a)
    i {s}
    j;
    {s}
    k {
        s
    }
    l(a) {
        s
    }
    m(a);
    {
        s
    }
    n<a>(a) {
        s
    }
    o<a>(a);
    {
        s
    }
    p(qux<a, b>)
    q(quux<a, b>(a))
    r(corge<a, 1, b>(a))
    s(grault<a, (1 + 2), b>(a))
    t(garply<a, 1 + 2, b>(a))
    u(waldo<a, 1 * 2, b>(a))
    v(fred<a, *, b>(a))
    w(plugh<a, "", b>(a))
    xyzzy<*>()
    1._foo()
    1.__foo()
    1_1._foo()
    1._1foo()
    1._1_foo()
}


//FunctionLiterals.kt
fun foo() {
    {}

    {foo}

    {a -> a}

    {x, y -> 1}

    {a: b -> f}
    {a: b, c -> f}
    {a: b, c: d -> f}
    {a: (Int) -> Unit, c: (Int) -> Unit -> f}

    //{a: ((Int) -> Unit) ->} todo
    //{[a] a: A -> }
}


//FunctionsWithoutName.kt
fun ()
fun T.()
fun T.(a : foo) : bar
fun T.<T : (a) -> b>(a : foo) : bar

fun ();
fun T.();
fun T.(a : foo) : bar;
fun T.<T : (a) -> b>(a : foo) : bar;

fun () {}
fun @[a] T.() {}
fun @[a] T.(a : foo) : bar {}
fun @[a()] T.<T :  (a) -> b>(a : foo) : bar {}
fun @[a()] T.<T : @[a]  (a) -> b>(a : foo) : bar {}

fun A?.() : bar?
fun A? .() : bar?


//FunctionTypes.kt
typealias f =  (@[a] a) -> b
typealias f =  (a) -> b
typealias f =  () -> @[x] b
typealias f =  () -> Unit

typealias f =  (a : @[a] a) -> b
typealias f =  (a : a) -> b
typealias f =  () -> b
typealias f =  () -> Unit

typealias f =  (a : @[a] a, foo, x : bar) -> b
typealias f =  (foo, a : a) -> b
typealias f =  (foo, a :  (a) -> b) -> b
typealias f =  (foo, a :  (a) -> b) ->  () -> Unit

typealias f =  T.() -> Unit
typealias f =  T.T.() -> Unit
typealias f =  T<A, B>.T<x>.() -> Unit

typealias f = @[a]  T.() -> Unit
typealias f = @[a]  T.T.() -> Unit
typealias f = @[a]  T<A, B>.T<x>.() -> Unit


//IfWithPropery.kt
val a = if(1) {var f = a;a} else {null}
val a = if(1) {
    var f = a;
    a
} else {null}


//Inner.kt
class Outer {
    inner class Inner
}


//Interface.kt
interface Foo {
    fun f()
    val a
}


//LocalDeclarations.kt
fun foo() {
    out
    1
    @a abstract class foof {}
    abstract @a class foof {}

    out val foo = 5
    @a var foo = 4
    typealias f =  T.() -> Unit
}


//ModifierAsSelector.kt
// JET-1

val z = System.out

fun foo() { throw Exception(); }


//NamedClassObject.kt
class A {
    companion object Companion

    companion object B

    companion object C {}

    companion object

    object C
}


//NewLinesValidOperations.kt
fun test() {
    val str = ""

    str

            .length

    str

            ?.length

    str

            as String

    str

            as? String

    str

            ?: foo

    true

            || false


    false

            && true
}


//NotIsAndNotIn.kt
fun test() {
    a !is B
    a !in B
    !isBoolean(a)
    !inRange(a)
}


//ObjectLiteralAsStatement.kt
fun main(args : Array<String>) {
    object : Thread() {
    }.run()

    object {
    }
}


//PropertyInvokes.kt
fun foo() {
    1._some
    1.__some
    1_1._some
    1._1some
    1._1_some
}


//QuotedIdentifiers.kt
@`return` fun `package`() {
    `class`()
}

class `$`
class `$$`
class ` `
class `1`


//SemicolonAfterIf.kt
fun foo(a: Int): Int { var x = a; var y = x++; if (y+1 != x) return -1; return x; }


//SimpleClassMembers.kt
class foo {

    class foo {
        object foo {

        }

        class Bar {}

        fun foo()

        val x

        var f

        typealias foo = bar
    }

    class Bar {
        object foo {
            companion object {

            }

            private companion object {

            }

            private companion object : Fooo {

            }

            private companion object : Fooo, Bar by foo {

            }

            private companion object : Fooo, Bar by foo, Goo()
        }

        class Bar {}

        fun foo()

        val x

        var f

        typealias foo = bar
    }

    fun foo()

    val x

    var f

    typealias foo = bar

    companion object {

    }

    private companion object {

    }

    private companion object : Fooo {

    }

    private companion object : Fooo, Bar by foo {

    }

    private companion object : Fooo, Bar by foo, Goo()


}


//SimpleExpressions.kt
fun a(
        a : foo = Unit,
        a : foo = 10,
        a : foo = 0x10,
        a : foo = '1',
        a : foo = "dsf",
        a : foo = """dsf""",
        a : foo = 10.0,
        a : foo = 10.dbl,
        a : foo = 10.flt,
        a : foo = 10.0.dbl,
        a : foo = 10.lng,
        a : foo = true,
        a : foo = false,
        a : foo = null,
        a : foo = this,
        a : foo = super<sdf>,
        a : foo = (10),
        a : foo = Triple(10, "A", 0xf),
        a : foo = Foo(bar),
        a : foo = Foo<A>(bar),
        a : foo = Foo(),
        a : foo = Foo<bar>(),
        a : foo = object : Foo{},
        a : foo = throw Foo(),
        a : foo = return 10,
        a : foo = break,
        a : foo = break@la,
        a : foo = continue,
        a : foo = continue@la,
        a : foo = 10,
        a : foo = 10,
        a : foo = 10,
        a : foo = 10,
        a : foo = 0xffffffff.lng
) {
    return 10
    return
    10
    break
    la@
    break@la
    continue
    la@
    continue@la
}


//SimpleModifiers.kt
abstract
open
open
annotation
override
open
abstract
private
protected
public
internal
in
out
class Bar<abstract
open
enum
open
annotation
override
open
abstract
private
protected
public
internal
in
out
T> {
    val abstract
    val open
    val enum
    val open
    val annotation
    val override
    val open
    val abstract
    val private
    val protected
    val public
    val internal
    val lazy
}


//SoftKeywords.kt
public protected private internal
abstract
open
open
annotation
override
open
abstract
private
protected
public
internal
in
out
suspend
class Bar<abstract, abstract enum : T, out open,
        public protected private internal abstract
        open
        enum
        open
        annotation
        override
        open
        abstract
        private
        protected
        public
        internal open
        > (a : B) : A by b {
    public protected private internal val abstract
    val open
    val enum
    val open
    val annotation
    val override
    val open
    val abstract
    val private
    val protected
    val public
    val internal
    val lazy
    val wraps
    val import
    val where
    val by
    val get
    val set
    val public
    val private
    val protected
    val internal
    val field
    val property
    val receiver
    val param
    val setparam
    val lateinit
    val const
    val suspend
    val coroutine
        get() = a
        set(S : s) {}


    public protected private internal
    fun abstract  () : abstract
    fun open   () : open
    fun enum      () : enum
    fun open      () : open
    fun annotation () : annotation
    fun override  () : override
    fun open   () : open
    fun abstract  () : abstract
    fun private   () : private
    fun protected () : protected
    fun public    () : public
    fun internal  () : internal
    fun lazy      () : lazy
    fun wraps     () : wraps
    fun import    () : import
    fun where     () : where
    fun by        () : by
    fun get       () : get
    fun set       () : set
    fun public    () : public
    fun private   () : private
    fun protected () : protected
    fun internal  () : internal
    fun field  () : field
    fun property  () : property
    fun receiver  () : receiver
    fun param  () : param
    fun setparam  () : setparam
    fun lateinit  () : lateinit
    fun const  () : const

    fun test(
            abstract  : t,
            open   : t,
            enum      : t,
            open      : t,
            annotation : t,
            override  : t,
            open   : t,
            abstract  : t,
            private   : t,
            protected : t,
            public    : t,
            internal  : t,
            lazy      : t,
            wraps     : t,
            import    : t,
            where     : t,
            by        : t,
            get       : t,
            set       : t,
            public    : t,
            private   : t,
            protected : t,
            internal  : t,
            field     : t,
            property  : t,
            receiver  : t,
            param     : t,
            setparam    : t,
            lateinit  : t,
            const     : t,
            public protected private internal abstract
            open
            enum
            open
            annotation
            override
            open
            abstract
            private
            protected
            public
            internal open : t

    )

}


class F(val foo : bar,
        abstract  : t,
        open   : t,
        enum      : t,
        open      : t,
        annotation : t,
        override  : t,
        open   : t,
        abstract  : t,
        private   : t,
        protected : t,
        public    : t,
        internal  : t,
        lazy      : t,
        wraps     : t,
        import    : t,
        where     : t,
        by        : t,
        get       : t,
        set       : t,
        public    : t,
        private   : t,
        protected : t,
        internal  : t,
        field     : t,
        property  : t,
        receiver  : t,
        param     : t,
        setparam    : t,
        lateinit  : t,
        const     : t,
        public protected private internal abstract
        open
        enum
        open
        annotation
        override
        open
        abstract
        private
        protected
        public
        internal open : b
) {

}


//SoftKeywordsInTypeArguments.kt
class Foo<out abstract, out out> {}

fun f() {

//  Foo<out out>
    Foo<out Int>

}


//TraitConstructor.kt
interface TestTrait(val a: Int, var b: String, c: Double)
interface TestTrait()


//TypeAlias.kt
typealias foo = bar
typealias foo<T> = bar
typealias foo<T : foo> = bar
typealias foo<A, B> = bar
typealias foo<A, B : A> = bar

typealias foo = bar ;
typealias foo<T> = bar ;

typealias foo<T : foo> = bar ;
typealias foo<A, B> = bar ;
typealias foo<A, B : A> = bar ;


//TypeConstraints.kt
class foo<T> where T : T {

}


//TypeModifiers.kt
val p1: suspend a
val p2: suspend (a) -> a
val p3: suspend (a) -> suspend a
val p4: suspend a.() -> a
val p4a: @a a.() -> a
val p5: (suspend a).() -> a
val p5a: (@a a).() -> a
val p6: a<in suspend a>
val p7: a<out suspend @a a>
val p8: a<out @a suspend @a a>
val p9: a<out @[a] suspend @[a] a>
val p10: suspend a<a>
val p11: suspend @a a
val p12: @a suspend a
val p13: @a suspend @a a
val p14: @[a] suspend @[a] a
val p15: suspend (suspend (() -> Unit)) -> Unit

@a fun @a a.f1() {}
fun (@a a.(a) -> a).f2() {}