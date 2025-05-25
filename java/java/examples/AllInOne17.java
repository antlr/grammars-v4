package var.var.sealed;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

@interface Dummy {
}

@interface Dummy2 {
}

@Target({ElementType.TYPE, ElementType.TYPE_USE})
@interface Dummy3 {
}

/**
 * https://openjdk.java.net/jeps/361
 */
class SwitchExpressions {

    final static private int C = 10;

    static class SC1 {
        final static int C = 100;
    }

    enum E1 {
        ONE;
    }

    int fn1(int n) {
        final int k = 4;
        var r = switch (n) {
            case 1, 2, 3 + 3, k, C, SC1.C -> 3 + SC1.C;
            case 20 -> 3 + 4 + C - k;
            case 21 -> {
                int ff = 222;
                yield ff;
            }
            case 22 -> {
                yield 33 + 3;
            }
            case 99 -> {
                throw new RuntimeException("");
            }
            default -> 0;
        };
        return r;
    }

    String fn2(String s) {
        return switch (s) {
            //case null -> "n";
            case "a" -> "";
            case "b", "c" -> "a";
            default -> "o";
        };
    }

    int fn3(final int var) {
        return switch (var) {
            case 1 -> 2;
            default -> var;
        };
    }

    void fn4() {

        fn1(switch (1) {
            case 1 -> 0;
            case 2 -> 2;
            default -> 1;
        });
    }

    int fn5() {
        E1 e = E1.ONE;
        return switch (e) {
            case ONE -> 0;
            //default -> 1;
        };
    }

    void fn6() {
        switch (1) {
            case 1 -> {

            }
        }
    }

    void fn7() {
        switch (1) {
            case 1 -> {
            }
            case 2 -> {
            }
        }
    }

    void fn8() {
        var i = 1;
        switch (1) {

        }
        var f = 2;
        switch (2) {
            case 2 -> {
                f = 3;
            }
        }
    }

    void fn9(String s) {
        switch (s) {
            case "" -> {
            }
            default -> {
            }
        }
    }

    void fn10() {
        var i = switch (1) {
            case 1 -> switch (2) {
                case 2 -> 0;
                default -> 2;
            };
            default -> 2;
        };
    }

    void fn11() {
        switch (1) {
            case 1 -> throw new RuntimeException("");
        }
    }

    int fn12() {
        var v = 1;
        int n = switch (1) {
            case 1:
                var g = 1;
                System.out.println();
                yield v;
            default:
                yield 3;
        };
        return n;
    }

    void fn13() {
        int n;
        switch (1) {
            case 1 -> n = 1;
        }
    }

    void fn14() {
        switch (1) {
            default -> {
            }
        }

        var n = 1;
        var m = switch (n) {
            case 1 -> 2;
            case 2 -> 2;
            default -> 1;
        };

        m = switch (n) {
            case 2:
                yield 2;
            default:
                yield 3;
        };


    }
}

/**
 * https://openjdk.java.net/jeps/394
 */
class PatternMatching4instanceof {

    void fn1(Number n) {
        if (n instanceof Long var) {
            var v = var;
        } else if (n instanceof Integer open) {
            var v = open;
        } else if (n instanceof Byte) {
            //
        } else {
            throw new RuntimeException("");
        }

        if (!(n instanceof Long l)) ;

        if (n instanceof final @Dummy @Dummy2 Long l && l.byteValue() == 1
                || n instanceof @Dummy @Dummy2 final Byte b && b.intValue() == 1) ;

        if (n instanceof Long) ;
        if (n instanceof Long var) ;
        if (n instanceof Long l) ;
        if (n instanceof final Long l) ;
        if (n instanceof @Dummy Long l) ;
        if (n instanceof @Dummy @Dummy2 Long l) ;
        if (n instanceof final @Dummy Long l) ;
        if (n instanceof final @Dummy @Dummy2 Long l) ;
        if (n instanceof @Dummy final Long l) ;
        if (n instanceof @Dummy @Dummy2 final Long l) ;
    }
}

/**
 * https://openjdk.java.net/jeps/406
 */
class PatternMatching4switchExp {

    void f(int i) {
    }

    void f1(Object obj) {
        switch (obj) {
            case null -> f(0);
            case String s -> f(1);
            case int[] a -> f(2);
            default -> f(-1);
        }
    }

    void f2(Object obj) {
        switch (obj) {
            case null -> f(0);
            case Long l -> f(1);
            case Integer i -> f(1);
            case int[] a -> f(2);
            default -> f(-1);
        }
    }

    void f3(Object o) {
        switch (o) {
            case null:
            case Long l:
                f(0);
                break;
            default:
                break;
        }
    }

    enum E1 {
        var;
    }

    void f4() {
        var var = E1.var;
        switch (var) {
            case var:
                return;
            default:
                break;
        }

        switch (var) {
            case var -> {
            }
            default -> {
            }
        }
    }

    int f5(Number n) {
        return switch (n) {
            case Long l && l.intValue() == 1 && l.byteValue() == 1 -> l.byteValue();
            case Long var -> var.byteValue();
            case Integer i -> i.byteValue();
            default -> throw new RuntimeException("");
        };
    }

    Function<Integer, String> f6(Object obj) {
        boolean b = true;
        return switch (obj) {
            case String var && b -> t -> var;
            default -> t -> "Default string";
        };
    }

    int dummy() {
        return 0;
    }

    Function<Integer, String> f7(Object obj) {
        boolean b = true;
        boolean b2 = true;
        boolean b3 = true;
        return switch (obj) {
            case (((String s) && (b && b2)) && s.length() > 0 && dummy() == 1) -> t -> s;
            case (((Integer i && b && b2) && (b && b2)) && b3 && (b && b2)) -> t -> "";
            case (((Integer i && b && b2) && (b && b2)) && b3 && (b && b2 && !b3)) -> {
                yield t -> "";
            }
            case final Long l && (b ? b2 : b3) -> {
                yield t -> "";
            }
            default -> t -> "Default string";
        };
    }

    void f8(Object o, int i) {
        switch (i) {
            case 1, 2:
            case 3, 4: {
            }
        }

        switch (o) {
            case Number b: {
            }
            default: {
            }
        }

        var f = switch (o) {
            case final I2 l: {
                yield switch (o) {
                    case Byte b -> 1;
                    default -> 0;
                };
            }
            default: {
                yield 1;
            }
        };
    }
}

/**
 * https://openjdk.java.net/jeps/395
 */
class Records {

    interface I1 {

    }

    static record R0(int x) {
        R0 {
            if (x > 3) throw Exception("new", null);
            x *= 3;
        }
    }

    final record R1(@Dummy2 @Dummy int x) {

        R1(int x) {
            this.x = x;
        }

        enum E {
            ONE;

            record ER() {

            }
        }

        class C {
            record CR() {

            }
        }

        interface I {
            record IR() {

            }
        }

        final static private record R() implements I1 {
        }

        final static protected record R2() implements I1 {
        }

        final static public record R3() implements I1 {
        }

        final static record R4() implements I1 {
        }
    }

    record R2() {
        public @interface TM1 {
            record AR() {

            }
        }
    }

    record R3<T>(int x, T y) {
    }

    record R4<T>(int x, T y) implements I1 {

    }

    void fn1() {
        final record Pt<T, G extends Number>(int x, int y) implements I1, R1.I {
            void fn(T t) {
            }

            <TT> void f() {
            }

            //final int x; implicitly defined

            Pt(int x, int y) {
                this.x = x;
                this.y = y;
            }

            //private int c = 1; not allowed
            private final static int C = 1; //allowed

            static class C {

            }
        }

        Pt<Long, Long> p = new Pt<>(1, 2);
        p.fn(1L);


    }

}

/**
 * https://openjdk.java.net/jeps/378
 */
class TextBlocks {

    void f(String s) {
    }

    void fn() {
        var s = """
                a \t
                \r""";

        var s2 = """
                a""" + """
                b""";

        var s3 = """
                """;

        f("""
                a""");

        f("""
                """);
    }

}

/**
 * https://openjdk.java.net/jeps/409
 */
class SealedClasses {

    interface I1 {
    }

    class C0 {
    }

    sealed class SC1 extends C0 implements I1 permits FC1, FC2 {

    }

    sealed class SC2 {
        void f() {
            var non = 1;
            var sealed = 2;
            var ns = non - sealed;
            var permits = 1;
            var record = 1;
        }
    }

    final class FC1 extends SC1 {

    }

    final class FC2 extends SC1 {

    }

    non-sealed class NSC1 extends SC2 {

    }

    class C1 extends NSC1 {

    }
}

class Ids {
    class oo {

        class opens<T> {

            enum E {
                provides;
            }

            class provides<S> {

                void f() {

                    opens<Byte>.provides<Long> b1 = new opens<>().new provides<>() {
                    };
                    opens<Byte>.provides<Long> b2 = new opens().new provides() {
                    };
                }

                void g() {
                    E e = E.provides;
                    switch (e) {
                        case provides:
                            break;
                    }
                }

                <T> Object var() {
                    return null;
                }

                provides<Long> get() {
                    return null;
                }

                class with<S> {

                }

                static class SS<R> {
                    interface Sup<T> {
                        T get();
                    }
                }

                void h() {
                    var o = get().<Long>var();

                    SS.Sup<provides<Long>.with<Long>> s = @Issue1897.Dum1 provides<Long>.with<Long>::new;
                }

                class R {

                    <to> void f() {
                    }
                }
            }


        }
    }

    static class opens {
        enum requires {
            opens;

        }

        public static <T> void with(String s) {

        }

        interface with {
            default void f() {
            }
        }

        class exports implements with {
            void g() {
                with.super.f();
            }
        }

        @interface to {

        }

        class module {
            public static <T> void with(String s) {
                try {

                } catch (Exception var) {

                }
            }
        }

        record provides(int to) {

            void f() {

                opens o = new opens();
                BiFunction<Long, Long, Long> b = (opens, with) -> 1L;
                Consumer<String> c = opens.module::<Byte>with;
            }
        }
    }

}

class Yield {

    int f(Object o) {

        final var yield = 1;
        return switch (o) {
            case Long l -> {
                //var yield = 1;
                yield yield;
            }
            default -> {
                yield yield;
            }
        };
    }

    int yield(int yield){
        return yield;
    }
}


class IF_PERMITS {
    final class T1 implements I1 {

    }

    final class T2 implements I1 {

    }

    interface I2 {
    }

    sealed interface I1 extends I2 permits T1, T2 {

    }

}