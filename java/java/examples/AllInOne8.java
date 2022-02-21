// Source: https://en.wikipedia.org/wiki/Java_syntax
// Source: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/index.html

// Lambdas
public class Lambdas {
     public static void main(String[] args) {
        // use predicate composition to remove matching names
        List<Name> list = new ArrayList<>();
        for (Name name : NAMES) {
            list.add(name);
        }
        Predicate<Name> pred1 = name -> "Sally".equals(name.firstName);
        Predicate<Name> pred2 = name -> "Queue".equals(name.lastName);
        list.removeIf(pred1.or(pred2));
        printNames("Names filtered by predicate:", list.toArray(new Name[list.size()]));

        Comparator<Name> com1 = Comparator.comparing((Name name1) -> name1.lastName)
            .thenComparing(name2 -> name2.firstName);
        Comparator<Name> com2 = Comparator.<Name,String>comparing(name1 -> name1.lastName)
            .thenComparing(name2 -> name2.firstName);

        // sort array using lambda expression
        copy = Arrays.copyOf(NAMES, NAMES.length);
        Arrays.sort(copy, (a, b) -> a.compareTo(b));
        printNames("Names sorted with lambda expression:", copy);
     }

}

class LambdaAndCastWithBounds{

    interface I1<T> {
        void fn();
    }

    interface I2 {
        void fn();
    }

    I1<Byte> i1 = (I1<Byte> & Serializable & Cloneable) () -> {
    };

    I2 i2 = (I2 & Serializable & Cloneable) () -> {
    };

}

// Default interface method
interface Formula {
    double calculate(int a);
    
    default double sqrt(int a) {
        return Math.sqrt(a);
    }
}

// Double colon
public class For {
    public void bar() {
        Function<Computer, Integer> getAge = Computer::getAge;
        Integer computerAge = getAge.apply(c1);

        Function<Computer, Integer> getAgeAlt = this::getAge;
        Function<Computer, Integer> getAgeAlt2 = MyClass.this::getAge;
        Function<Computer, Integer> getAgeAlt3 = generate()::getAge;
        Function<Computer, Integer> getAgeAlt4 = MyClass.generate()::getAge;
        Function<Computer, Integer> getAgeAlt5 = MyClass.twice().nested()::getAge;
        Function<Computer, Integer> getAgeAlt6 = twice().nested()::getAge;
        Function<Computer, Integer> getAgeAlt7 = this.singletonInstanceMethod::get;

        autodetect(this.beans, ((AutodetectCapableMBeanInfoAssembler) this.assembler)::includeBean);

        TriFunction <Integer, String, Integer, Computer> c6Function = Computer::new;
        Computer c3 = c6Function.apply(2008, "black", 90);

        Function <Integer, Computer[]> computerCreator = Computer[]::new;
        Computer[] computerArray = computerCreator.apply(5);
    }
}

// Type Annotations
public class Annotations {
    @Valid
    private List<@NotNull String> property;
}

public interface CallableProcessingInterceptor {
    default <T> void beforeConcurrentHandling(NativeWebRequest request, Callable<T> task) throws Exception {
    }
}

@FunctionalInterface
public interface RouterFunction<T extends ServerResponse> {
    default <S extends ServerResponse> RouterFunction<S> filter(HandlerFilterFunction<T, S> filterFunction) {
        return new RouterFunctions.FilteredRouterFunction<>(this, filterFunction);
    }
}

//Instanceof
class InstanceOf{
    public static void main(String[] args){
        if(args instanceof String[]){

        }
        if(args instanceof a.String[]){

        }
    }
}

// Unicode
class Unicode {
    public static void main(String[] args) {
        System.out.println("A = \uuu0041");
    }
}

// More Annotations
public class Annos {
    public @interface Dummy4 {
        String[] value();
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    public @interface Dummy03 {
        Dummy3[] value();
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    public @interface Dummy01 {
        Dummy1[] value();
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    @Repeatable(Dummy01.class)
    public @interface Dummy1 {
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    public @interface Dummy2 {
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    @Repeatable(Dummy03.class)
    public @interface Dummy3 {
    }

    public static @Dummy4("#1")
    @Dummy1 @Dummy3 <T extends @Dummy2 @Dummy3 Object>
    @Dummy1 @Dummy3 T @Dummy1 @Dummy3 [] foo(@Dummy1 T @Dummy2 @Dummy3 [] arr, @Dummy1 @Dummy3 T @Dummy1 @Dummy3 ... t) {
        return (@Dummy1 @Dummy3 T[]) null;
    }

    public static @Dummy4("")
    <@Dummy1 @Dummy2 T extends @Dummy1 @Dummy3 Object>
    @Dummy1 @Dummy3 T @Dummy1 @Dummy3 [] @Dummy1 @Dummy2 [] foo2(@Dummy1 T @Dummy2 @Dummy3 [] @Dummy1 @Dummy3 [] arr) {
        return (@Dummy1 @Dummy2 T[] @Dummy1 @Dummy2 []) null;
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.METHOD})
    public @interface TM1{
    }

    class Gen<T> {    }
    class A<@Dummy1 T extends @Dummy1 Gen<@Dummy1 T> >{}

    public static <@Dummy3 T> void foo3(T t, Gen<@Dummy1 @Dummy3 ? super @Dummy1 @Dummy3 T> c) {
    }

    public static <@Dummy3 T> void foo33(T t, Gen<@Dummy1 @Dummy3 ? > c) {    }

    public static <@Dummy3 T, @Dummy1 S> void foo333(T t, Gen<@Dummy1 @Dummy3 T @Dummy1 [] > c) {    }

    public static @Dummy3 <@Dummy3 T extends @Dummy3 Gen<@Dummy1 ? super @Dummy1 T>> @Dummy3 T @Dummy3 [] f(@Dummy3 T @Dummy3 ... t) {
        return (@Dummy3 T @Dummy3 []) null;
    }

    interface TI1{
        public @TM1 void im01();
        public <@Dummy3 T> @TM1 @TM2 T gim01(@TM1 T t);
    }

    static class Issue2454 {
        interface I {
            default <T> void fn1() {
            }

            default <T, S, U> void fn2() {
            }
        }

        class C implements I {

            public void test() {
                I.super.<Long>fn1(); // fix #2454
                I.super.fn1(); //ok
                I.super.<List<Integer>, Byte, Map<Long, String>>fn2(); // fix #2454
                I.super.fn2(); //ok
            }
        }
    }

}

class Issue1897 {
    @Target({ElementType.TYPE, ElementType.TYPE_USE})
    @interface Dum1 {
    }

    @Target(ElementType.TYPE_USE)
    @interface Dum2 {
    }

    @Target(ElementType.TYPE_USE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface Dum3 {
    }

    abstract class C {
        // https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4.1
        void f(@Dum1 @Dum2 C this) {

        }

        void d(C this, int i, int j) {

        }

        void e(C this, int i, int j, int... k) {

        }

        void c(C this, int... k) {

        }

        abstract void am(C this);

        class D {

            class E {
                E(Issue1897.C.D D.this) {
                }
            }

            class GE {
                <TY> GE(Issue1897.C.D D.this) {

                }
            }

            void b(Issue1897.C.@Dum3 D Issue1897.C.D.this) {

            }

            void b2(Issue1897.C.@Dum3 D this) {
            }

            void b3(@Dum1 D this) {
            }
        }

        void b(Issue1897.C Issue1897.C.this) {

        }
    }
}
