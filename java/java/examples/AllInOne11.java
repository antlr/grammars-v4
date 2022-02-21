package var.var;

import java.lang.annotation.*;
import java.util.ArrayList;
import java.util.List;

import var.var.base.T;

class Tests {

    class AC implements AutoCloseable {

        @Override
        public void close() throws Exception {

        }
    }

    class TT extends var.var.base.T implements var.var.base.I {

        <T extends var.var.base.I> void f() {
        }

        @Override
        public void f1() {

        }

        void var() {
            var:
            for (; ; ) {
                break var;
            }
        }
    }

    interface II {
        void var();
    }

    enum E1 {
        var(1);

        E1(int i) {

        }
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.METHOD})
    public @interface TM1 {
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.TYPE})
    public @interface TM2 {
    }

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.MODULE, ElementType.TYPE_USE})
    public @interface TM3 {
    }

    static class PF {
        protected int p;
        static int sp;
        static int var;
    }

    static class CCC {
        static int var = 0;

        static int var() {
            return 0;
        }
    }

    {
        final var c = 1;
        PF.sp = c;
        T t;
        var var = 1;
        var = var + var;
        var b = (byte) var;
        var r = var();
        var v = CCC.var;
        var n = CCC.var() + CCC.var;
        CI4 ci4 = new CI4();
        I4 i4a = ci4::var;
        E1 e = E1.var;
        switch (e) {
            case var:
                break;
        }
    }

    {
        int var, i = 1;
    }

    {
        var var = new PF();
        var.var = 1;
    }

    private int var = 1;

    int var() {
        return 0;
    }

    static {
        final var c = 1;
        PF.sp = c;
    }

    {
        var var = "var";
        var module = "module";
        var open = "open";
        var requires = "requires";
        var exports = "exports";
        var opens = "opens";
        var to = "to";
        var uses = "uses";
        var provides = "provides";
        var with = "with";
        var transitive = "transitive";
    }

    void foo() {
        var f = 1;
    }

    void foo2() throws Exception {
        try (var a = new AC()) {

            for (final var i = 1; ; ) {
                break;
            }

            for (final var var = 1; ; ) {
                break;
            }

            List<Integer> lst = new ArrayList<>();
            for (final var item : lst) {

            }

            for (final var var : lst) {

            }
        }
    }

    void foo3() throws Exception {
        var a = new AC();
        try (a) {

        }
    }

    void foo31() throws Exception {
        var var = new AC();
        var a = new AC();
        try (a; var) {

        }
    }

    void foo4() {

        I1 i1a = (int i) -> 1;
        I1 i1b = (var var) -> 1;
        I1 i1c = var -> 1;

        I2 i2a = (final int var, int var1) -> 1;
        I2 i2b = (final var var, var j) -> 1;
    }

    interface I1 {
        int m(int i);
    }

    interface I2 {
        int m2(int i, int j);
    }

    interface I3 {
        void m0();

        default void m1() {
        }

        private void m2() {
        }

        private <T> void gm2(T t) {
        }

        private static void m3() {
        }

        static void m4() {
        }
    }

    interface I4 {
        int var();
    }

    interface I6 {
        default <TT,UU> void var() {
        }
    }

    class CI4 implements I4 {

        @Override
        public int var() {
            return 0;
        }
    }

    class CI6 implements I6 {

        public void vartest() {
            I6.super.var();
            I6.super.<Byte, Long>var();
        }
    }

    class D extends CI4 {

        public int var() {
            return super.var();
        }

        void f(int var) {

        }
    }

    static class Annos {
        @interface InnerAnnotation {
            int var() default 0;
        }

        @interface OuterAnnotation {
            InnerAnnotation var() default @InnerAnnotation(var = 1);
        }
    }

    static class TH{

        void var(){}
        int var = 1;

        TH(){
            this.var();
            this.var = 1;
            System.out.println(var);
        }
    }
}
