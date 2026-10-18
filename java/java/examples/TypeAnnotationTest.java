class TypeAnnotationTest {
    java.lang.@Foo.Bar Object ok;

    static class Foo {
	@java.lang.annotation.Target(java.lang.annotation.ElementType.TYPE_USE)
	@interface Bar { }
    }
}
