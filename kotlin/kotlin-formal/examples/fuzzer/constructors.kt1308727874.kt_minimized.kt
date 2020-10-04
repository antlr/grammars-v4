fun box(): String {
assertEquals(A::class.java, ((::A))!!!!.returnType.javaType)
}