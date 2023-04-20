@Target(AnnotationTarget.CLASS, AnnotationTarget.TYPE_PARAMETER, AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
@Retention(AnnotationRetention.SOURCE)
annotation class A<T>{}

@Target(AnnotationTarget.CLASS, AnnotationTarget.TYPE_PARAMETER, AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
@Retention(AnnotationRetention.SOURCE)
annotation class A2{}

interface DI<T>{
    @Target(AnnotationTarget.CLASS, AnnotationTarget.TYPE_PARAMETER, AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
    @Retention(AnnotationRetention.SOURCE)
    annotation class A<T>{}

    interface EEI{
        @Target(AnnotationTarget.CLASS, AnnotationTarget.TYPE_PARAMETER, AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
        @Retention(AnnotationRetention.SOURCE)
        annotation class A<T,S>{}
    }
}

interface AI1{}
interface AI2{}
interface AI3{}
class C<T> : @A<T> @A2 AI1, @DI.A<T> AI2, @DI.EEI.A<T, T> AI3 {}
