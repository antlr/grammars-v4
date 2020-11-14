class MyClass<T> {
  final T a;
  final String b;

  const MyClass({@required this.a, @required this.b});

  @override
  String toString() => "$runtimeType(a: $a, b: \"$b\")";
}
