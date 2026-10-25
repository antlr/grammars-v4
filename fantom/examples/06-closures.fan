using sys;

class Closures
{
  static Void run()
  {
    add := |Int a, Int b -> Int| { return a + b; };
    echo(add.call(2, 3));

    greet := |Str name| { echo("hi " + name); };
    greet.call("world");
  }
}
