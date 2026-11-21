using sys;

mixin Greetable
{
  abstract Str greeting();

  Void sayHello()
  {
    echo(greeting());
  }
}

class Person : Greetable
{
  Str name := "Anon";

  override Str greeting()
  {
    return "Hello, " + name;
  }
}
