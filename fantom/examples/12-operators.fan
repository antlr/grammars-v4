using sys;

class Operators
{
  static Void run()
  {
    r := 0..<10;
    Obj o := "hi";
    isStr := o is Str;
    s := (Str) o;
    t := isStr ? "yes" : "no";
    x := null;
    y := x ?: "default";
    d := 100ms;
    u := `https://fantom.org/`;
    echo(r);
    echo(isStr);
    echo(s);
    echo(t);
    echo(y);
    echo(d);
    echo(u);
  }
}
