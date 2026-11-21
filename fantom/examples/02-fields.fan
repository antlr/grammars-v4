using sys;

class Point
{
  Int x := 0;
  Int y := 0;

  Str label := "origin"
  {
    get { return &label; }
    private set;
  };

  Int id
  {
    get { echo("get id"); return &id; }
    set { &id = it; }
  };
}
