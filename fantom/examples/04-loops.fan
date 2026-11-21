using sys;

class Loops
{
  static Void countUp()
  {
    for (Int i := 0; i < 10; i++)
    {
      if (i == 5) { continue; }
      if (i == 8) { break; }
      echo(i);
    }
  }

  static Void spin()
  {
    Int n := 0;
    while (n < 3)
    {
      echo(n);
      n++;
    }
  }
}
