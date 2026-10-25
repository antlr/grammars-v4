using sys;

class Calc
{
  static Int max(Int a, Int b)
  {
    if (a > b) { return a; }
    else { return b; }
  }

  static Str classify(Int n)
  {
    if (n < 0) { return "negative"; }
    else if (n == 0) { return "zero"; }
    else { return "positive"; }
  }
}
