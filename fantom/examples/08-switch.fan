using sys;

class Switcher
{
  static Str dayName(Int d)
  {
    switch (d)
    {
      case 0:
        return "Sunday";
      case 1:
        return "Monday";
      default:
        return "Other";
    }
    return "Other";
  }
}
