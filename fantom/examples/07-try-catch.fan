using sys;

class ErrorHandling
{
  static Void run()
  {
    try
    {
      throw Err("boom");
    }
    catch (Err e)
    {
      echo("caught: " + e.msg);
    }
    finally
    {
      echo("cleanup");
    }
  }
}
