using sys;

class Animal
{
  Str name;

  new make(Str name)
  {
    this.name = name;
  }
}

class Dog : Animal
{
  Str breed;

  new make(Str name, Str breed) : super.make(name)
  {
    this.breed = breed;
  }

  new makeDefault() : this.make("Rex", "Mutt")
  {
  }
}
