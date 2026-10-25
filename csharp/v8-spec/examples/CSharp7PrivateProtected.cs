// C# 7.2: private protected access modifier.
// Grammar rule exercised: accessor_modifier (PRIVATE PROTECTED).
using System;

class Animal
{
    // C# 7.2: private protected field and property —
    // accessible only within this class and derived classes in the same assembly.
    private protected string _name;

    private protected string Name
    {
        get => _name;
        set => _name = value ?? throw new ArgumentNullException(nameof(value));
    }

    public Animal(string name) => _name = name;
}

class Dog : Animal
{
    private string _breed;

    public Dog(string name, string breed) : base(name)
    {
        _breed = breed;
    }

    // _name and Name are accessible here: same assembly, derived class.
    public void Rename(string newName) => Name = newName;

    public override string ToString() => $"{_name} ({_breed})";
}

class CSharp7PrivateProtected
{
    static void Main()
    {
        var d = new Dog("Rex", "Labrador");
        Console.WriteLine(d);       // Rex (Labrador)
        d.Rename("Max");
        Console.WriteLine(d);       // Max (Labrador)
    }
}
