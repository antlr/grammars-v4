using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Obj
{
    public List<Func> Funcs { get; set; } = new List<Func>();
    public List<Sym> Paths { get; set; } = new List<Sym>();
}

