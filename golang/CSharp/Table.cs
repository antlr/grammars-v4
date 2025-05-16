using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Table
{
    public List<Sym> Syms { get; set; } = new List<Sym>();
    public List<Func> Funcs { get; set; } = new List<Func>();
    public Dictionary<string, Obj> Files { get; set; } = new Dictionary<string, Obj>();
    public List<Obj> Objs { get; set; } = new List<Obj>();
//    public LineTable Go12Line { get; set; }
}
