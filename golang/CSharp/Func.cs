using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Func
{
    public ulong Entry { get; set; }
    public Sym Sym { get; set; }
    public ulong End { get; set; }
    public List<Sym> Params { get; set; } = new List<Sym>();
    public List<Sym> Locals { get; set; } = new List<Sym>();
    public int FrameSize { get; set; }
//    public LineTable LineTable { get; set; }
    public Obj Obj { get; set; }
}
