using System.Collections.Generic;

public class Func
{
    public ulong Entry { get; set; }
    public Sym Sym { get; set; }
    public ulong End { get; set; }
    public List<Sym> Params { get; set; } = new List<Sym>();
    public List<Sym> Locals { get; set; } = new List<Sym>();
    public int FrameSize { get; set; }
    public Obj Obj { get; set; }
}
