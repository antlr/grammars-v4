class O{
    i = 1;
}

class Entity{
    name = "test";
    obj = new O();
}

function processEntity(e?: Entity) {
  let s = e!.name;
  let t = e.name;
  let o = e!.obj!.i;
  let p = e?.name;

  let i = p!;
}

var e = null;
processEntity(e);
