// Generic structs with named specializations, and constructor syntax
// (both positional and named fields).

struct vec2<T>:
    x:T
    y:T

struct int2 = vec2<int>
struct float2 = vec2<float>

def add(a:int2, b:int2) -> int2:
    return int2 { a.x + b.x, a.y + b.y }

def main():
    let p = int2 { 1, 2 }
    let q = int2 { x: 3, y: 4 }
    let r = add(p, q)
    print "r = ({r.x}, {r.y})"

main()
