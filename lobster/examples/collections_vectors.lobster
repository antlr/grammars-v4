// Vector literals, indexing, and a function returning multiple values.

def minmax(v):
    var lo = v[0]
    var hi = v[0]
    for(v) x:
        if x < lo: lo = x
        if x > hi: hi = x
    return lo, hi

def main():
    let v = [5, 3, 8, 1, 9]
    let lo, hi = minmax(v)
    print "min={lo} max={hi}"

main()
