extends Object

func f():
    var a = 0
    var b = 1
    var c = 2
    var d = 3
    
    a = b*c+d
    a = b+c-d
    a = b-c<<d
    a = b<<c&d
    a = b&c^d
    a = b^c|d
    a = b|c<d
    a = b<c in d
    a = b in c and d
    a = b and c or d