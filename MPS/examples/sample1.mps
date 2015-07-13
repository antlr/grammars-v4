NAME          example2.mps

ROWS

 N  obj     

 L  c1      

 L  c2      

COLUMNS

    x1        obj                 -1   c1                  -1

    x1        c2                   1

    x2        obj                 -2   c1                   1

    x2        c2                  -3

    x3        obj                 -3   c1                   1

    x3        c2                   1

RHS

    rhs       c1                  20   c2                  30

BOUNDS

 UP BOUND     x1                  40

ENDATA