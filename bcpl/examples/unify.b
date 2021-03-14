/*
This is a unification algorithm based on that which Eben Upton
has been working on.

Implemented in BCPL by Martin Richards (c) May 2002
*/

GET "libhdr"

GLOBAL {
  up : ug     // The uplink for variable sets
  size        // The size of the set tree rooted at this point
  id          // Variable name of this term or zero
  new         // New term if this one copied

  termv       // The term, if any, associated with this variable set
  termp  
  termt

  stackv      // Stack of variable pairs
  stackp      // 
  stackt      // 

  spacev      // Free store area
  spacep      // 
  spacet      // 

  tmax        // Highest term number
  vmax        // highest variable number
  opmax       // highest op number

  mk          // mk(op, n, v1,..., vn) make a term
  matchterms
  matchvars
  loadtermpair
  unify
  link        // Link two variable sets
  find        // Find the root of a variable set
  pr
}

MANIFEST {
  Stackupb  =  20000
  Spaceupb  = 100000
  Termupb   =  10000

  Op    = 0    // Term field selectors
  Arity
  Child0=Arity
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("TERMS,VARS,OPS", argv, 50) DO
  { writef("Bad arguments*n")
    RESULTIS 20
  }

  tmax  := argv!0 -> str2numb(argv!0),  6
  vmax  := argv!1 -> str2numb(argv!1),  3
  opmax := argv!2 -> str2numb(argv!2),  2

  writef("tmax=%n  vmax=%n  opmax=%n*n", tmax, vmax, opmax)

  up   := getvec(Termupb)
  size := getvec(Termupb)
  id   := getvec(Termupb)
  new  := getvec(Termupb)
  termv,  termt  := getvec(Termupb),  Termupb+1
  stackv, stackt := getvec(Stackupb), Stackupb+1
  spacev, spacet := getvec(Spaceupb), Spaceupb+1
  spacep := spacet

  UNLESS up & size & termv & id & new & stackv & spacev DO
  { writef("Insufficient memory*n")
    RESULTIS 20
  }

  // Initialise the variable sets
  FOR i = 0 TO Termupb DO
    up!i, size!i, termv!i, id!i, new!i := 0, 1, 0, 0, 0

  loadtermpair()
  pr()

  writef("Calling unify*n")
  TEST unify()
  THEN writef("Unification successful*n")
  ELSE writef("The terms cannot be unified*n")

  pr()

  freevec(up)
  freevec(size)
  freevec(termv)
  freevec(id)
  freevec(new)
  freevec(stackv)
  freevec(spacev)

  RESULTIS 0
}

AND mknet(n) = VALOF
{ LET op = randno(opmax)
  LET arity = n<=1 -> 0, randno(5)
  LET k = 2*n/(arity+1) + 1
  LET node = mk(op, arity, randno(tmax), randno(tmax),
                           randno(tmax), randno(tmax), randno(tmax))
  LET net = pushterm(node)
  n := n-1
  FOR c = 1 TO arity IF n DO
  { IF k>n DO k := n
    node!(Child0+c) := mknet(k)
    n := n-k
  }
  RESULTIS net  
}

AND onelevelcopy(tn) = VALOF
{ LET t = termv!tn
  LET op, arity = Op!t, Arity!t
  LET newt = mk(op, arity, 0, 0, 0, 0, 0)
  FOR c = Child0+1 TO Child0+arity DO newt!c := t!c
  RESULTIS pushterm(newt)
}

AND copy(tn) = id!tn>0 -> tn, new!tn -> new!tn, VALOF
{ // If tn has a variable, either use it or make a copy
  IF id!tn & randno(1000)<500 RESULTIS tn

  // If it has already been copied, use the previous copy
  IF new!tn RESULTIS new!tn
writef("copying term %n*n", tn)
  // Otherwise make a new copy of the term
  { LET t = termv!tn
    LET op, arity = Op!t, Arity!t
    LET newterm = mk(op, arity, 0, 0, 0, 0, 0)
    LET newtn = pushterm(newterm)
    new!tn := newtn
    FOR c = Child0+1 TO Child0+arity DO newterm!c := copy(t!c)
    id!newtn := 0
    RESULTIS newtn
  }
}

AND loadtermpair() BE
{ LET t1, t2 = ?, ?
  termp := 1

  writef("loadtermpair calling mknet*n")
  t1 := mknet(tmax)
  id!t1 := -1

pr()
writef("Give some terms variable names*n")
  // Give some terms variable names
  { LET count = 1
    UNTIL count > vmax DO
    { LET n = randno(tmax)
      IF id!n LOOP
      writef("Term %n is %c*n", n, 'A'-1+count)
      id!n, count := count, count+1
    }
  }

pr()
writef("*nCopy the net*n")
  // Make an identical copy  
  t2 := copy(t1)

pr()
writef("Copy some of the children in both terms*n")
  // Randomly perturb both terms
  FOR n = 1 TO termp DO
  { LET term = termv!randno(termp) // Choose a random term
    LET op, arity = Op!term, Arity!term
    IF arity DO
    { LET c = randno(Arity!term)   // Choose a random child
      // Replace with a top level copy of it
      term!(Child0+c) := onelevelcopy(term!(Child0+c))
    }
  }

pr()
writef("Remove the terms from variables*n")
  FOR n = 1 TO termp-1 IF id!n>0 DO termv!n := 0

pr()
  stackp := 1
  pushpair(t1, t2)
}

AND loadtermpair1() BE
{ LET A, B = ?, ?
  LET t1, t2 = ?, ?
  termp := 1

  A, B := pushid(1), pushid(2)

  // b(a(A,B),B)
  t1 := pushterm(mk(1, 2, A, B))
  t1 := pushterm(mk(2, 2, t1, B))

  // b(A, a(A,B))
  t2 := pushterm(mk(1, 2, A, B))
  t2 := pushterm(mk(2, 2, A, t2))

  stackp := 1
  id!t1 := -1
  pushpair(t1, t2)
}

AND mk(op, n, c1, c2, c3, c4, c5) = VALOF
{ LET t = ?
  LET arg = @op

  spacep := spacep-n-2
  IF spacep<0 DO
  { writef("Insufficient space*n")
    abort(999)
    RESULTIS 0
  }
  t := @spacev!spacep
  FOR i = 0 TO n+1 DO t!i := arg!i
  RESULTIS t
}
 
AND pushterm(t) = VALOF
{ LET n = termp
  termp := n+1
  termv!n := t
  writef("term %n: %i6 %c(", n, t, 'a'-1+Op!t, Arity!t)
  FOR i = 2 TO Arity!t+1 DO writef(" %n", t!i)
  writes(" )*n")
  RESULTIS n
}

AND pushid(var) = VALOF
{ LET n = termp
  id!n := var
  termp := n+1
  writef("term %n: %i6 var=%c*n", n, 0, 'A'-1+var)
  RESULTIS n
}

AND pushpair(t1, t2) = VALOF
{ LET p = @stackv!stackp
  p!0, p!1 := t1, t2
  stackp := stackp+2
//writef("pushpair:  %n: %n %n*n", stackp-2, t1, t2)
prstk()
//abort(1000)
  RESULTIS p
}

AND unify() = VALOF
{ LET x, y = ?, ?
  LET s, t = ?, ?
pr()
  stackp := stackp-2
  IF stackp<=0 RESULTIS TRUE
  x, y := stackv!stackp, stackv!(stackp+1)
  writef("Unifying term %n with term %n*n", x, y)
  x, y := find(x), find(y)

  UNLESS x & y DO
  { writef("Error: variable 0 encountered*n")
    abort(999)
    RESULTIS FALSE
  }

  IF x=y LOOP // Variables in the same set

  s, t := termv!x, termv!y

  IF s=t LOOP // Identical terms

  IF s & t DO
  { // Neither of the terms are variables
    LET n = Arity!s
    UNLESS Op!s=Op!t & n=Arity!t RESULTIS FALSE
    // Push variable pairs into the stack
    FOR c = Child0+1 TO Child0+n DO pushpair(s!c, t!c)
    unite(x, y, s)
    LOOP
  }
    
  UNLESS s | t DO
  { // Both terms are variables
    unite(x, y, 0)
    LOOP
  }

  // Either s or t is a term, the other is a variable
  TEST s
  THEN { unite(x, y, s)
         UNLESS id!x DO { id!x := id!y; id!y := 0 }
       }
  ELSE { unite(y, x, t)
         UNLESS id!y DO { id!y := id!x; id!x := 0 }
       }
} REPEAT

AND pr() BE
{ FOR n = 1 TO termt-1 IF termv!n | id!n DO
  { LET t  = termv!n
    LET var = id!n
    LET ch = var=0 -> ' ', var<0 -> '**', 'A' -1 + var
    writef("t%i3: up:%i3 sz:%i3   %c", n, up!n, size!n, ch)
    IF t DO
    { writef("   %c", 'a'-1 + Op!t)
      IF Arity!t DO
      { LET first = TRUE
        wrch('(')
        FOR c = Child0+1 TO Child0+Arity!t DO
        { LET child = find(t!c)
          LET var = id!child
          LET ch = var=0 -> ' ', var<0 -> '**', 'A' -1 + var
          UNLESS first DO writes(", ")
          first := FALSE
          TEST var THEN wrch(ch)
                   ELSE writef("t%n", child)
        }
        wrch(')')
      }
    }
    newline()
  }
}

AND prstk() BE
{ LET p = @stackv!stackp
  writef("stack: ")
  FOR i = 1 TO 10 DO
  { p := p-2
    IF p<=stackv BREAK
    writef(" (%n,%n)", p!0, p!1)
  }
  IF stackp>20 DO writef(" ...")
  newline()
}
  
AND unite(x, y, val) BE
{ LET sx = size!x
  AND sy = size!y
  AND sz = sx+sy
  //writef("unite(%n, %n, %n)*n", x, y, val)
  TEST sx>=sy
  THEN termv!x, termv!y, size!x, up!y := val, 0, sz, x // x is the root
  ELSE termv!y, termv!x, size!y, up!x := val, 0, sz, y // y is the root
  //TEST sx>=sy
  //THEN writef("up!%n := %n*n", y, x)
  //ELSE writef("up!%n := %n*n", x, y)
}

AND find(x) = VALOF
{ LET r = x
  //writef("find(%n) => ", x)
  WHILE up!r DO r := up!r
  //writef("%n*n", r)
  // Perform compression
  { LET t = up!x
    UNLESS t RESULTIS r
    //writef("up!%n := %n*n", x, r)
    termv!x, up!x := 0, r
    x := t
  } REPEAT
}


