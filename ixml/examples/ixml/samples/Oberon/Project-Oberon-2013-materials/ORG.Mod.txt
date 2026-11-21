MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)
  IMPORT SYSTEM, Files, ORS, ORB;
  (*Code generator for Oberon compiler for RISC processor.
     Procedural interface to Parser OSAP; result in array "code".
     Procedure Close writes code-files*)

  CONST WordSize* = 4;
    StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)
    MT = 12; SP = 14; LNK = 15;   (*dedicated registers*)
    maxCode = 8000; maxStrx = 2400; maxTD = 160; C24 = 1000000H;
    Reg = 10; RegI = 11; Cond = 12;  (*internal item modes*)

  (*frequently used opcodes*)  U = 2000H; V = 1000H;
    Mov = 0; Lsl = 1; Asr = 2; Ror= 3; And = 4; Ann = 5; Ior = 6; Xor = 7;
    Add = 8; Sub = 9; Cmp = 9; Mul = 10; Div = 11;
    Fad = 12; Fsb = 13; Fml = 14; Fdv = 15;
    Ldr = 8; Str = 10;
    BR = 0; BLR = 1; BC = 2; BL = 3;
    MI = 0; PL = 8; EQ = 1; NE = 9; LT = 5; GE = 13; LE = 6; GT = 14;

    TYPE Item* = RECORD
      mode*: INTEGER;
      type*: ORB.Type;
      a*, b*, r: LONGINT;
      rdo*: BOOLEAN  (*read only*)
    END ;

  (* Item forms and meaning of fields:
    mode    r      a       b
    --------------------------------
    Const   -     value (proc adr)  (immediate value)
    Var     base   off     -               (direct adr)
    Par      -     off0     off1         (indirect adr)
    Reg    regno
    RegI   regno   off     -
    Cond  cond   Fchain  Tchain  *)

  VAR pc*, varsize: LONGINT;   (*program counter, data index*)
    tdx, strx: LONGINT;
    entry: LONGINT;   (*main entry point*)
    RH: LONGINT;  (*available registers R[0] ... R[H-1]*)
    frame: LONGINT;  (*frame offset changed in SaveRegs and RestoreRegs*)
    fixorgP, fixorgD, fixorgT: LONGINT;   (*origins of lists of locations to be fixed up by loader*)
    check: BOOLEAN;  (*emit run-time checks*)
    version: INTEGER;  (* 0 = RISC-0, 1 = RISC-5 *)
    
    relmap: ARRAY 6 OF INTEGER;  (*condition codes for relations*)
    code: ARRAY maxCode OF LONGINT;
    data: ARRAY maxTD OF LONGINT;  (*type descriptors*)
    str: ARRAY maxStrx OF CHAR;

  (*instruction assemblers according to formats*)

  PROCEDURE Put0(op, a, b, c: LONGINT);
  BEGIN (*emit format-0 instruction*)
    code[pc] := ((a*10H + b) * 10H + op) * 10000H + c; INC(pc)
  END Put0;

  PROCEDURE Put1(op, a, b, im: LONGINT);
  BEGIN (*emit format-1 instruction,  -10000H <= im < 10000H*)
    IF im < 0 THEN INC(op, V) END ;
    code[pc] := (((a+40H) * 10H + b) * 10H + op) * 10000H + (im MOD 10000H); INC(pc)
  END Put1;

  PROCEDURE Put1a(op, a, b, im: LONGINT);
  BEGIN (*same as Put1, but with range test  -10000H <= im < 10000H*)
    IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im)
    ELSE Put1(Mov+U, RH, 0, im DIV 10000H);
      IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H) END ;
      Put0(op, a, b, RH)
    END
  END Put1a;

  PROCEDURE Put2(op, a, b, off: LONGINT);
  BEGIN (*emit load/store instruction*)
    code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H); INC(pc)
  END Put2;

  PROCEDURE Put3(op, cond, off: LONGINT);
  BEGIN (*emit branch instruction*)
    code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H); INC(pc)
  END Put3;

  PROCEDURE incR;
  BEGIN
    IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
  END incR;

  PROCEDURE CheckRegs*;
  BEGIN
    IF RH # 0 THEN ORS.Mark("Reg Stack"); RH := 0 END ;
    IF pc >= maxCode - 40 THEN ORS.Mark("program too long") END ;
    IF frame # 0 THEN ORS.Mark("frame error"); frame := 0 END
  END CheckRegs;

  PROCEDURE SetCC(VAR x: Item; n: LONGINT);
  BEGIN x.mode := Cond; x.a := 0; x.b := 0; x.r := n
  END SetCC;

  PROCEDURE Trap(cond, num: LONGINT);
  BEGIN Put3(BLR, cond, ORS.Pos()*100H + num*10H + MT)
  END Trap;

  (*handling of forward reference, fixups of branch addresses and constant tables*)

  PROCEDURE negated(cond: LONGINT): LONGINT;
  BEGIN
    IF cond < 8 THEN cond := cond+8 ELSE cond := cond-8 END ;
    RETURN cond
  END negated;

  PROCEDURE fix(at, with: LONGINT);
  BEGIN code[at] := code[at] DIV C24 * C24 + (with MOD C24)
  END fix;

  PROCEDURE FixOne*(at: LONGINT);
  BEGIN fix(at, pc-at-1)
  END FixOne;

  PROCEDURE FixLink*(L: LONGINT);
    VAR L1: LONGINT;
  BEGIN
    WHILE L # 0 DO L1 := code[L] MOD 40000H; fix(L, pc-L-1); L := L1 END
  END FixLink;

  PROCEDURE FixLinkWith(L0, dst: LONGINT);
    VAR L1: LONGINT;
  BEGIN
    WHILE L0 # 0 DO
      L1 := code[L0] MOD C24;
      code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 1) MOD C24); L0 := L1
    END
  END FixLinkWith;

  PROCEDURE merged(L0, L1: LONGINT): LONGINT;
    VAR L2, L3: LONGINT;
  BEGIN 
    IF L0 # 0 THEN L3 := L0;
      REPEAT L2 := L3; L3 := code[L2] MOD 40000H UNTIL L3 = 0;
      code[L2] := code[L2] + L1; L1 := L0
    END ;
    RETURN L1
  END merged;

  (* loading of operands and addresses into registers *)

  PROCEDURE GetSB(base: LONGINT);
  BEGIN
    IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0)
    ELSE Put2(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
    END
  END GetSB;

  PROCEDURE NilCheck;
  BEGIN IF check THEN Trap(EQ, 4) END
  END NilCheck;

  PROCEDURE load(VAR x: Item);
    VAR op: LONGINT;
  BEGIN
    IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END ;
    IF x.mode # Reg THEN
      IF x.mode = ORB.Const THEN
        IF x.type.form = ORB.Proc THEN
          IF x.r > 0 THEN ORS.Mark("not allowed")
          ELSIF x.r = 0 THEN Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
          ELSE GetSB(x.r); Put1(Add, RH, RH, x.a + 100H) (*mark as progbase-relative*)
          END
        ELSIF (x.a <= 0FFFFH) & (x.a >= -10000H) THEN Put1(Mov, RH, 0, x.a)
        ELSE Put1(Mov+U, RH, 0, x.a DIV 10000H MOD 10000H);
          IF x.a MOD 10000H # 0 THEN Put1(Ior, RH, RH, x.a MOD 10000H) END
        END ;
        x.r := RH; incR
      ELSIF x.mode = ORB.Var THEN
        IF x.r > 0 THEN (*local*) Put2(op, RH, SP, x.a + frame)
        ELSE GetSB(x.r); Put2(op, RH, RH, x.a)
        END ;
        x.r := RH; incR
      ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame); Put2(op, RH, RH, x.b); x.r := RH; incR
      ELSIF x.mode = RegI THEN Put2(op, x.r, x.r, x.a)
      ELSIF x.mode = Cond THEN
        Put3(BC, negated(x.r), 2);
        FixLink(x.b); Put1(Mov, RH, 0, 1); Put3(BC, 7, 1);
        FixLink(x.a); Put1(Mov, RH, 0, 0); x.r := RH; incR
      END ;
      x.mode := Reg
    END
  END load;

  PROCEDURE loadAdr(VAR x: Item);
  BEGIN
    IF x.mode = ORB.Var THEN
      IF x.r > 0 THEN (*local*) Put1a(Add, RH, SP, x.a + frame)
      ELSE GetSB(x.r); Put1a(Add, RH, RH, x.a)
      END ;
      x.r := RH; incR
    ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame);
      IF x.b # 0 THEN Put1a(Add, RH, RH, x.b) END ;
      x.r := RH; incR
    ELSIF x.mode = RegI THEN
      IF x.a # 0 THEN Put1a(Add, x.r, x.r, x.a) END
    ELSE ORS.Mark("address error")
    END ;
    x.mode := Reg
  END loadAdr;

  PROCEDURE loadCond(VAR x: Item);
  BEGIN
    IF x.type.form = ORB.Bool THEN
      IF x.mode = ORB.Const THEN x.r := 15 - x.a*8
      ELSE load(x);
        IF code[pc-1] DIV 40000000H # -2 THEN Put1(Cmp, x.r, x.r, 0) END ;
        x.r := NE; DEC(RH)
      END ;
      x.mode := Cond; x.a := 0; x.b := 0
    ELSE ORS.Mark("not Boolean?")
    END
  END loadCond;

  PROCEDURE loadTypTagAdr(T: ORB.Type);
    VAR x: Item;
  BEGIN x.mode := ORB.Var; x.a := T.len; x.r := -T.mno; loadAdr(x)
  END loadTypTagAdr;

  PROCEDURE loadStringAdr(VAR x: Item);
  BEGIN GetSB(0); Put1a(Add, RH, RH, varsize+x.a); x.mode := Reg; x.r := RH; incR
  END loadStringAdr;

  (* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

  PROCEDURE MakeConstItem*(VAR x: Item; typ: ORB.Type; val: LONGINT);
  BEGIN x.mode := ORB.Const; x.type := typ; x.a := val
  END MakeConstItem;

  PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
  BEGIN x.mode := ORB.Const; x.type := ORB.realType; x.a := SYSTEM.VAL(LONGINT, val)
  END MakeRealItem;

  PROCEDURE MakeStringItem*(VAR x: Item; len: LONGINT); (*copies string from ORS-buffer to ORG-string array*)
    VAR i: LONGINT;
  BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; i := 0;
    IF strx + len + 4 < maxStrx THEN
      WHILE len > 0 DO str[strx] := ORS.str[i]; INC(strx); INC(i); DEC(len) END ;
      WHILE strx MOD 4 # 0 DO str[strx] := 0X; INC(strx) END
    ELSE ORS.Mark("too many strings")
    END
  END MakeStringItem;

  PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: LONGINT);
  BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
    IF y.class = ORB.Par THEN x.b := 0
    ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*) ;
    ELSE x.r := y.lev
    END ;
    IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("not accessible ") END
  END MakeItem;

  (* Code generation for Selectors, Variables, Constants *)

  PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
  BEGIN;
    IF x.mode = ORB.Var THEN
      IF x.r >= 0 THEN x.a := x.a + y.val
      ELSE loadAdr(x); x.mode := RegI; x.a := y.val
      END
    ELSIF x.mode = RegI THEN x.a := x.a + y.val
    ELSIF x.mode = ORB.Par THEN x.b := x.b + y.val
    END
  END Field;

  PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
    VAR s, lim: LONGINT;
  BEGIN s := x.type.base.size; lim := x.type.len;
    IF (y.mode = ORB.Const) & (lim >= 0) THEN
      IF (y.a < 0) OR (y.a >= lim) THEN ORS.Mark("bad index") END ;
      IF x.mode IN {ORB.Var, RegI} THEN x.a := y.a * s + x.a
      ELSIF x.mode = ORB.Par THEN x.b := y.a * s + x.b
      END
    ELSE load(y);
      IF check THEN  (*check array bounds*)
        IF lim >= 0 THEN Put1a(Cmp, RH, y.r, lim)
        ELSE (*open array*)
          IF x.mode IN {ORB.Var, ORB.Par} THEN Put2(Ldr, RH, SP, x.a+4+frame); Put0(Cmp, RH, y.r, RH)
          ELSE ORS.Mark("error in Index")
          END
        END ;
        Trap(10, 1)  (*BCC*)
      END ;
      IF s = 4 THEN Put1(Lsl, y.r, y.r, 2) ELSIF s > 1 THEN Put1a(Mul, y.r, y.r, s) END ;
      IF x.mode = ORB.Var THEN
        IF x.r > 0 THEN Put0(Add, y.r, SP, y.r); INC(x.a, frame)
        ELSE GetSB(x.r);
          IF x.r = 0 THEN Put0(Add, y.r, RH, y.r)
          ELSE Put1a(Add, RH, RH, x.a); Put0(Add, y.r, RH, y.r); x.a := 0
          END
        END ;
        x.r := y.r; x.mode := RegI
      ELSIF x.mode = ORB.Par THEN
        Put2(Ldr, RH, SP, x.a + frame);
        Put0(Add, y.r, RH, y.r); x.mode := RegI; x.r := y.r; x.a := x.b
      ELSIF x.mode = RegI THEN Put0(Add, x.r, x.r, y.r); DEC(RH)
      END
    END
  END Index;

  PROCEDURE DeRef*(VAR x: Item);
  BEGIN
    IF x.mode = ORB.Var THEN
      IF x.r > 0 THEN (*local*) Put2(Ldr, RH, SP, x.a + frame) ELSE GetSB(x.r); Put2(Ldr, RH, RH, x.a) END ;
      NilCheck; x.r := RH; incR
    ELSIF x.mode = ORB.Par THEN
      Put2(Ldr, RH, SP, x.a + frame); Put2(Ldr, RH, RH, x.b); NilCheck; x.r := RH; incR
    ELSIF x.mode = RegI THEN Put2(Ldr, x.r, x.r, x.a); NilCheck
    ELSIF x.mode # Reg THEN ORS.Mark("bad mode in DeRef")
    END ;
    x.mode := RegI; x.a := 0; x.b := 0
  END DeRef;

  PROCEDURE Q(T: ORB.Type; VAR dcw: LONGINT);
  BEGIN (*one entry of type descriptor extension table*)
    IF T.base # NIL THEN
      Q(T.base, dcw); data[dcw] := (T.mno*1000H + T.len) * 1000H + dcw - fixorgT;
      fixorgT := dcw; INC(dcw)
    END
  END Q;

  PROCEDURE FindPtrFlds(typ: ORB.Type; off: LONGINT; VAR dcw: LONGINT);
    VAR fld: ORB.Object; i, s: LONGINT;
  BEGIN
    IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN data[dcw] := off; INC(dcw)
    ELSIF typ.form = ORB.Record THEN
      fld := typ.dsc;
      WHILE fld # NIL DO FindPtrFlds(fld.type, fld.val + off, dcw); fld := fld.next END
    ELSIF typ.form = ORB.Array THEN
      s := typ.base.size;
      FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off, dcw) END
    END
  END FindPtrFlds;

  PROCEDURE BuildTD*(T: ORB.Type; VAR dc: LONGINT);
    VAR dcw, k, s: LONGINT;  (*dcw = word address*)
  BEGIN dcw := dc DIV 4; s := T.size; (*convert size for heap allocation*)
    IF s <= 24 THEN s := 32 ELSIF s <= 56 THEN s := 64 ELSIF s <= 120 THEN s := 128
    ELSE s := (s+263) DIV 256 * 256
    END ;
    T.len := dc; data[dcw] := s; INC(dcw);  (*len used as address*)
    k := T.nofpar;   (*extension level!*)
    IF k > 3 THEN ORS.Mark("ext level too large")
    ELSE Q(T, dcw);
      WHILE k < 3 DO data[dcw] := -1; INC(dcw); INC(k) END
    END ;
    FindPtrFlds(T, 0, dcw); data[dcw] := -1; INC(dcw); tdx := dcw; dc := dcw*4;
    IF tdx >= maxTD THEN ORS.Mark("too many record types"); tdx := 0 END
  END BuildTD;

  PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
    VAR pc0: LONGINT;
  BEGIN
    IF T = NIL THEN
      IF x.mode >= Reg THEN DEC(RH) END ;
      SetCC(x, 7)
    ELSE (*fetch tag into RH*)
      IF varpar THEN Put2(Ldr, RH, SP, x.a+4+frame)
      ELSE load(x);
        pc0 := pc; Put3(BC, EQ, 0);  (*NIL belongs to every pointer type*)
        Put2(Ldr, RH, x.r, -8)
      END ;
      Put2(Ldr, RH, RH, T.nofpar*4); incR;
      loadTypTagAdr(T);  (*tag of T*)
      Put0(Cmp, RH-1, RH-1, RH-2); DEC(RH, 2);
      IF ~varpar THEN fix(pc0, pc - pc0 - 1) END ;
      IF isguard THEN
        IF check THEN Trap(NE, 2) END
      ELSE SetCC(x, EQ);
        IF ~varpar THEN DEC(RH) END
      END
    END
  END TypeTest;

  (* Code generation for Boolean operators *)

  PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
    VAR t: LONGINT;
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
  END Not;

  PROCEDURE And1*(VAR x: Item);   (* x := x & *)
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    Put3(BC, negated(x.r), x.a); x.a := pc-1; FixLink(x.b); x.b := 0
  END And1;

  PROCEDURE And2*(VAR x, y: Item);
  BEGIN
    IF y.mode # Cond THEN loadCond(y) END ;
    x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
  END And2;

  PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    Put3(BC, x.r, x.b);  x.b := pc-1; FixLink(x.a); x.a := 0
  END Or1;

  PROCEDURE Or2*(VAR x, y: Item);
  BEGIN
    IF y.mode # Cond THEN loadCond(y) END ;
    x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
  END Or2;

  (* Code generation for arithmetic operators *)

  PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
  BEGIN
    IF x.type.form = ORB.Int THEN
      IF x.mode = ORB.Const THEN x.a := -x.a
      ELSE load(x); Put1(Mov, RH, 0, 0); Put0(Sub, x.r, RH, x.r)
      END
    ELSIF x.type.form = ORB.Real THEN
      IF x.mode = ORB.Const THEN x.a := x.a + 7FFFFFFFH + 1
      ELSE load(x); Put1(Mov, RH, 0, 0); Put0(Fsb, x.r, RH, x.r)
      END
    ELSE (*form = Set*)
      IF x.mode = ORB.Const THEN x.a := -x.a-1 
      ELSE load(x); Put1(Xor, x.r, x.r, -1)
      END
    END
  END Neg;

  PROCEDURE AddOp*(op: LONGINT; VAR x, y: Item);   (* x := x +- y *)
  BEGIN
    IF op = ORS.plus THEN
      IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a + y.a
      ELSIF y.mode = ORB.Const THEN load(x);
        IF y.a # 0 THEN Put1a(Add, x.r, x.r, y.a) END
      ELSE load(x); load(y); Put0(Add, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
      END
    ELSE (*op = ORS.minus*)
      IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a - y.a
      ELSIF y.mode = ORB.Const THEN load(x);
        IF y.a # 0 THEN Put1a(Sub, x.r, x.r, y.a) END
      ELSE load(x); load(y); Put0(Sub, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
      END
    END
  END AddOp;

  PROCEDURE log2(m: LONGINT; VAR e: LONGINT): LONGINT;
  BEGIN e := 0;
    WHILE ~ODD(m) DO m := m DIV 2; INC(e) END ;
    RETURN m
  END log2;
  
  PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
    VAR e: LONGINT;
  BEGIN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a * y.a
    ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x); Put1(Lsl, x.r, x.r, e)
    ELSIF y.mode = ORB.Const THEN load(x); Put1a(Mul, x.r, x.r, y.a)
    ELSIF (x.mode = ORB.Const) & (x.a >= 2) & (log2(x.a, e) = 1) THEN load(y); Put1(Lsl, y.r, y.r, e); x.mode := Reg; x.r := y.r
    ELSIF x.mode = ORB.Const THEN load(y); Put1a(Mul, y.r, y.r, x.a); x.mode := Reg; x.r := y.r
    ELSE load(x); load(y); Put0(Mul, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
    END
  END MulOp;

  PROCEDURE DivOp*(op: LONGINT; VAR x, y: Item);   (* x := x op y *)
    VAR e: LONGINT;
  BEGIN
    IF op = ORS.div THEN
      IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
        IF y.a > 0 THEN x.a := x.a DIV y.a ELSE ORS.Mark("bad divisor") END
      ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x); Put1(Asr, x.r, x.r, e)
      ELSIF y.mode = ORB.Const THEN
        IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a) ELSE ORS.Mark("bad divisor") END
      ELSE load(y);
        IF check THEN Trap(LE, 6) END ;
        load(x); Put0(Div, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
      END
    ELSE (*op = ORS.mod*)
      IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
        IF y.a > 0 THEN x.a := x.a MOD y.a ELSE ORS.Mark("bad modulus") END
      ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x);
        IF e <= 16 THEN Put1(And, x.r, x.r, y.a-1) ELSE Put1(Lsl, x.r, x.r, 32-e); Put1(Ror, x.r, x.r, 32-e) END
      ELSIF y.mode = ORB.Const THEN
        IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a); Put0(Mov+U, x.r, 0, 0) ELSE ORS.Mark("bad modulus") END
      ELSE load(y);
        IF check THEN Trap(LE, 6) END ;
        load(x); Put0(Div, RH-2, x.r, y.r); Put0(Mov+U, RH-2, 0, 0); DEC(RH); x.r := RH-1
      END
    END
  END DivOp;

  (* Code generation for REAL operators *)

  PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
  BEGIN load(x); load(y);
    IF op = ORS.plus THEN Put0(Fad, RH-2, x.r, y.r)
    ELSIF op = ORS.minus THEN Put0(Fsb, RH-2, x.r, y.r)
    ELSIF op = ORS.times THEN Put0(Fml, RH-2, x.r, y.r)
    ELSIF op = ORS.rdiv THEN Put0(Fdv, RH-2, x.r, y.r)
    END ;
    DEC(RH); x.r := RH-1
  END RealOp;

  (* Code generation for set operators *)

  PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
  BEGIN
    IF x.mode = ORB.Const THEN x.a := LSL(1, x.a) 
    ELSE load(x); Put1(Mov, RH, 0, 1); Put0(Lsl, x.r, RH,  x.r)
    END
  END Singleton;

  PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
  BEGIN
    IF (x.mode = ORB.Const) & ( y.mode = ORB.Const) THEN
      IF x.a <= y.a THEN x.a := LSL(2, y.a) - LSL(1, x.a) ELSE x.a := 0 END
    ELSE
      IF (x.mode = ORB.Const) & (x.a <= 16) THEN x.a := LSL(-1, x.a)
      ELSE load(x); Put1(Mov, RH, 0, -1); Put0(Lsl, x.r, RH, x.r)
      END ;
      IF (y.mode = ORB.Const) & (y.a < 16) THEN Put1(Mov, RH, 0, LSL(-2, y.a)); y.mode := Reg; y.r := RH; incR
      ELSE load(y); Put1(Mov, RH, 0, -2); Put0(Lsl, y.r, RH, y.r)
      END ;
      IF x.mode = ORB.Const THEN
        IF x.a # 0 THEN Put1(Xor, y.r, y.r, -1); Put1a(And, RH-1, y.r, x.a) END ;
        x.mode := Reg; x.r := RH-1
      ELSE DEC(RH); Put0(Ann, RH-1, x.r, y.r)
      END
    END
  END Set;

  PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
  BEGIN load(y);
    IF x.mode = ORB.Const THEN Put1(Ror, y.r, y.r, (x.a + 1) MOD 20H); DEC(RH)
    ELSE load(x); Put1(Add, x.r, x.r, 1); Put0(Ror, y.r, y.r, x.r); DEC(RH, 2)
    END ;
    SetCC(x, MI)
  END In;

  PROCEDURE SetOp*(op: LONGINT; VAR x, y: Item);   (* x := x op y *)
    VAR xset, yset: SET; (*x.type.form = Set*)
  BEGIN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
      xset := SYSTEM.VAL(SET, x.a); yset := SYSTEM.VAL(SET, y.a);
      IF op = ORS.plus THEN xset := xset + yset
      ELSIF op = ORS.minus THEN xset := xset - yset
      ELSIF op = ORS.times THEN xset := xset * yset
      ELSIF op = ORS.rdiv THEN xset := xset / yset
      END ;
      x.a := SYSTEM.VAL(LONGINT, xset)
    ELSIF y.mode = ORB.Const THEN
      load(x);
      IF op = ORS.plus THEN Put1a(Ior, x.r, x.r, y.a)
      ELSIF op = ORS.minus THEN Put1a(Ann, x.r, x.r, y.a)
      ELSIF op = ORS.times THEN Put1a(And, x.r, x.r, y.a)
      ELSIF op = ORS.rdiv THEN Put1a(Xor, x.r, x.r, y.a)
      END ;
    ELSE load(x); load(y);
      IF op = ORS.plus THEN Put0(Ior, RH-2, x.r, y.r)
      ELSIF op = ORS.minus THEN Put0(Ann, RH-2, x.r, y.r)
      ELSIF op = ORS.times THEN Put0(And, RH-2, x.r, y.r)
      ELSIF op = ORS.rdiv THEN Put0(Xor, RH-2, x.r, y.r)
      END ;
      DEC(RH); x.r := RH-1
    END 
  END SetOp;

  (* Code generation for relations *)

  PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  BEGIN
    IF (y.mode = ORB.Const) & (y.type.form # ORB.Proc) THEN
      load(x);
      IF (y.a # 0) OR ~(op IN {ORS.eql, ORS.neq}) OR (code[pc-1] DIV 40000000H # -2) THEN Put1a(Cmp, x.r, x.r, y.a) END ;
      DEC(RH)
    ELSE
      IF (x.mode = Cond) OR (y.mode = Cond) THEN ORS.Mark("not implemented") END ;
      load(x); load(y); Put0(Cmp, x.r, x.r, y.r); DEC(RH, 2)
    END ;
    SetCC(x, relmap[op - ORS.eql])
  END IntRelation;

  PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  BEGIN load(x);
    IF (y.mode = ORB.Const) & (y.a = 0) THEN DEC(RH)
    ELSE load(y); Put0(Fsb, x.r, x.r, y.r); DEC(RH, 2)
    END ;
    SetCC(x, relmap[op - ORS.eql])
  END RealRelation;

  PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
    (*x, y are char arrays or strings*)
  BEGIN
    IF x.type.form = ORB.String THEN loadStringAdr(x) ELSE loadAdr(x) END ;
    IF y.type.form = ORB.String THEN loadStringAdr(y) ELSE loadAdr(y) END ;
    Put2(Ldr+1, RH, x.r, 0); Put1(Add, x.r, x.r, 1);
    Put2(Ldr+1, RH+1, y.r, 0); Put1(Add, y.r, y.r, 1);
    Put0(Cmp, RH+2, RH, RH+1); Put3(BC, NE, 2);
    Put1(Cmp, RH+2, RH, 0); Put3(BC, NE, -8);
    DEC(RH, 2); SetCC(x, relmap[op - ORS.eql])
  END StringRelation;

  (* Code generation of Assignments *)

  PROCEDURE StrToChar*(VAR x: Item);
  BEGIN x.type := ORB.charType; DEC(strx, 4); x.a := ORD(str[x.a])
  END StrToChar;

  PROCEDURE Store*(VAR x, y: Item); (* x := y *)
    VAR op: LONGINT;
  BEGIN  load(y);
    IF x.type.size = 1 THEN op := Str+1 ELSE op := Str END ;
    IF x.mode = ORB.Var THEN
      IF x.r > 0 THEN (*local*) Put2(op, y.r, SP, x.a + frame)
      ELSE GetSB(x.r); Put2(op, y.r, RH, x.a)
      END
    ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame); Put2(op, y.r, RH, x.b);
    ELSIF x.mode = RegI THEN Put2(op, y.r, x.r, x.a); DEC(RH);
    ELSE ORS.Mark("bad mode in Store")
    END ;
    DEC(RH)
  END Store;

  PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
    VAR s, pc0: LONGINT;
  BEGIN
    IF y.type.size # 0 THEN
      loadAdr(x); loadAdr(y);
      IF (x.type.form = ORB.Array) &  (x.type.len > 0) THEN
        IF y.type.len >= 0 THEN 
          IF x.type.size = y.type.size THEN Put1a(Mov, RH, 0, (y.type.size+3) DIV 4)
          ELSE ORS.Mark("different length/size, not implemented")
          END
        ELSE (*y  open array*) Put2(Ldr, RH, SP, y.a+4); s := y.type.base.size;  (*element size*)
          pc0 := pc; Put3(BC, EQ, 0);
          IF s = 1 THEN Put1(Add, RH, RH, 3); Put1(Asr, RH, RH, 2)
          ELSIF s # 4 THEN Put1a(Mul, RH, RH, s DIV 4)
          END ;
          IF check THEN
            Put1a(Mov, RH+1, 0, (x.type.size+3) DIV 4); Put0(Cmp, RH+1, RH, RH+1); Trap(GT, 3)
          END ;
          fix(pc0, pc + 5 - pc0)
        END
      ELSIF x.type.form = ORB.Record THEN Put1a(Mov, RH, 0, x.type.size DIV 4)
      ELSE ORS.Mark("inadmissible assignment")
      END ;
      Put2(Ldr, RH+1, y.r, 0); Put1(Add, y.r, y.r, 4);
      Put2(Str, RH+1, x.r, 0); Put1(Add, x.r, x.r, 4);
      Put1(Sub, RH, RH, 1); Put3(BC, NE, -6)
    END ;
    RH := 0
  END StoreStruct;

  PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *) 
     VAR len: LONGINT;
   BEGIN loadAdr(x); len := x.type.len;
    IF len >= 0 THEN
      IF len <  y.b THEN ORS.Mark("string too long") END
    ELSIF check THEN Put2(Ldr, RH, SP, x.a+4);  (*open array len, frame = 0*)
      Put1(Cmp,RH, RH, y.b); Trap(LT, 3)
    END ;
    loadStringAdr(y);
    Put2(Ldr, RH, y.r, 0); Put1(Add, y.r, y.r, 4);
    Put2(Str, RH, x.r, 0); Put1(Add, x.r, x.r, 4);
    Put1(Asr, RH, RH, 24); Put3(BC, NE,  -6);  RH := 0
   END CopyString;
  
  (* Code generation for parameters *)
  
  PROCEDURE OpenArrayParam*(VAR x: Item);
  BEGIN loadAdr(x);
    IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE Put2(Ldr, RH, SP, x.a+4+frame) END ;
    incR
  END OpenArrayParam;

  PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
    VAR xmd: INTEGER;
  BEGIN xmd := x.mode; loadAdr(x);
    IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN (*open array*)
      IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE  Put2(Ldr, RH, SP, x.a+4+frame) END ;
      incR
    ELSIF ftype.form = ORB.Record THEN
      IF xmd = ORB.Par THEN Put2(Ldr, RH, SP, x.a+4+frame); incR ELSE loadTypTagAdr(x.type) END
    END
  END VarParam;

  PROCEDURE ValueParam*(VAR x: Item);
  BEGIN load(x)
  END ValueParam;

  PROCEDURE StringParam*(VAR x: Item);
  BEGIN loadStringAdr(x); Put1(Mov, RH, 0, x.b); incR  (*len*)
  END StringParam;

  (*For Statements*)

  PROCEDURE For0*(VAR x, y: Item);
  BEGIN load(y)
  END For0;

  PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
  BEGIN 
    IF z.mode = ORB.Const THEN Put1a(Cmp, RH, y.r, z.a)
    ELSE load(z); Put0(Cmp, RH-1, y.r, z.r); DEC(RH)
    END ;
    L := pc;
    IF w.a > 0 THEN Put3(BC, GT, 0)
    ELSIF w.a < 0 THEN Put3(BC, LT, 0)
    ELSE ORS.Mark("zero increment"); Put3(BC, MI, 0)
    END ;
    Store(x, y)
  END For1;

  PROCEDURE For2*(VAR x, y, w: Item);
  BEGIN load(x); DEC(RH); Put1a(Add, x.r, x.r, w.a)
  END For2;

  (* Branches, procedure calls, procedure prolog and epilog *)

  PROCEDURE Here*(): LONGINT;
  BEGIN RETURN pc
  END Here;

  PROCEDURE FJump*(VAR L: LONGINT);
  BEGIN Put3(BC, 7, L); L := pc-1
  END FJump;

  PROCEDURE CFJump*(VAR x: Item);
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    Put3(BC, negated(x.r), x.a); FixLink(x.b); x.a := pc-1
  END CFJump;

  PROCEDURE BJump*(L: LONGINT);
  BEGIN Put3(BC, 7, L-pc-1)
  END BJump;

  PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    Put3(BC, negated(x.r), L-pc-1); FixLink(x.b); FixLinkWith(x.a, L)
  END CBJump;

  PROCEDURE Fixup*(VAR x: Item);
  BEGIN FixLink(x.a)
  END Fixup;

  PROCEDURE SaveRegs(r: LONGINT);  (* R[0 .. r-1]*)
    VAR r0: LONGINT;
  BEGIN (*r > 0*) r0 := 0;
    Put1(Sub, SP, SP, r*4); INC(frame, 4*r);
    REPEAT Put2(Str, r0, SP, (r-r0-1)*4); INC(r0) UNTIL r0 = r
  END SaveRegs;

  PROCEDURE RestoreRegs(r: LONGINT); (*R[0 .. r-1]*)
    VAR r0: LONGINT;
  BEGIN (*r > 0*) r0 := r;
    REPEAT DEC(r0); Put2(Ldr, r0, SP, (r-r0-1)*4) UNTIL r0 = 0;
    Put1(Add, SP, SP, r*4); DEC(frame, 4*r)
  END RestoreRegs;

  PROCEDURE PrepCall*(VAR x: Item; VAR r: LONGINT);
  BEGIN (*x.type.form = ORB.Proc*)
    IF x.mode > ORB.Par THEN load(x) END ;
    r := RH;
    IF RH > 0 THEN SaveRegs(RH); RH := 0 END
  END PrepCall;

  PROCEDURE Call*(VAR x: Item; r: LONGINT);
  BEGIN (*x.type.form = ORB.Proc*)
    IF x.mode = ORB.Const THEN
      IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
      ELSE (*imported*)
        IF pc - fixorgP < 1000H THEN
          Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP); fixorgP := pc-1
        ELSE ORS.Mark("fixup impossible")
        END
      END
    ELSE
      IF x.mode <= ORB.Par THEN load(x); DEC(RH)
      ELSE Put2(Ldr, RH, SP, 0); Put1(Add, SP, SP, 4); DEC(r); DEC(frame, 4)
      END ;
      IF check THEN Trap(EQ, 5) END ;
      Put3(BLR, 7, RH)
    END ;
    IF x.type.base.form = ORB.NoTyp THEN (*procedure*) RH := 0
    ELSE (*function*)
      IF r > 0 THEN Put0(Mov, r, 0, 0); RestoreRegs(r) END ;
      x.mode := Reg; x.r := r; RH := r+1
    END
  END Call;

  PROCEDURE Enter*(parblksize, locblksize: LONGINT; int: BOOLEAN);
    VAR a, r: LONGINT;
  BEGIN frame := 0;
    IF ~int THEN (*procedure prolog*)
      IF locblksize >= 10000H THEN ORS.Mark("too many locals") END ;
      a := 4; r := 0;
      Put1(Sub, SP, SP, locblksize); Put2(Str, LNK, SP, 0);
      WHILE a < parblksize DO Put2(Str, r, SP, a); INC(r); INC(a, 4) END
    ELSE (*interrupt procedure*)
      Put1(Sub, SP, SP, locblksize); Put2(Str, 0, SP, 0); Put2(Str, 1, SP, 4); Put2(Str, 2, SP, 8)
      (*R0, R1, R2 saved on stack*)
    END
  END Enter;

  PROCEDURE Return*(form: INTEGER; VAR x: Item; size: LONGINT; int: BOOLEAN);
  BEGIN
    IF form # ORB.NoTyp THEN load(x) END ;
    IF ~int THEN (*procedure epilog*)
      Put2(Ldr, LNK, SP, 0); Put1(Add, SP, SP, size); Put3(BR, 7, LNK)
    ELSE (*interrupt return, restore R2, R1, R0*)
      Put2(Ldr, 2, SP, 8); Put2(Ldr, 1, SP, 4); Put2(Ldr, 0, SP, 0); Put1(Add, SP, SP, size);
      Put3(BR, 7, 10H) (*RTI*)
    END ;
    RH := 0
  END Return;

  (* In-line code procedures*)

  PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
    VAR op, zr, v: LONGINT;
  BEGIN (*frame = 0*)
    IF upordown = 0 THEN op := Add ELSE op := Sub END ;
    IF x.type = ORB.byteType THEN v := 1 ELSE v := 0 END ;
    IF y.type.form = ORB.NoTyp THEN y.mode := ORB.Const; y.a := 1 END ;
    IF (x.mode = ORB.Var) & (x.r > 0) THEN
      zr := RH; Put2(Ldr+v, zr, SP, x.a); incR;
      IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y); Put0(op, zr, zr, y.r); DEC(RH) END ;
      Put2(Str+v, zr, SP, x.a); DEC(RH)
    ELSE loadAdr(x); zr := RH; Put2(Ldr+v, RH, x.r, 0); incR;
      IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y); Put0(op, zr, zr, y.r); DEC(RH) END ;
      Put2(Str+v, zr, x.r, 0); DEC(RH, 2)
    END
  END Increment;

  PROCEDURE Include*(inorex: LONGINT; VAR x, y: Item);
    VAR op, zr: LONGINT;
  BEGIN loadAdr(x); zr := RH; Put2(Ldr, RH, x.r, 0); incR;
    IF inorex = 0 THEN op := Ior ELSE op := Ann END ;
    IF y.mode = ORB.Const THEN Put1a(op, zr, zr, LSL(1, y.a))
    ELSE load(y); Put1(Mov, RH, 0, 1); Put0(Lsl, y.r, RH, y.r); Put0(op, zr, zr, y.r); DEC(RH)
    END ;
    Put2(Str, zr, x.r, 0); DEC(RH, 2)
  END Include;

  PROCEDURE Assert*(VAR x: Item);
    VAR cond: LONGINT;
  BEGIN
    IF x.mode # Cond THEN loadCond(x) END ;
    IF x.a = 0 THEN cond := negated(x.r)
    ELSE Put3(BC, x.r, x.b); FixLink(x.a); x.b := pc-1; cond := 7
    END ;
    Trap(cond, 7); FixLink(x.b)
  END Assert; 

  PROCEDURE New*(VAR x: Item);
  BEGIN loadAdr(x); loadTypTagAdr(x.type.base); Trap(7, 0); RH := 0
  END New;

  PROCEDURE Pack*(VAR x, y: Item);
    VAR z: Item;
  BEGIN z := x; load(x); load(y);
    Put1(Lsl, y.r, y.r, 23); Put0(Add, x.r, x.r, y.r); DEC(RH); Store(z, x)
  END Pack;

  PROCEDURE Unpk*(VAR x, y: Item);
    VAR z, e0: Item;
  BEGIN  z := x; load(x); e0.mode := Reg; e0.r := RH; e0.type := ORB.intType;
    Put1(Asr, RH, x.r, 23); Put1(Sub, RH, RH, 127); Store(y, e0); incR;
    Put1(Lsl, RH, RH, 23); Put0(Sub, x.r, x.r, RH); Store(z, x)
  END Unpk;

  PROCEDURE Led*(VAR x: Item);
  BEGIN load(x); Put1(Mov, RH, 0, -60); Put2(Str, x.r, RH, 0); DEC(RH)
  END Led;

  PROCEDURE Get*(VAR x, y: Item);
  BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(y, x)
  END Get;

  PROCEDURE Put*(VAR x, y: Item);
  BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(x, y)
  END Put;

  PROCEDURE Copy*(VAR x, y, z: Item);
  BEGIN load(x); load(y);
    IF z.mode = ORB.Const THEN
      IF z.a > 0 THEN load(z) ELSE ORS.Mark("bad count") END
    ELSE load(z);
      IF check THEN Trap(LT, 3) END ;
      Put3(BC, EQ, 6)
    END ;
    Put2(Ldr, RH, x.r, 0); Put1(Add, x.r, x.r, 4);
    Put2(Str, RH, y.r, 0); Put1(Add, y.r, y.r, 4);
    Put1(Sub, z.r, z.r, 1); Put3(BC, NE, -6); DEC(RH, 3)
  END Copy;

  PROCEDURE LDPSR*(VAR x: Item);
  BEGIN (*x.mode = Const*)  Put3(0, 15, x.a + 20H)
  END LDPSR;

  PROCEDURE LDREG*(VAR x, y: Item);
  BEGIN
    IF y.mode = ORB.Const THEN Put1a(Mov, x.a, 0, y.a)
    ELSE load(y); Put0(Mov, x.a, 0, y.r); DEC(RH)
    END
  END LDREG;

  (*In-line code functions*)

  PROCEDURE Abs*(VAR x: Item);
  BEGIN
    IF x.mode = ORB.Const THEN x.a := ABS(x.a)
    ELSE load(x);
      IF x.type.form = ORB.Real THEN Put1(Lsl, x.r, x.r, 1); Put1(Ror, x.r, x.r, 1)
      ELSE Put1(Cmp, x.r, x.r, 0); Put3(BC, GE, 2); Put1(Mov, RH, 0, 0); Put0(Sub, x.r, RH, x.r)
      END
    END
  END Abs;

  PROCEDURE Odd*(VAR x: Item);
  BEGIN load(x); Put1(And, x.r, x.r, 1); SetCC(x, NE); DEC(RH)
  END Odd;

  PROCEDURE Floor*(VAR x: Item);
  BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H); Put0(Fad+V, x.r, x.r, RH)
  END Floor;

  PROCEDURE Float*(VAR x: Item);
  BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H);  Put0(Fad+U, x.r, x.r, RH)
  END Float;

  PROCEDURE Ord*(VAR x: Item);
  BEGIN
    IF x.mode IN {ORB.Var, ORB.Par, RegI, Cond} THEN load(x) END
  END Ord;

  PROCEDURE Len*(VAR x: Item);
  BEGIN
    IF x.type.len >= 0 THEN
      IF x.mode = RegI THEN DEC(RH) END ;
      x.mode := ORB.Const; x.a := x.type.len
    ELSE (*open array*) Put2(Ldr, RH, SP, x.a + 4 + frame); x.mode := Reg; x.r := RH; incR
    END 
  END Len;

  PROCEDURE Shift*(fct: LONGINT; VAR x, y: Item);
    VAR op: LONGINT;
  BEGIN load(x);
    IF fct = 0 THEN op := Lsl ELSIF fct = 1 THEN op := Asr ELSE op := Ror END ;
    IF y.mode = ORB.Const THEN Put1(op, x.r, x.r, y.a MOD 20H)
    ELSE load(y); Put0(op, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
    END
  END Shift;

  PROCEDURE ADC*(VAR x, y: Item);
  BEGIN load(x); load(y); Put0(Add+2000H, x.r, x.r, y.r); DEC(RH)
  END ADC;

  PROCEDURE SBC*(VAR x, y: Item);
  BEGIN load(x); load(y); Put0(Sub+2000H, x.r, x.r, y.r); DEC(RH)
  END SBC;

  PROCEDURE UML*(VAR x, y: Item);
  BEGIN load(x); load(y); Put0(Mul+2000H, x.r, x.r, y.r); DEC(RH)
  END UML;

  PROCEDURE Bit*(VAR x, y: Item);
  BEGIN load(x); Put2(Ldr, x.r, x.r, 0);
    IF y.mode = ORB.Const THEN Put1(Ror, x.r, x.r, y.a+1); DEC(RH)
    ELSE load(y); Put1(Add, y.r, y.r, 1); Put0(Ror, x.r, x.r, y.r); DEC(RH, 2)
    END ;
    SetCC(x, MI)
  END Bit;

  PROCEDURE Register*(VAR x: Item);
  BEGIN (*x.mode = Const*)
    Put0(Mov, RH, 0, x.a MOD 10H); x.mode := Reg; x.r := RH; incR
  END Register;

  PROCEDURE H*(VAR x: Item);
  BEGIN (*x.mode = Const*)
    Put0(Mov + U + x.a MOD 2 * V, RH, 0, 0); x.mode := Reg; x.r := RH; incR
  END H;

  PROCEDURE Adr*(VAR x: Item);
  BEGIN 
    IF x.mode IN {ORB.Var, ORB.Par, RegI} THEN loadAdr(x)
    ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.Proc) THEN load(x)
    ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.String) THEN loadStringAdr(x)
    ELSE ORS.Mark("not addressable")
    END
  END Adr;

  PROCEDURE Condition*(VAR x: Item);
  BEGIN (*x.mode = Const*) SetCC(x, x.a)
  END Condition;

  PROCEDURE Open*(v: INTEGER);
  BEGIN pc := 0; tdx := 0; strx := 0; RH := 0; fixorgP := 0; fixorgD := 0; fixorgT := 0; check := v # 0; version := v;
    IF v = 0 THEN pc := 1;
      REPEAT code[pc] := 0; INC(pc) UNTIL pc = 8
    END
  END Open;

  PROCEDURE SetDataSize*(dc: LONGINT);
  BEGIN varsize := dc
  END SetDataSize;

  PROCEDURE Header*;
  BEGIN entry := pc*4;
    IF version = 0 THEN code[0] := 0E7000000H-1 + pc;  Put1a(Mov, SP, 0, StkOrg0)  (*RISC-0*)
    ELSE Put1(Sub, SP, SP, 4); Put2(Str, LNK, SP, 0)
    END
  END Header;

  PROCEDURE NofPtrs(typ: ORB.Type): LONGINT;
    VAR fld: ORB.Object; n: LONGINT;
  BEGIN
    IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN n := 1
    ELSIF typ.form = ORB.Record THEN
      fld := typ.dsc; n := 0;
      WHILE fld # NIL DO n := NofPtrs(fld.type) + n; fld := fld.next END
    ELSIF typ.form = ORB.Array THEN n := NofPtrs(typ.base) * typ.len
    ELSE n := 0
    END ;
    RETURN n
  END NofPtrs;

  PROCEDURE FindPtrs(VAR R: Files.Rider; typ: ORB.Type; adr: LONGINT);
    VAR fld: ORB.Object; i, s: LONGINT;
  BEGIN
    IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN Files.WriteInt(R, adr)
    ELSIF typ.form = ORB.Record THEN
      fld := typ.dsc;
      WHILE fld # NIL DO FindPtrs(R, fld.type, fld.val + adr); fld := fld.next END
    ELSIF typ.form = ORB.Array THEN
      s := typ.base.size;
      FOR i := 0 TO typ.len-1 DO FindPtrs(R, typ.base, i*s + adr) END
    END
  END FindPtrs;

  PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: LONGINT);
    VAR obj: ORB.Object;
      i, comsize, nofimps, nofptrs, size: LONGINT;
      name: ORS.Ident;
      F: Files.File; R: Files.Rider;
  BEGIN  (*exit code*)
    IF version = 0 THEN Put1(Mov, 0, 0, 0); Put3(BR, 7, 0)  (*RISC-0*)
    ELSE Put2(Ldr, LNK, SP, 0); Put1(Add, SP, SP, 4); Put3(BR, 7, LNK)
    END ;
    obj := ORB.topScope.next; nofimps := 0; comsize := 4; nofptrs := 0;
    WHILE obj # NIL DO
      IF (obj.class = ORB.Mod) & (obj.dsc # ORB.system) THEN INC(nofimps) (*count imports*)
      ELSIF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc)
          & (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN i := 0; (*count commands*)
        WHILE obj.name[i] # 0X DO INC(i) END ;
        i := (i+4) DIV 4 * 4; INC(comsize, i+4)
      ELSIF obj.class = ORB.Var THEN INC(nofptrs, NofPtrs(obj.type))  (*count pointers*)
      END ;
      obj := obj.next
    END ;
    size := varsize + strx + comsize + (pc + nofimps + nofent + nofptrs + 1)*4;  (*varsize includes type descriptors*)
    
    ORB.MakeFileName(name, modid, ".rsc"); (*write code file*)
    F := Files.New(name); Files.Set(R, F, 0); Files.WriteString(R, modid); Files.WriteInt(R, key); Files.Write(R, CHR(version));
    Files.WriteInt(R, size);
    obj := ORB.topScope.next;
    WHILE (obj # NIL) & (obj.class = ORB.Mod) DO  (*imports*)
      IF obj.dsc # ORB.system THEN Files.WriteString(R, obj(ORB.Module).orgname); Files.WriteInt(R, obj.val) END ;
      obj := obj.next
    END ;
    Files.Write(R, 0X);
    Files.WriteInt(R, tdx*4);
    i := 0;
    WHILE i < tdx DO Files.WriteInt(R, data[i]); INC(i) END ; (*type descriptors*)
    Files.WriteInt(R, varsize - tdx*4);  (*data*)
    Files.WriteInt(R, strx);
    FOR i := 0 TO strx-1 DO Files.Write(R, str[i]) END ;  (*strings*)
    Files.WriteInt(R, pc);  (*code len*)
    FOR i := 0 TO pc-1 DO Files.WriteInt(R, code[i]) END ;  (*program*)
    obj := ORB.topScope.next;
    WHILE obj # NIL DO  (*commands*)
      IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) &
          (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
        Files.WriteString(R, obj.name); Files.WriteInt(R, obj.val)
      END ;
      obj := obj.next
    END ;
    Files.Write(R, 0X);
    Files.WriteInt(R, nofent); Files.WriteInt(R, entry);
    obj := ORB.topScope.next;
    WHILE obj # NIL DO  (*entries*)
      IF obj.exno # 0 THEN
        IF (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) OR (obj.class = ORB.Var) THEN
          Files.WriteInt(R, obj.val);
        ELSIF obj.class = ORB.Typ THEN
          IF obj.type.form = ORB.Record THEN Files.WriteInt(R,  obj.type.len MOD 10000H)
          ELSIF (obj.type.form = ORB.Pointer) & ((obj.type.base.typobj = NIL) OR (obj.type.base.typobj.exno = 0)) THEN
            Files.WriteInt(R,  obj.type.base.len MOD 10000H)
          END
        END
      END ;
      obj := obj.next
    END ;
    obj := ORB.topScope.next;
    WHILE obj # NIL DO  (*pointer variables*)
      IF obj.class = ORB.Var THEN FindPtrs(R, obj.type, obj.val) END ;
      obj := obj.next
    END ;
    Files.WriteInt(R, -1);
    Files.WriteInt(R, fixorgP); Files.WriteInt(R, fixorgD); Files.WriteInt(R, fixorgT); Files.WriteInt(R, entry);
    Files.Write(R, "O"); Files.Register(F)
  END Close;

BEGIN relmap[0] := 1; relmap[1] := 9; relmap[2] := 5; relmap[3] := 6; relmap[4] := 14; relmap[5] := 13;
END ORG.
