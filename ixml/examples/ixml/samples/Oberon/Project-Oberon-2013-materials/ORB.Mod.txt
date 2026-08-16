MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 8.3.2019  in Oberon-07*)
  IMPORT Files, ORS;
  (*Definition of data types Object and Type, which together form the data structure
    called "symbol table". Contains procedures for creation of Objects, and for search:
    NewObj, this, thisimport, thisfield (and OpenScope, CloseScope).
    Handling of import and export, i.e. reading and writing of "symbol files" is done by procedures
    Import and Export. This module contains the list of standard identifiers, with which
    the symbol table (universe), and that of the pseudo-module SYSTEM are initialized. *)

  CONST versionkey* = 1; maxTypTab = 64;
    (* class values*) Head* = 0;
      Const* = 1; Var* = 2; Par* = 3; Fld* = 4; Typ* = 5;
      SProc* = 6; SFunc* = 7; Mod* = 8;

    (* form values*)
      Byte* = 1; Bool* = 2; Char* = 3; Int* = 4; Real* = 5; Set* = 6;
      Pointer* = 7; NilTyp* = 8; NoTyp* = 9; Proc* = 10;
      String* = 11; Array* = 12; Record* = 13;
      
  TYPE Object* = POINTER TO ObjDesc;
    Module* = POINTER TO ModDesc;
    Type* = POINTER TO TypeDesc;

    ObjDesc*= RECORD
      class*, exno*: BYTE;
      expo*, rdo*: BOOLEAN;   (*exported / read-only*)
      lev*: INTEGER;
      next*, dsc*: Object;
      type*: Type;
      name*: ORS.Ident;
      val*: LONGINT
    END ;

    ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident END ;

    TypeDesc* = RECORD
      form*, ref*, mno*: INTEGER;  (*ref is only used for import/export*)
      nofpar*: INTEGER;  (*for procedures, extension level for records*)
      len*: LONGINT;  (*for arrays, len < 0 => open array; for records: adr of descriptor*)
      dsc*, typobj*: Object;
      base*: Type;  (*for arrays, records, pointers*)
      size*: LONGINT;  (*in bytes; always multiple of 4, except for Byte, Bool and Char*)
    END ;

  (* Object classes and the meaning of "val":
    class    val
    ----------
    Var      address
    Par      address
    Const    value
    Fld      offset
    Typ      type descriptor (TD) address
    SProc    inline code number
    SFunc    inline code number
    Mod      key

  Type forms and the meaning of "dsc" and "base":
    form     dsc      base
    ------------------------
    Pointer  -        type of dereferenced object
    Proc     params   result type
    Array    -        type of elements
    Record   fields   extension *)

  VAR topScope*, universe, system*: Object;
    byteType*, boolType*, charType*: Type;
    intType*, realType*, setType*, nilType*, noType*, strType*: Type;
    nofmod, Ref: INTEGER;
    typtab: ARRAY maxTypTab OF Type;

  PROCEDURE NewObj*(VAR obj: Object; id: ORS.Ident; class: INTEGER);  (*insert new Object with name id*)
    VAR new, x: Object;
  BEGIN x := topScope;
    WHILE (x.next # NIL) & (x.next.name # id) DO x := x.next END ;
    IF x.next = NIL THEN
      NEW(new); new.name := id; new.class := class; new.next := NIL; new.rdo := FALSE; new.dsc := NIL;
      x.next := new; obj := new
    ELSE obj := x.next; ORS.Mark("mult def")
    END 
  END NewObj;

  PROCEDURE thisObj*(): Object;
    VAR s, x: Object;
  BEGIN s := topScope;
    REPEAT x := s.next;
      WHILE (x # NIL) & (x.name # ORS.id) DO x := x.next END ;
      s := s.dsc
    UNTIL (x # NIL) OR (s = NIL);
    RETURN x
  END thisObj;

  PROCEDURE thisimport*(mod: Object): Object;
    VAR obj: Object;
  BEGIN
    IF mod.rdo THEN
      IF mod.name[0] # 0X THEN
        obj := mod.dsc;
        WHILE (obj # NIL) & (obj.name # ORS.id) DO obj := obj.next END
      ELSE obj := NIL
      END
    ELSE obj := NIL
    END ;
    RETURN obj
  END thisimport;

  PROCEDURE thisfield*(rec: Type): Object;
    VAR fld: Object;
  BEGIN fld := rec.dsc;
    WHILE (fld # NIL) & (fld.name # ORS.id) DO fld := fld.next END ;
    RETURN fld
  END thisfield;

  PROCEDURE OpenScope*;
    VAR s: Object;
  BEGIN NEW(s); s.class := Head; s.dsc := topScope; s.next := NIL; topScope := s
  END OpenScope;

  PROCEDURE CloseScope*;
  BEGIN topScope := topScope.dsc
  END CloseScope;

  (*------------------------------- Import ---------------------------------*)

  PROCEDURE MakeFileName*(VAR FName: ORS.Ident; name, ext: ARRAY OF CHAR);
    VAR i, j: INTEGER;
  BEGIN i := 0; j := 0;  (*assume name suffix less than 4 characters*)
    WHILE (i < ORS.IdLen-5) & (name[i] > 0X) DO FName[i] := name[i]; INC(i) END ;
    REPEAT FName[i]:= ext[j]; INC(i); INC(j) UNTIL ext[j] = 0X;
    FName[i] := 0X
  END MakeFileName;
  
  PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
    VAR mod: Module; obj, obj1: Object;
  BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
    IF obj = NIL THEN  (*new module, search for alias*)
      obj := topScope.next;
      WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
      IF obj = NIL THEN (*insert new module*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.dsc := NIL; mod.next := NIL;
        IF decl THEN mod.type := noType ELSE mod.type := nilType END ;
        obj1.next := mod; obj := mod
      ELSIF decl THEN
        IF obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
      ELSE ORS.Mark("conflict with alias")
      END
    ELSIF decl THEN (*module already present, explicit import by declaration*)
      IF  obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
    END ;
    RETURN obj
  END ThisModule;
  
  PROCEDURE Read(VAR R: Files.Rider; VAR x: INTEGER);
    VAR b: BYTE;
  BEGIN Files.ReadByte(R, b);
    IF b < 80H THEN x := b ELSE x := b - 100H END
  END Read;
  
  PROCEDURE InType(VAR R: Files.Rider; thismod: Object; VAR T: Type);
    VAR key: LONGINT;
      ref, class, form, np, readonly: INTEGER;
      fld, par, obj, mod, last: Object;
      t: Type;
      name, modname: ORS.Ident;
  BEGIN Read(R, ref);
    IF ref < 0 THEN T := typtab[-ref]  (*already read*)
    ELSE NEW(t); T := t; typtab[ref] := t; t.mno := thismod.lev;
      Read(R, form); t.form := form;
      IF form = Pointer THEN InType(R, thismod, t.base); t.size := 4
      ELSIF form = Array THEN
        InType(R, thismod, t.base); Files.ReadNum(R, t.len); Files.ReadNum(R, t.size)
      ELSIF form = Record THEN
        InType(R, thismod, t.base);
        IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END ;
        Files.ReadNum(R, t.len); (*TD adr/exno*)
        Files.ReadNum(R, t.nofpar);  (*ext level*)
        Files.ReadNum(R, t.size);
        Read(R, class); last := NIL;
        WHILE class # 0 DO  (*fields*)
          NEW(fld); fld.class := class; Files.ReadString(R, fld.name);
          IF last = NIL THEN t.dsc := fld ELSE last.next := fld END ;
          last := fld;
          IF fld.name[0] # 0X THEN fld.expo := TRUE; InType(R, thismod, fld.type) ELSE fld.expo := FALSE; fld.type := nilType END ;
          Files.ReadNum(R, fld.val); Read(R, class)
        END ;
        IF last = NIL THEN t.dsc := obj ELSE last.next := obj END
      ELSIF form = Proc THEN
        InType(R, thismod, t.base);
        obj := NIL; np := 0; Read(R, class);
        WHILE class # 0 DO  (*parameters*)
          NEW(par); par.class := class; Read(R, readonly); par.rdo := readonly = 1; 
          InType(R, thismod, par.type); par.next := obj; obj := par; INC(np); Read(R, class)
        END ;
        t.dsc := obj; t.nofpar := np; t.size := 4
      END ;
      Files.ReadString(R, modname);
      IF modname[0] #  0X THEN  (*re-import ========*)
        Files.ReadInt(R, key); Files.ReadString(R, name);
        mod := ThisModule(modname, modname, FALSE, key);
        obj := mod.dsc;  (*search type*)
        WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
        IF obj # NIL THEN T := obj.type   (*type object found in object list of mod*)
        ELSE (*insert new type object in object list of mod*)
          NEW(obj); obj.name := name; obj.class := Typ; obj.next := mod.dsc; mod.dsc := obj; obj.type := t;
          t.mno := mod.lev; t.typobj := obj; T := t
        END ;
        typtab[ref] := T
      END
    END
  END InType;
  
  PROCEDURE Import*(VAR modid, modid1: ORS.Ident);
    VAR key: LONGINT; class, k: INTEGER;
      obj: Object;  t: Type;
      thismod: Object;
      modname, fname: ORS.Ident;
      F: Files.File; R: Files.Rider;
  BEGIN
    IF modid1 = "SYSTEM" THEN
      thismod := ThisModule(modid, modid1, TRUE,  key); DEC(nofmod);
      thismod.lev := 0; thismod.dsc := system; thismod.rdo := TRUE
    ELSE MakeFileName(fname, modid1, ".smb"); F := Files.Old(fname);
      IF F # NIL THEN
        Files.Set(R, F, 0); Files.ReadInt(R, key); Files.ReadInt(R, key); Files.ReadString(R, modname);
        thismod := ThisModule(modid, modid1, TRUE, key); thismod.rdo := TRUE;
        Read(R, class); (*version key*)
        IF class # versionkey THEN ORS.Mark("wrong version") END ;
        Read(R, class);
        WHILE class # 0 DO
          NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
          InType(R, thismod, obj.type); obj.lev := -thismod.lev;
          IF class = Typ THEN
            t := obj.type; t.typobj := obj; Read(R, k);  (*fixup bases of previously declared pointer types*)
            WHILE k # 0 DO typtab[k].base := t; Read(R, k) END
          ELSE
            IF class = Const THEN
              IF obj.type.form = Real THEN Files.ReadInt(R, obj.val) ELSE Files.ReadNum(R, obj.val) END
            ELSIF class = Var THEN Files.ReadNum(R, obj.val); obj.rdo := TRUE
            END
          END ;
          obj.next := thismod.dsc; thismod.dsc := obj; Read(R, class)
        END ;
      ELSE ORS.Mark("import not available")
      END
    END
  END Import;
  
  (*-------------------------------- Export ---------------------------------*)

  PROCEDURE Write(VAR R: Files.Rider; x: INTEGER);
  BEGIN Files.WriteByte(R, x)
  END Write;

  PROCEDURE OutType(VAR R: Files.Rider; t: Type);
    VAR obj, mod, fld, bot: Object;

    PROCEDURE OutPar(VAR R: Files.Rider; par: Object; n: INTEGER);
      VAR cl: INTEGER;
    BEGIN
      IF n > 0 THEN
        OutPar(R, par.next, n-1); cl := par.class;
        Write(R, cl);
        IF par.rdo THEN Write(R, 1) ELSE Write(R, 0) END ;
        OutType(R, par.type)
      END
    END OutPar;

    PROCEDURE FindHiddenPointers(VAR R: Files.Rider; typ: Type; offset: LONGINT);
      VAR fld: Object; i, n: LONGINT;
    BEGIN
      IF (typ.form = Pointer) OR (typ.form = NilTyp) THEN Write(R, Fld); Write(R, 0); Files.WriteNum(R, offset)
      ELSIF typ.form = Record THEN fld := typ.dsc;
        WHILE fld # NIL DO FindHiddenPointers(R, fld.type, fld.val + offset); fld := fld.next END
      ELSIF typ.form = Array THEN i := 0; n := typ.len;
        WHILE i < n DO FindHiddenPointers(R, typ.base, typ.base.size * i + offset); INC(i) END
      END
    END FindHiddenPointers;

  BEGIN
    IF t.ref > 0 THEN (*type was already output*) Write(R, -t.ref)
    ELSE obj := t.typobj;
      IF obj # NIL THEN Write(R, Ref); t.ref := Ref; INC(Ref) ELSE (*anonymous*) Write(R, 0) END ;
      Write(R, t.form);
      IF t.form = Pointer THEN OutType(R, t.base)
      ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
      ELSIF t.form = Record THEN
        IF t.base # NIL THEN OutType(R, t.base); bot := t.base.dsc ELSE OutType(R, noType); bot := NIL END ;
        IF obj # NIL THEN Files.WriteNum(R, obj.exno) ELSE Write(R, 0) END ;
        Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size);
        fld := t.dsc;
        WHILE fld # bot DO  (*fields*)
          IF fld.expo THEN
            Write(R, Fld); Files.WriteString(R, fld.name); OutType(R, fld.type); Files.WriteNum(R, fld.val)  (*offset*)
          ELSE FindHiddenPointers(R, fld.type, fld.val)
          END ;
          fld := fld.next
        END ;
        Write(R, 0)
      ELSIF t.form = Proc THEN OutType(R, t.base); OutPar(R, t.dsc, t.nofpar); Write(R, 0)
      END ;
      IF (t.mno > 0) & (obj # NIL) THEN  (*re-export, output name*)
        mod := topScope.next;
        WHILE (mod # NIL) & (mod.lev # t.mno) DO mod := mod.next END ;
        IF mod # NIL THEN Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val); Files.WriteString(R, obj.name)
        ELSE ORS.Mark("re-export not found"); Write(R, 0)
        END
      ELSE Write(R, 0)
      END
    END
  END OutType;

  PROCEDURE Export*(VAR modid: ORS.Ident; VAR newSF: BOOLEAN; VAR key: LONGINT);
    VAR x, sum, oldkey: LONGINT;
      obj, obj0: Object;
      filename: ORS.Ident;
      F, F1: Files.File; R, R1: Files.Rider;
  BEGIN Ref := Record + 1; MakeFileName(filename, modid, ".smb");
    F := Files.New(filename); Files.Set(R, F, 0);
    Files.WriteInt(R, 0); (*placeholder*)
    Files.WriteInt(R, 0); (*placeholder for key to be inserted at the end*)
    Files.WriteString(R, modid); Write(R, versionkey);
    obj := topScope.next;
    WHILE obj # NIL DO
      IF obj.expo THEN
        Write(R, obj.class); Files.WriteString(R, obj.name);
        OutType(R, obj.type);
        IF obj.class = Typ THEN
          IF obj.type.form = Record THEN
            obj0 := topScope.next;  (*check whether this is base of previously declared pointer types*)
            WHILE obj0 # obj DO
              IF (obj0.type.form = Pointer) & (obj0.type.base = obj.type) & (obj0.type.ref > 0) THEN Write(R, obj0.type.ref) END ;
              obj0 := obj0.next
            END
          END ;
          Write(R, 0)
        ELSIF obj.class = Const THEN
          IF obj.type.form = Proc THEN Files.WriteNum(R, obj.exno)
          ELSIF obj.type.form = Real THEN Files.WriteInt(R, obj.val)
          ELSE Files.WriteNum(R, obj.val)
          END
        ELSIF obj.class = Var THEN Files.WriteNum(R, obj.exno)
        END
      END ;
      obj := obj.next
    END ;
    REPEAT Write(R, 0) UNTIL Files.Length(F) MOD 4 = 0;
    FOR Ref := Record+1 TO maxTypTab-1 DO typtab[Ref] := NIL END ;
    Files.Set(R, F, 0); sum := 0; Files.ReadInt(R, x);  (* compute key (checksum) *)
    WHILE ~R.eof DO sum := sum + x; Files.ReadInt(R, x) END ;
    F1 := Files.Old(filename); (*sum is new key*)
    IF F1 # NIL THEN Files.Set(R1, F1, 4); Files.ReadInt(R1, oldkey) ELSE oldkey := sum+1 END ;
    IF sum # oldkey THEN
      IF newSF OR (F1 = NIL) THEN
        key := sum; newSF := TRUE; Files.Set(R, F, 4); Files.WriteInt(R, sum); Files.Register(F)  (*insert checksum*)
      ELSE ORS.Mark("new symbol file inhibited")
      END
    ELSE newSF := FALSE; key := sum
    END
  END Export;

  PROCEDURE Init*;
  BEGIN topScope := universe; nofmod := 1
  END Init;
  
  PROCEDURE type(ref, form: INTEGER; size: LONGINT): Type;
    VAR tp: Type;
  BEGIN NEW(tp); tp.form := form; tp.size := size; tp.ref := ref; tp.base := NIL;
    typtab[ref] := tp; RETURN tp
  END type;

  PROCEDURE enter(name: ARRAY OF CHAR; cl: INTEGER; type: Type; n: LONGINT);
    VAR obj: Object;
  BEGIN NEW(obj); obj.name := name; obj.class := cl; obj.type := type; obj.val := n; obj.dsc := NIL;
    IF cl = Typ THEN type.typobj := obj END ;
    obj.next := system; system := obj
  END enter;
  
BEGIN
  byteType := type(Byte, Int, 1);
  boolType := type(Bool, Bool, 1);
  charType := type(Char, Char,1);
  intType := type(Int, Int, 4);
  realType := type(Real, Real, 4);
  setType := type(Set, Set,4);
  nilType := type(NilTyp, NilTyp, 4);
  noType := type(NoTyp, NoTyp, 4);
  strType := type(String, String, 8);
    
  (*initialize universe with data types and in-line procedures;
    LONGINT is synonym to INTEGER, LONGREAL to REAL.
    LED, ADC, SBC; LDPSR, LDREG, REG, COND are not in language definition*)
  system := NIL;  (*n = procno*10 + nofpar*)
  enter("UML", SFunc, intType, 132);  (*functions*)
  enter("SBC", SFunc, intType, 122);
  enter("ADC", SFunc, intType, 112);
  enter("ROR", SFunc, intType, 92);
  enter("ASR", SFunc, intType, 82);
  enter("LSL", SFunc, intType, 72);
  enter("LEN", SFunc, intType, 61);
  enter("CHR", SFunc, charType, 51);
  enter("ORD", SFunc, intType, 41);
  enter("FLT", SFunc, realType, 31);
  enter("FLOOR", SFunc, intType, 21);
  enter("ODD", SFunc, boolType, 11);
  enter("ABS", SFunc, intType, 1);
  enter("LED", SProc, noType, 81);  (*procedures*)
  enter("UNPK", SProc, noType, 72);
  enter("PACK", SProc, noType, 62);
  enter("NEW", SProc, noType, 51);
  enter("ASSERT", SProc, noType, 41);
  enter("EXCL", SProc, noType, 32);
  enter("INCL", SProc, noType, 22);
  enter("DEC", SProc, noType, 11);
  enter("INC", SProc, noType, 1);
  enter("SET", Typ, setType, 0);   (*types*)
  enter("BOOLEAN", Typ, boolType, 0);
  enter("BYTE", Typ, byteType, 0);
  enter("CHAR", Typ, charType, 0);
  enter("LONGREAL", Typ, realType, 0);
  enter("REAL", Typ, realType, 0);
  enter("LONGINT", Typ, intType, 0);
  enter("INTEGER", Typ, intType, 0);
  topScope := NIL; OpenScope; topScope.next := system; universe := topScope;
  
  system := NIL;  (* initialize "unsafe" pseudo-module SYSTEM*)
  enter("H", SFunc, intType, 201);     (*functions*)
  enter("COND", SFunc, boolType, 191);
  enter("SIZE", SFunc, intType, 181);
  enter("ADR", SFunc, intType, 171);
  enter("VAL", SFunc, intType, 162);
  enter("REG", SFunc, intType, 151);
  enter("BIT", SFunc, boolType, 142);
  enter("LDREG", SProc, noType, 142);  (*procedures*)
  enter("LDPSR", SProc, noType, 131);
  enter("COPY", SProc, noType, 123);
  enter("PUT", SProc, noType, 112);
  enter("GET", SProc, noType, 102);
END ORB.
