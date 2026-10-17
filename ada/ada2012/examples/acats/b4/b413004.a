-- B413004.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--  OBJECTIVE:
--
--     Check that for the reference L.R, if L represents an object or value
--     of an non-access untagged type T or an access type designating
--     an untagged type T, and R represents a subprogram with a
--     first parameter of T, the reference is illegal even if the
--     subprogram is primitive for T.
--
--     Check that for the reference L.R, if L represents an object or value
--     of an non-access untagged type T or an access type designating
--     an untagged type T, and R represents a subprogram with a
--     first access parameter designating T, the reference is illegal
--     even if the subprogram is primitive for T.
--
-- CHANGE HISTORY:
--     27 Sep 2007 RLB Created test.
--      6 Aug 2018 RLB Corrected test to avoid problem of AI12-0289-1.
--
--!
procedure B413004 is
   package Pack is
      type SmallInt is range 0 .. 99;

      procedure IP    (X : in SmallInt);
      function IFn    (X : in SmallInt) return Integer;
      procedure IPA   (X : access SmallInt);
      function IFA    (X : access SmallInt) return Integer;

      function F_Int   return SmallInt;
      function F_AInt  return access SmallInt;

      type Rec is record
         Int : SmallInt := 99;
      end record;

      procedure RP    (X : in Rec);
      function RF     (X : in Rec) return Integer;
      procedure RPA   (X : access Rec);
      function RFA    (X : access Rec) return Integer;

      function F_Rec   return Rec;
      function F_ARec  return access Rec;

      type Arr is array (1 .. 2) of SmallInt;

      procedure AP    (X : in Arr);
      function AF     (X : in Arr) return Integer;
      procedure APA   (X : access Arr);
      function AFA    (X : access Arr) return Integer;

      function F_Arr   return Arr;
      function F_AArr  return access Arr;

      type Priv is private;

      procedure PP    (X : in Priv);
      function PF     (X : in Priv) return Integer;
      procedure PPA   (X : not null access Priv);
      function PFA    (X : not null access Priv) return Integer;

      function F_Priv   return Priv;
      function F_APriv  return access Priv;

   private
      type Priv is tagged record
         Int : SmallInt := 99;
      end record;
         -- Note: The fact that the full type is tagged is irrelevant;
         -- the private type is not tagged.
   end Pack;

   package body Pack is
      procedure IP    (X : in SmallInt) is
      begin
         null;
      end IP;

      function IFn    (X : in SmallInt) return Integer is
      begin
         return Integer(X);
      end IFn;

      procedure IPA   (X : access SmallInt) is
      begin
         null;
      end IPA;

      function IFA    (X : access SmallInt) return Integer is
      begin
         return Integer(X.all);
      end IFA;

      function F_Int   return SmallInt is
      begin
         return 0;
      end F_Int;

      function F_AInt  return access SmallInt is
      begin
         return new SmallInt'(1);
      end F_AInt;

      procedure RP    (X : in Rec) is
      begin
         null;
      end RP;

      function RF     (X : in Rec) return Integer is
      begin
         return Integer(X.Int);
      end RF;

      procedure RPA   (X : access Rec) is
      begin
         null;
      end RPA;

      function RFA    (X : access Rec) return Integer is
      begin
         return Integer(X.Int);
      end RFA;

      function F_Rec   return Rec is
      begin
         return (Int => 0);
      end F_Rec;

      function F_ARec  return access Rec is
      begin
         return new Rec'(Int => 1);
      end F_ARec;

      procedure AP    (X : in Arr) is
      begin
         null;
      end AP;

      function AF     (X : in Arr) return Integer is
      begin
         return Integer(X(1));
      end AF;

      procedure APA   (X : access Arr) is
      begin
         null;
      end APA;

      function AFA    (X : access Arr) return Integer is
      begin
         return Integer(X(1));
      end AFA;

      function F_Arr   return Arr is
      begin
         return (1, 2);
      end F_Arr;

      function F_AArr  return access Arr is
      begin
         return new Arr'(1, 2);
      end F_AArr;

      procedure PP    (X : in Priv) is
      begin
         null;
      end PP;

      function PF     (X : in Priv) return Integer is
      begin
         return Integer(X.Int);
      end PF;

      procedure PPA   (X : not null access Priv) is
      begin
         null;
      end PPA;

      function PFA    (X : not null access Priv) return Integer is
      begin
         return Integer(X.Int);
      end PFA;

      function F_Priv   return Priv is
      begin
         return (Int => 1);
      end F_Priv;

      function F_APriv  return access Priv is
      begin
         return new Priv'(Int => 1);
      end F_APriv;

   end Pack;

   R : Integer;


begin
   declare
      OI  : aliased Pack.SmallInt;
      OIA : access Pack.SmallInt := new Pack.SmallInt'(0);
      RenI : Pack.SmallInt renames OI;

      procedure Proc (P : in Pack.SmallInt) is
      begin
         Pack.IP(P);       -- OK.
         P.IP;             -- ERROR:
         R := Pack.IFn(P); -- OK.
         R := P.IFn;       -- ERROR:
      end Proc;
   begin
      Proc (OI);

      Pack.IP(OI);         -- OK.
      OI.IP;               -- ERROR:
      R := Pack.IFn(OI);   -- OK.
      R := OI.IFn;         -- ERROR:
      OIA.IP;              -- ERROR:
      R := OIA.IFn;        -- ERROR:
      RenI.IP;             -- ERROR:
      R := RenI.IFn;       -- ERROR:
      OIA.all.IPA;         -- ERROR:
      R := OIA.all.IFA;    -- ERROR:
      Pack.IP(Pack.F_Int); -- OK.
      Pack.F_Int.IP;       -- ERROR:
      R := Pack.F_Int.IFn; -- ERROR:
      Pack.F_AInt.IPA;     -- ERROR:
      R := Pack.F_AInt.IFA;-- ERROR:
   end;

   declare
      ORc : aliased Pack.Rec;
      ORA : access Pack.Rec := new Pack.Rec'(Int => 0);
      RenR : Pack.Rec renames ORc;

      procedure Proc (P : in Pack.Rec) is
      begin
         Pack.RP(P);       -- OK.
         P.RP;             -- ERROR:
         R := Pack.RF(P);  -- OK.
         R := P.RF;        -- ERROR:
      end Proc;
   begin
      Proc (ORc);

      Pack.RP(ORc);        -- OK.
      ORc.RP;              -- ERROR:
      R := Pack.RF(ORc);   -- OK.
      R := ORc.RF;         -- ERROR:
      ORA.RP;              -- ERROR:
      R := ORA.RF;         -- ERROR:
      RenR.RP;             -- ERROR:
      R := RenR.RF;        -- ERROR:
      ORA.all.RPA;         -- ERROR:
      R := ORA.all.RFA;    -- ERROR:
      Pack.RP(Pack.F_Rec); -- OK.
      Pack.F_Rec.RP;       -- ERROR:
      R := Pack.F_Rec.RF;  -- ERROR:
      Pack.F_ARec.RPA;     -- ERROR:
      R := Pack.F_ARec.RFA;-- ERROR:
   end;

   declare
      OA  : aliased Pack.Arr;
      OAA : access Pack.Arr := new Pack.Arr'(1, 2);
      RenA : Pack.Arr renames OA;

      procedure Proc (P : in Pack.Arr) is
      begin
         Pack.AP(P);       -- OK.
         P.AP;             -- ERROR:
         R := Pack.AF(P);  -- OK.
         R := P.AF;        -- ERROR:
      end Proc;
   begin
      Proc (OA);

      Pack.AP(OA);         -- OK.
      OA.AP;               -- ERROR:
      R := Pack.AF(OA);    -- OK.
      R := OA.AF;          -- ERROR:
      OAA.AP;              -- ERROR:
      R := OAA.AF;         -- ERROR:
      RenA.AP;             -- ERROR:
      R := RenA.AF;        -- ERROR:
      OAA.all.APA;         -- ERROR:
      R := OAA.all.AFA;    -- ERROR:
      Pack.AP(Pack.F_Arr); -- OK.
      Pack.F_Arr.AP;       -- ERROR:
      R := Pack.F_Arr.AF;  -- ERROR:
      Pack.F_AArr.APA;     -- ERROR:
      R := Pack.F_AArr.AFA;-- ERROR:
   end;

   declare
      OP  : aliased Pack.Priv;
      OPA : access Pack.Priv := new Pack.Priv;
      RenP : Pack.Priv renames OP;

      procedure Proc (P : in Pack.Priv) is
      begin
         Pack.PP(P);       -- OK.
         P.PP;             -- ERROR:
         R := Pack.PF(P);  -- OK.
         R := P.PF;        -- ERROR:
      end Proc;
   begin
      Proc (OP);

      Pack.PP(OP);         -- OK.
      OP.PP;               -- ERROR:
      R := Pack.PF(OP);    -- OK.
      R := OP.PF;          -- ERROR:
      OPA.PP;              -- ERROR:
      R := OPA.PF;         -- ERROR:
      RenP.PP;             -- ERROR:
      R := RenP.PF;        -- ERROR:
      OPA.all.PPA;         -- ERROR:
      R := OPA.all.PFA;    -- ERROR:
      Pack.PP(Pack.F_Priv);-- OK.
      Pack.F_Priv.PP;      -- ERROR:
      R := Pack.F_Priv.PF; -- ERROR:
      Pack.F_APriv.PPA;    -- ERROR:
      R := Pack.F_APriv.PFA;-- ERROR:
   end;

end B413004;
