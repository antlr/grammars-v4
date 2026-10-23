-- B851007.A
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
-- OBJECTIVES:
--     Check that an object renaming declaration is illegal if it
--     renames a discriminant-dependent component or subcomponent
--     if the enclosing record type has an unconstrained nominal
--     subtype and the object is not known to be constrained.
--     Part C: Generic units.
--
-- TEST DESCRIPTION:
--     This test follows the same outline as test B851005. We included test
--     cases from legacy tests B85003A and B85003B.
--
--     We started with the outline of legacy test B85002A, and added additional
--     cases to cover cases of known to be constrained not imagined in 1988.
--
--     Discriminant dependent components can be:
--     (1) A component with a discriminant constraint using a discriminant.
--     (2) A component with an index constraint using a discriminant.
--     (3) A component declared in a variant.
--     We indicate components that are none of these by "Not DDC".
--
--     No enclosing object cannot have an unconstrained-but-definite
--         nominal subtype that's not immutably limited and be:
--     (A) A component of a containing composite type (of any other of these
--         choices) [try both array and record cases];
--     (B) A variable stand-alone object;
--     (C) A formal "in out" or "out" parameter of a subprogram or entry;
--     (D) A generic formal in out parameter (note: the subtype is
--         unconstrained by 12.4(9/2) even if the named subtype is
--         constrained);
--     (E) A nonconstant return object in an extended return statement;
--     (F) A dereference of a general access-to-object type (try an
--         access-to-constant case);
--     (G) A dereference of a pool-specific access-to-object type if the
--         designated type has a constrained partial view;
--     (H) A qualified expression of any of these cases (by AI12-0228-1).
--     Additionally, in a generic body, we assume that a type descended from
--     an untagged generic formal private or derived type is not indefinite
--     and has a constrained partial view. (Thus, if the type is unconstrained,
--     all of the cases noted above are illegal.) Also, formal access types
--     are not pool-specific. We also check that rechecking of the
--     above is done in generic instances.
--
--     All of the following can be the enclosing object, regardless of the
--     subtype:
--     (R) Objects of constrained subtypes (except generic formal "in out"
--         parameters);
--     (S) Any object of a immutably limited type;
--     (T) Any object of an indefinite type;
--     (U) A dereference of a pool-specific access-to-object type if the
--         designated type does not have a constrained partial view;
--     (V) A formal "in" parameter of a subprogram or entry;
--     (W) A constant return object in an extended return statement;
--     (X) A qualified aggregate;
--     (Y) A function call;
--     (Z) A stand-alone constant.
--     (Recall that the rules need to pass for *all* enclosing objects of the
--     renamed component; these cases don't automatically make the rename legal
--     if there are other enclosing objects as well). We only try a few of
--     these cases; C-Tests ought to be used to test these legal cases.
--
--     This test includes tests for all three kinds of components,
--     and cases D and Z combined with modifiers A and H. We also test some
--     of the generic body cases.
--
-- CHANGE HISTORY:
--     08 Jun 2018  RLB  Created new test using outline of B851005 plus
--                       legacy tests B85003A and B85003B.
--     28 Sep 2018  RLB  Fixed test errors.

procedure B851007 is

   type R0 (D : Integer) is record
      F : Integer := D;
   end record;

   type A0 is array (Integer range <>) of Integer;

   type Rec (D : Integer := 1) is
      record
         F1 : Integer;
         F2 : R0(D);
         F3 : A0(1 .. D);
         case D is
            when 1 =>
               F4 : Integer range -10 .. 0;
               F5 : A0(1..5);
            when others =>
               F6 : Float;
               F7 : R0(5);
           end case;
      end record;

   R1 : Rec := (D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                F4 => -10, F5 => (1,2,3,4,5));

   R2 : Rec := (2, 10, (2,2), (1,2), 2.71, (5,5));

   subtype SubRec1 is Rec(1);
   subtype SubRec2 is Rec(2);

   type Indef_Rec (D : Integer) is
      record
         F1 : Integer;
         F2 : R0(D);
         F3 : A0(1 .. D);
         case D is
            when 1 =>
               F4 : Integer range -10 .. 0;
               F5 : A0(1..5);
            when others =>
               F6 : Float;
               F7 : R0(5);
           end case;
      end record;

   generic
      GR1, GR2 : in out Rec;
      SR1 : in out SubRec1;
      SR2 : in out Subrec2;
      IR1 : in out Indef_Rec;
      CR1 : in Rec;
   package Gen_Pack_1 is
      X101 : Integer renames GR1.F1;             -- OK. {7;1} (D, not DDC)
      X102 : R0 renames GR1.F2;                  -- ERROR: {7;1} (D1)
      X103 : A0 renames GR1.F3;                  -- ERROR: {7;1} (D2)
      X104 : Integer renames GR1.F4;             -- ERROR: {7;1} (D3)
      X105 : A0 renames GR1.F5;                  -- ERROR: {7;1} (D3)
      X106 : R0 renames GR2.F2;                  -- ERROR: {7;1} (D1)
      X107 : A0 renames GR2.F3;                  -- ERROR: {7;1} (D2)
      X108 : Float renames GR2.F6;               -- ERROR: {7;1} (D3)
      X109 : R0 renames GR2.F7;                  -- ERROR: {7;1} (D3)
      X110 : Integer renames GR2.F2.F;           -- ERROR: {7;1} (AD1)
      X111 : Integer renames GR2.F3(1);          -- ERROR: {7;1} (AD2)
      X112 : Integer renames GR2.F7.F;           -- ERROR: {7;1} (AD3)
      X113 : R0 renames Rec'(GR1).F2;            -- ERROR: {7;1} (DH1)
      X114 : A0 renames Rec'(GR2).F3;            -- ERROR: {7;1} (DH2)
      X115 : Integer renames Rec'(GR1).F4;       -- ERROR: {7;1} (DH3)

      X201 : Integer renames SR1.F1;             -- OK. {7;1} (D, not DDC)
      X202 : R0 renames SR1.F2;                  -- ERROR: {7;1} (D1)
      X203 : A0 renames SR1.F3;                  -- ERROR: {7;1} (D2)
      X204 : Integer renames SR1.F4;             -- ERROR: {7;1} (D3)
      X205 : A0 renames SR1.F5;                  -- ERROR: {7;1} (D3)
      X206 : R0 renames SR2.F2;                  -- ERROR: {7;1} (D1)
      X207 : A0 renames SR2.F3;                  -- ERROR: {7;1} (D2)
      X208 : Float renames SR2.F6;               -- ERROR: {7;1} (D3)
      X209 : R0 renames SR2.F7;                  -- ERROR: {7;1} (D3)
      X210 : Integer renames SR2.F2.F;           -- ERROR: {7;1} (AD1)
      X211 : Integer renames SR2.F3(1);          -- ERROR: {7;1} (AD2)
      X212 : Integer renames SR2.F7.F;           -- ERROR: {7;1} (AD3)
      X213 : R0 renames Rec'(SR1).F2;            -- ERROR: {7;1} (DH1)
      X214 : A0 renames Rec'(SR2).F3;            -- ERROR: {7;1} (DH2)
      X215 : Integer renames Rec'(SR1).F4;       -- ERROR: {7;1} (DH3)

      X301 : R0 renames IR1.F2;                  -- OK. {7;1} (T1)
      X302 : A0 renames IR1.F3;                  -- OK. {7;1} (T2)
      X303 : R0 renames IR1.F7;                  -- OK. {7;1} (T3)
      X304 : Integer renames IR1.F2.F;           -- OK. {7;1} (AT1)
      X305 : Integer renames IR1.F3(1);          -- OK. {7;1} (AT2)
      X306 : Integer renames IR1.F7.F;           -- OK. {7;1} (AT3)
      X307 : Integer renames IR1.F2.F;           -- OK. {7;1} (AT1)
      X308 : Integer renames IR1.F3(1);          -- OK. {7;1} (AT2)
      X309 : Integer renames IR1.F7.F;           -- OK. {7;1} (AT3)
      X310 : R0 renames Indef_Rec'(IR1).F2;      -- OK. {7;1} (TH1)
      X311 : A0 renames Indef_Rec'(IR1).F3;      -- OK. {7;1} (TH2)
      X312 : R0 renames Indef_Rec'(IR1).F7;      -- OK. {7;1} (TH3)

      -- Note: Generic formal in parameters are considered stand-alone
      -- constants.

      X401 : R0 renames CR1.F2;                  -- OK. {7;1} (Z1)
      X402 : A0 renames CR1.F3;                  -- OK. {7;1} (Z2)
      X403 : R0 renames CR1.F7;                  -- OK. {7;1} (Z3)
      X404 : Integer renames CR1.F2.F;           -- OK. {7;1} (AZ1)
      X405 : Integer renames CR1.F3(1);          -- OK. {7;1} (AZ2)
      X406 : Integer renames CR1.F7.F;           -- OK. {7;1} (AZ3)
      X407 : Integer renames CR1.F2.F;           -- OK. {7;1} (AZ1)
      X408 : Integer renames CR1.F3(1);          -- OK. {7;1} (AZ2)
      X409 : Integer renames CR1.F7.F;           -- OK. {7;1} (AZ3)
      X410 : R0 renames Rec'(CR1).F2;            -- OK. {7;1} (ZH1)
      X411 : A0 renames Rec'(CR1).F3;            -- OK. {7;1} (ZH2)
      X412 : R0 renames Rec'(CR1).F7;            -- OK. {7;1} (ZH3)

   end Gen_Pack_1;

   -- A formal access type is considered not pool-specific in a generic
   -- body. ("Assume-the-worst").

   generic
      type FAcc is access Rec;
   package Gen_Pack_2 is

      procedure Dooey (P1 : in FAcc);

   end Gen_Pack_2;


   package body Gen_Pack_2 is

      procedure Dooey (P1 : in FAcc) is

         G2X01 : Integer renames P1.F1;        -- OK. {10;1} (F, not DDC)
         G2X02 : R0 renames P1.F2;             -- ERROR: {10;1} (F1)
         G2X03 : A0 renames P1.F3;             -- ERROR: {10;1} (F2)
         G2X04 : Integer renames P1.F4;        -- ERROR: {10;1} (F3)
         G2X05 : A0 renames P1.F5;             -- ERROR: {10;1} (F3)
         G2X06 : R0 renames P1.all.F2;         -- ERROR: {10;1} (F1)
         G2X07 : A0 renames P1.all.F3;         -- ERROR: {10;1} (F2)
         G2X08 : Float renames P1.all.F6;      -- ERROR: {10;1} (F3)
         G2X09 : R0 renames P1.F7;             -- ERROR: {10;1} (F3)
         G2X10 : Integer renames P1.F2.F;      -- ERROR: {10;1} (AF1)
         G2X11 : Integer renames P1.F3(1);     -- ERROR: {10;1} (AF2)
         G2X12 : Integer renames P1.F7.F;      -- ERROR: {10;1} (AF3)
         G2X13 : R0 renames FAcc'(P1).F2;      -- ERROR: {10;1} (HF1)
         G2X14 : A0 renames FAcc'(P1).F3;      -- ERROR: {10;1} (HF2)
         G2X15 : Integer renames FAcc'(P1).F4; -- ERROR: {10;1} (HF3)
         G2X16 : Integer renames Rec'(P1.all).F1; -- OK. {10;1} (FH, not DDC)
         G2X17 : R0 renames Rec'(P1.all).F2;   -- ERROR: {10;1} (FH1)
         G2X18 : A0 renames Rec'(P1.all).F3;   -- ERROR: {10;1} (FH2)
         G2X19 : Integer renames Rec'(P1.all).F4;-- ERROR: {10;1} (FH3)
      begin
         null;
      end Dooey;

   end Gen_Pack_2;


   -- A formal access type is considered pool-specific in a generic
   -- specification and is (re)checked in an instance of that specification.
   -- ("Assume-the-best").
   -- Note: A formal access type is considered pool-specific if there is no
   -- general_access_modifier, even though the actual type may be a general
   -- access type. This follows from the syntax of a formal access type using
   -- "access_type_definition", and RM 3.10(10) gives the rules that determine
   -- when such a definition is pool-specific.

   generic
      type FAcc is access Rec;
      VAcc : in FAcc;
   package Gen_Pack_3 is

      G3X01 : Integer renames VAcc.F1;           -- OK. {7;1} (U, not DDC)
      G3X02 : R0 renames VAcc.F2;                -- OK. {7;1} (U1)
      G3X03 : A0 renames VAcc.F3;                -- OK. {7;1} (U2)
      G3X04 : Integer renames VAcc.F4;           -- OK. {7;1} (U3)
      G3X05 : A0 renames VAcc.F5;                -- OK. {7;1} (U3)
      G3X06 : R0 renames VAcc.all.F2;            -- OK. {7;1} (U1)
      G3X07 : A0 renames VAcc.all.F3;            -- OK. {7;1} (U2)
      G3X08 : Float renames VAcc.all.F6;         -- OK. {7;1} (U3)
      G3X09 : R0 renames VAcc.F7;                -- OK. {7;1} (U3)
      G3X10 : Integer renames VAcc.F2.F;         -- OK. {7;1} (AU1)
      G3X11 : Integer renames VAcc.F3(1);        -- OK. {7;1} (AU2)
      G3X12 : Integer renames VAcc.F7.F;         -- OK. {7;1} (AU3)
      G3X13 : R0 renames FAcc'(VAcc).F2;         -- OK. {7;1} (HU1)
      G3X14 : A0 renames FAcc'(VAcc).F3;         -- OK. {7;1} (HU2)
      G3X15 : Integer renames FAcc'(VAcc).F4;    -- OK. {7;1} (HU3)
      G3X16 : Integer renames Rec'(VAcc.all).F1; -- OK. {7;1} (UH, not DDC)
      G3X17 : R0 renames Rec'(VAcc.all).F2;      -- OK. {7;1} (UH1)
      G3X18 : A0 renames Rec'(VAcc.all).F3;      -- OK. {7;1} (UH2)
      G3X19 : Integer renames Rec'(VAcc.all).F4; -- OK. {7;1} (UH3)

      -- Note: All of these but G3X01 and G3X16 are illegal if the actual for
      -- FAcc is a general access type.
   end Gen_Pack_3;

   type Acc1_Rec is access Rec; -- Pool-specific access type
   type Acc2_Rec is access all Rec; -- General access type

   V1 : Acc1_Rec;
   V2 : Acc2_Rec;

   package Inst1 is new Gen_Pack_3 (Acc1_Rec, V1); -- OK. {4;1}
      -- This is case U in this instance.

   package Inst2 is new Gen_Pack_3 (Acc2_Rec, V2); -- ERROR: {4;1}
      -- This is case F in this instance. See note at end of Gen_Pack_3 for
      -- which declarations are in error.

   -- A descendant of an untagged formal derived type is not considered
   -- indefinite and is considered to have a constrained partial view in
   -- a generic body.
   -- Note: This pair of examples is inspired by the example in the AARM,
   -- 8.5.1(5.a.1-5/1).

   generic
      type GDer is new Indef_Rec;
   package Gen_Pack_4 is

      procedure Looey (P3, P4 : in out GDer);

   end Gen_Pack_4;


   package body Gen_Pack_4 is

      procedure Looey (P3, P4 : in out GDer) is

         G4X01 : Integer renames P3.F1;          -- OK. {10;1} (C, not DDC)
         G4X02 : R0 renames P3.F2;               -- ERROR: {10;1} (C1)
         G4X03 : A0 renames P3.F3;               -- ERROR: {10;1} (C2)
         G4X04 : Integer renames P3.F4;          -- ERROR: {10;1} (C3)
         G4X05 : A0 renames P3.F5;               -- ERROR: {10;1} (C3)
         G4X06 : R0 renames P4.F2;               -- ERROR: {10;1} (C1)
         G4X07 : A0 renames P4.F3;               -- ERROR: {10;1} (C2)
         G4X08 : Float renames P4.F6;            -- ERROR: {10;1} (C3)
         G4X09 : R0 renames P4.F7;               -- ERROR: {10;1} (C3)
         G4X10 : Integer renames P4.F2.F;        -- ERROR: {10;1} (AC1)
         G4X11 : Integer renames P4.F3(1);       -- ERROR: {10;1} (AC2)
         G4X12 : Integer renames P4.F7.F;        -- ERROR: {10;1} (AC3)
         G4X13 : R0 renames GDer'(P3).F2;        -- ERROR: {10;1} (CH1)
         G4X14 : A0 renames GDer'(P4).F3;        -- ERROR: {10;1} (CH2)
         G4X15 : Integer renames GDer'(P3).F4;   -- ERROR: {10;1} (CH3)

         -- Check that GDer is treated as having a constrained partial view:

         type GAcc is access GDer; -- Pool-specific.
         VG : GAcc;

         A4X01 : Integer renames VG.F1;          -- OK. {10;1} (aG, not DDC)
         A4X02 : R0 renames VG.F2;               -- ERROR: {10;1} (aG1)
         A4X03 : A0 renames VG.F3;               -- ERROR: {10;1} (aG2)
         A4X04 : Integer renames VG.F4;          -- ERROR: {10;1} (aG3)
         A4X05 : A0 renames VG.F5;               -- ERROR: {10;1} (aG3)
         A4X06 : R0 renames VG.all.F2;           -- ERROR: {10;1} (aG1)
         A4X07 : A0 renames VG.all.F3;           -- ERROR: {10;1} (aG2)
         A4X08 : Float renames VG.all.F6;        -- ERROR: {10;1} (aG3)
         A4X09 : R0 renames VG.F7;               -- ERROR: {10;1} (aG3)
         A4X10 : Integer renames VG.F2.F;        -- ERROR: {10;1} (aAG1)
         A4X11 : Integer renames VG.F3(1);       -- ERROR: {10;1} (aAG2)
         A4X12 : Integer renames VG.F7.F;        -- ERROR: {10;1} (aAG3)
         A4X13 : R0 renames GAcc'(VG).F2;        -- ERROR: {10;1} (aHG1)
         A4X14 : A0 renames GAcc'(VG).F3;        -- ERROR: {10;1} (aHG2)
         A4X15 : Integer renames GAcc'(VG).F4;   -- ERROR: {10;1} (aHG3)
         A4X16 : Integer renames GDer'(VG.all).F1; -- OK. {10;1} (aGH, not DDC)
         A4X17 : R0 renames GDer'(VG.all).F2;    -- ERROR: {10;1} (aGH1)
         A4X18 : A0 renames GDer'(VG.all).F3;    -- ERROR: {10;1} (aGH2)
         A4X19 : Integer renames GDer'(VG.all).F4;-- ERROR: {10;1} (aGH3)


      begin
         null;
      end Looey;

   end Gen_Pack_4;


   -- The "known to be constrained" rule is (re)checked in a generic instance
   -- for a formal derived type.

   generic
      type GDer is new Indef_Rec;
      with function Constructor (D : in Integer) return GDer;
   package Gen_Pack_5 is

      V51 : GDer := Constructor (1);

      V52 : GDer := Constructor (2);

      G5X01 : Integer renames V51.F1;            -- OK. {7;1} (T, not DDC)
      G5X02 : R0 renames V51.F2;                 -- OK. {7;1} (T1)
      G5X03 : A0 renames V51.F3;                 -- OK. {7;1} (T2)
      G5X04 : Integer renames V51.F4;            -- OK. {7;1} (T3)
      G5X05 : A0 renames V51.F5;                 -- OK. {7;1} (T3)
      G5X06 : R0 renames V52.F2;                 -- OK. {7;1} (T1)
      G5X07 : A0 renames V52.F3;                 -- OK. {7;1} (T2)
      G5X08 : Float renames V52.F6;              -- OK. {7;1} (T3)
      G5X09 : R0 renames V52.F7;                 -- OK. {7;1} (T3)
      G5X10 : Integer renames V52.F2.F;          -- OK. {7;1} (AT1)
      G5X11 : Integer renames V52.F3(1);         -- OK. {7;1} (AT2)
      G5X12 : Integer renames V52.F7.F;          -- OK. {7;1} (AT3)
      G5X13 : R0 renames GDer'(V51).F2;          -- OK. {7;1} (TH1)
      G5X14 : A0 renames GDer'(V52).F3;          -- OK. {7;1} (TH2)
      G5X15 : Integer renames GDer'(V51).F4;     -- OK. {7;1} (TH3)

      -- Note: All of these except G5X01 are illegal if the actual for
      -- GDer is a definite type.

   end Gen_Pack_5;


   function Struct (D : in Integer) return Indef_Rec is
      (if D = 1 then (D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                      F4 => -10, F5 => (1,2,3,4,5))
       elsif D = 2 then (2, 10, (2,2), (1,2), 2.71, (5,5))
       else raise Program_Error);

   package Inst3 is new Gen_Pack_5 (Indef_Rec, Struct); -- OK. {4;1}
      -- This is case T in this instance.

   type Def_Rec (D2 : Integer := 1) is new Indef_Rec (D => D2);

   function AStruct (D : in Integer) return Def_Rec is
      (if D = 1 then (D2 => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                      F4 => -10, F5 => (1,2,3,4,5))
       elsif D = 2 then (2, 10, (2,2), (1,2), 2.71, (5,5))
       else raise Program_Error);

   package Inst4 is new Gen_Pack_5 (Def_Rec, AStruct); -- ERROR: {4;1}
      -- This is case C in this instance.


begin
   null;
end B851007;
