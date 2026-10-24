-- B851003.A
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
-- OBJECTIVE:
--     For an object renaming with an anonymous access-to-subprogram type,
--     check that the name is illegal if it does not resolve to an anonymous
--     access type with the appropriate designated profile.
--
--     For an object renaming with an anonymous access-to-subprogram type, check
--     that the renaming is illegal if the designated profiles are not subtype
--     conformant.
--
--     For an object renaming with an anonymous access-to-subprogram type, check
--     that the renaming is illegal if there are multiple interpretations,
--     even if only one is legal.
--
-- CHANGE HISTORY:
--     21 Jul 2008 RLB Created test.
--
with Report;
procedure B851003 is

   type Int_Ptr is access Integer;
   subtype Dyn1 is Integer range 0 .. Report.Ident_Int(Natural'Last);
   type Disc (D : Natural) is null record;
   subtype Disc10 is Disc(10);
   subtype Disc20 is Disc(20);
   type Disc_Ptr is access all Disc;
   subtype Str5 is String(1..5);
   subtype Str9 is String(1..9);

   -- First objective:
   Obj1 : access function (X : Float) return Float;
   Obj2 : access procedure (X : Float);
   type Func_Ptr is access function (X : Float) return Float;
   Obj3 : Func_Ptr;

   -- Second objective:
   Obj4 : access function (X : Natural) return Float;
   Obj5 : access procedure (O : in out Disc10);
   Obj6 : access procedure (O : access Str5);
   Obj7 : access procedure (O : not null access Disc);
   Obj8 : access procedure (O : not null Int_Ptr);
   Obj9 : access procedure (O : Int_Ptr);
   ObjA : access procedure (O : access constant Str9);

    -- Third objective:
    type OK_Rec is record
       Comp1 : access function (X : Natural) return Float;
       Comp2 : access procedure (O : in Disc20);
       Comp3 : access procedure (O : not null Int_Ptr);
       Comp4 : access procedure (O : access constant Str9);
    end record;

    function Element return OK_Rec is
    begin
        return (others => <>);
    end Element;

    type Bad_Rec is record -- None of these will be legal, but they should
                           -- still be considered for resolution purposes.
       Comp1 : access function (X : Integer) return Float;
       Comp2 : access procedure (O : in out Disc10);
       Comp3 : access procedure (O : Int_Ptr);
       Comp4 : access procedure (O : access Str9);
    end record;

    function Element return Bad_Rec is
    begin
        return (others => <>);
    end Element;

begin

   declare
      -- First objective:
      Ren11 : access function (X : Float) return Float
                  renames Obj1;                                -- OK.
      Ren21 : access function (X : Float) return Float
                  renames Obj2;                                -- ERROR:
      Ren31 : access function (X : Float) return Float
                  renames Obj3;                                -- ERROR:

      -- Second objective:
      Ren41 : access function (X : Natural) return Float
                  renames Obj4;                                -- OK.
      Ren42 : access function (X : Integer) return Float
                  renames Obj4;                                -- ERROR:
      Ren43 : access function (X : Dyn1) return Float
                  renames Obj4;                                -- ERROR:

      Ren51 : access procedure (O : in out Disc10)
                  renames Obj5;                                -- OK.
      Ren52 : access procedure (O : in out Disc20)
                  renames Obj5;                                -- ERROR:
      Ren53 : access procedure (O : in Disc10)
                  renames Obj5;                                -- ERROR:
      Ren54 : access procedure (O : out Disc10)
                  renames Obj5;                                -- ERROR:
      Ren55 : access procedure (O : access Disc10)
                  renames Obj5;                                -- ERROR:

      Ren61 : access procedure (O : access Str5)
                  renames Obj6;                                -- OK.
      Ren62 : access procedure (O : in out Str5)
                  renames Obj6;                                -- ERROR:
      Ren63 : access procedure (O : access Str9)
                  renames Obj6;                                -- ERROR:
      Ren64 : access procedure (O : access constant Str5)
                  renames Obj6;                                -- ERROR:
      Ren65 : access procedure (O : not null access Str5)
                  renames Obj6;                                -- ERROR:

      Ren71 : access procedure (O : not null access Disc)
                  renames Obj7;                                -- OK.
      Ren72 : access procedure (O : not null access Disc20)
                  renames Obj7;                                -- ERROR:
      Ren73 : access procedure (O : access Disc)
                  renames Obj7;                                -- ERROR:
      Ren74 : access procedure (O : Disc_Ptr)
                  renames Obj7;                                -- ERROR:
      Ren75 : access procedure (O : not null Disc_Ptr)
                  renames Obj7;                                -- ERROR:

      Ren81 : access procedure (O : not null Int_Ptr)
                  renames Obj8;                                -- OK.
      Ren82 : access procedure (O : Int_Ptr)
                  renames Obj8;                                -- ERROR:
      Ren83 : access procedure (O : not null access Integer)
                  renames Obj8;                                -- ERROR:
      Ren84 : access procedure (O : in out not null Int_Ptr)
                  renames Obj8;                                -- ERROR:
      Ren85 : access procedure (O : in not null Int_Ptr)
                  renames Obj8;                                -- OK.

      Ren91 : access procedure (O : Int_Ptr)
                  renames Obj9;                                -- OK.
      Ren92 : access procedure (O : not null Int_Ptr)
                  renames Obj9;                                -- ERROR:
      Ren93 : access procedure (O : access Integer)
                  renames Obj9;                                -- ERROR:

      RenA1 : access procedure (O : access constant Str9)
                  renames ObjA;                                -- OK.
      RenA2 : access procedure (O : access constant Str5)
                  renames ObjA;                                -- ERROR:
      RenA3 : access procedure (O : access Str9)
                  renames ObjA;                                -- ERROR:


      -- Third objective:
      RenC1 : access function (X : Natural) return Float
                  renames Element.Comp1;                       -- ERROR:
      RenC2 : access procedure (O : in Disc20)
                  renames Element.Comp2;                       -- ERROR:
      RenC3 : access procedure (O : not null Int_Ptr)
                  renames Element.Comp3;                       -- ERROR:
      RenC4 : access procedure (O : access constant Str9)
                  renames Element.Comp4;                       -- ERROR:
   begin
      null;
   end;
end B851003;
