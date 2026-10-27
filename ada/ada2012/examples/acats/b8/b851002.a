-- B851002.A
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
--     For an object renaming with an anonymous access-to-object type, check
--     that the name is illegal if it does not resolve to an anonymous access
--     type with the appropriate designated type.
--
--     For an object renaming with an anonymous access-to-object type, check
--     that the renaming is illegal if the designated subtypes don't statically
--     match.
--
--     For an object renaming with an anonymous access-to-object type, check
--     that the renaming is illegal if one of the types is access-to-constant
--     and the other is access-to-variable.
--
--     For an object renaming with an anonymous access-to-object type, check
--     that the renaming is illegal if there are multiple interpretations,
--     even if only one is legal.
--
-- CHANGE HISTORY:
--     21 Jul 2008 RLB Created test.
--
with Report;
procedure B851002 is

    type Disc (D : Natural) is null record;

    -- First objective:
    Obj1 : access Integer;
    Obj2 : Integer;
    type Int_Ptr is access all Integer;
    Obj3 : Int_Ptr;

    -- Second objective:
    Obj4 : access Natural;
    subtype Dyn1 is Integer range 0 .. Report.Ident_Int(10);
    subtype Dyn2 is Integer range 0 .. Report.Ident_Int(10);
    Obj5 : access Dyn1;
    subtype Str5 is String(1..5);
    subtype Str9 is String(1..9);
    Obj6 : access Str5;
    subtype Disc10 is Disc(10);
    subtype Disc20 is Disc(20);
    subtype DiscMatch is Disc(10);
    Obj7 : access Disc10;

    -- Third objective:
    Obj8 : access constant Integer;
    Obj9 : access Integer;

    -- Fourth objective:
    type OK_Rec is record
       Comp1 : access Integer;
       Comp2 : access Disc10;
       Comp3 : access Dyn1;
       Comp4 : access constant Integer;
    end record;

    function Element return OK_Rec is
    begin
        return (others => <>);
    end Element;

    type Bad_Rec is record -- None of these will be legal, but they should
                           -- still be considered for resolution purposes.
       Comp1 : access Natural;
       Comp2 : access Disc20;
       Comp3 : access Dyn2;
       Comp4 : access Integer;
    end record;

    function Element return Bad_Rec is
    begin
        return (others => <>);
    end Element;

begin
   declare
      -- First objective:
      Ren11 : access Integer renames Obj1;                       -- OK.
      Ren21 : access Integer renames Obj2;                       -- ERROR:
      Ren31 : access Integer renames Obj3;                       -- ERROR:

      -- Second objective:
      Ren41 : access Integer renames Obj4;                       -- ERROR:
      Ren42 : access Dyn2 renames Obj4;                          -- ERROR:
      Ren43 : access Natural renames Obj4;                       -- OK.
      Ren51 : access Integer renames Obj5;                       -- ERROR:
      Ren52 : access Dyn2 renames Obj5;                          -- ERROR:
      Ren53 : access Dyn1 renames Obj5;                          -- OK.
      Ren61 : access Str9 renames Obj6;                          -- ERROR:
      Ren62 : access Str5 renames Obj6;                          -- OK.
      Ren71 : access Disc20 renames Obj7;                        -- ERROR:
      Ren72 : access DiscMatch renames Obj7;                     -- OK.

      -- Third objective:
      Ren81 : access Integer renames Obj8;                       -- ERROR:
      Ren82 : access constant Integer renames Obj8;              -- OK.
      Ren91 : access Integer renames Obj9;                       -- OK.
      Ren92 : access constant Integer renames Obj9;              -- ERROR:

      -- Fourth objective:
      RenC1 : access Integer renames Element.Comp1;              -- ERROR:
      RenC2 : access Disc10 renames Element.Comp2;               -- ERROR:
      RenC3 : access Dyn1 renames Element.Comp3;                 -- ERROR:
      RenC4 : access constant Integer renames Element.Comp4;     -- ERROR:
   begin
      null;
   end;
end B851002;
