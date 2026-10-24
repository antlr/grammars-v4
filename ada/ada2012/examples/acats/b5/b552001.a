-- B552001.A
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
--
--*
--
-- OBJECTIVE:
--      Check that the subtype indication of an array component iterator
--      must statically match the array component subtype.
--
--      Check that the iterable_name of an array component iterator cannot
--      denote a subcomponent that depends on the discriminants of an object
--      whose nominal subtype is unconstrained, unless the object is known
--      to be constrained.
--
-- TEST DESCRIPTION:
--      Both of these rules are new to the 2015 Corrigendum; they serve to
--      eliminate anomalies from the original Ada 2012 definition.

-- CHANGE HISTORY:
--   18 Mar 2015   RLB   Initial version.
--
--!
with Report;
procedure B552001 is

    type Colors is (Red, Green, Blue);
    subtype All_Colors is Colors range Red .. Blue;
    subtype Hate_Blue is Colors range Red .. Green;

    type Small is range 1 .. 100;
    subtype Tiny is Small range 1 .. 10;
    Small_Var : Small := Small(Report.Ident_Int(10));
    subtype Dyn_Tiny is Small range 1 .. Small_Var;

    type Color_Array is array (1 .. 10) of Colors;
    CA_Obj : Color_Array;

    type Tiny_Array is array (Tiny) of Tiny;
    TA_Obj : Tiny_Array;

    subtype Alpha is Character range 'A' .. 'Z';
    Str_Obj : String (1..10);

    type Disc (D : Tiny) is null record;
    subtype SDisc is Disc(10);
    subtype DDisc is Disc(Small_Var);
    type Tiny_Disc_Array is array (Tiny) of SDisc;
    TDA_Obj : Tiny_Disc_Array;


    type Variant (C : Colors := Red) is record
       TA_Comp : Tiny_Array;
       case C is
          when Red =>
             Str_Comp : String(1..5);
          when Green =>
             TDA_Comp : Tiny_Disc_Array;
          when Blue =>
             CA_Comp : Color_Array;
       end case;
    end record;
    type Access_Variant is access Variant;

begin
    -- The subtype_indication, if any, in an array component iterator
    -- must statically match the array component subtype:

    for E : Colors of CA_Obj loop                           -- OK.
       null;
    end loop;

    for E : All_Colors of CA_Obj loop                       -- OK.
       null;
    end loop;

    for E : Hate_Blue of CA_Obj loop                        -- ERROR:
       null;
    end loop;

    for E : Colors range Red .. Blue of CA_Obj loop         -- OK.
       null;
    end loop;

    for E : Colors range Red .. Red of CA_Obj loop          -- ERROR:
       null;
    end loop;

    for E : Tiny of TA_Obj loop                             -- OK.
       null;
    end loop;

    for E : Dyn_Tiny of TA_Obj loop                         -- ERROR:
       null;
    end loop;

    for E : Small of TA_Obj loop                            -- ERROR:
       null;
    end loop;

    for E : Tiny'Base of TA_Obj loop                        -- ERROR:
       null;
    end loop;

    for E : Small range 1 .. 10 of TA_Obj loop              -- OK.
       null;
    end loop;

    for E : Small range 1 .. Small_Var of TA_Obj loop       -- ERROR:
       null;
    end loop;

    for E : Character of Str_Obj loop                       -- OK.
       null;
    end loop;

    for E : Alpha of Str_Obj loop                           -- ERROR:
       null;
    end loop;

    for E : Character range '0' .. 'z' of Str_Obj loop      -- ERROR:
       null;
    end loop;

    for E : SDisc of TDA_Obj loop                           -- OK.
       null;
    end loop;

    for E : Disc of TDA_Obj loop                            -- ERROR:
       null;
    end loop;

    for E : DDisc of TDA_Obj loop                           -- ERROR:
       null;
    end loop;

    for E : Disc(4) of TDA_Obj loop                         -- ERROR:
       null;
    end loop;

    for E : Disc(10) of TDA_Obj loop                        -- OK.
       null;
    end loop;

    -- The iterable_name of an array component iterator cannot
    -- denote a subcomponent that depends on the discriminants of an object
    -- whose nominal subtype is unconstrained, unless the object is known
    -- to be constrained.
    declare
       VAObj   : Variant;
       VRObj   : Variant(Red);
       CVRObj  : constant Variant := VRObj;
       AAVAObj : access Variant := new Variant;
       APVAObj : Access_Variant := new Variant;

       procedure Checker (PVObj : in Variant) is
       begin
          for E of PVObj.Str_Comp loop                        -- OK.
             null; -- PVObj is known-to-be-constrained (its an in parameter).
          end loop;
       end Checker;
    begin
       for E of VAObj.TA_Comp loop                            -- OK.
          null; -- TA_Comp is not discriminant-dependent.
       end loop;

       for E of VAObj.STR_Comp loop                           -- ERROR:
          null;
       end loop;

       for E of VAObj.CA_Comp loop                            -- ERROR:
          null;
       end loop;

       for E of VRObj.STR_Comp loop                           -- OK.
          null; -- VRObj is known-to-be-constrained (its subtype
                -- is constrained).
       end loop;

       for E of CVRObj.STR_Comp loop                          -- OK.
          null; -- CVRObj is known-to-be-constrained (it's a stand-alone
                -- constant).
       end loop;

       for E of AAVAObj.STR_Comp loop                          -- ERROR:
          null;
       end loop;

       for E of AAVAObj.all.STR_Comp loop                      -- ERROR:
          null;
       end loop;

       for E of APVAObj.STR_Comp loop                          -- OK.
          null; -- APVAObj.all is known-to-be-constrained (its a
                -- dereference of a pool-specific type).
       end loop;
   end;

end B552001;
