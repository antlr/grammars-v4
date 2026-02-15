-- B611009.A
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
--     Check that F'Result is not allowed in the body of F, including in a
--     pragma Assert.
--
--     Check that F'Result is not allowed in the postcondition expression for
--     a nested function.
--
-- CHANGE HISTORY:
--     24 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
procedure B611009 is

    function Test (N : in Natural) return Natural
       with Post => Test'Result > N;                         -- OK. {21;5}

    function Test (N : in Natural) return Natural is
       Obj : Natural := Test'Result;                         -- ERROR: {25;1}

       function Foobar (C : in Character) return Natural
          with Post => Test'Result > 10;                     -- ERROR: {24;6}

       function Foobar (C : in Character) return Natural is
       begin
          return Character'Pos(C);
       end Foobar;

       type Root is tagged null record;

       function Blah (R : in Root) return Natural
          with Post'Class => Test'Result > 4;                -- ERROR: {30;5}

       function Blah (R : in Root) return Natural is
       begin
          return N;
       end Blah;

    begin
       return Ret : Natural do
          if Test'Result > 0 then                            -- ERROR: {14;9}
             Ret := N;
          else
             Ret := 0;
          end if;
          pragma Assert (Test'Result in 1 .. 10);            -- ERROR: {26;13}
       end return;
    end Test;

begin
   null;
end B611009;
