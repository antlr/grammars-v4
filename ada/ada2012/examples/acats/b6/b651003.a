-- B651003.A
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
--
--     Check that the aspect No_Return can only be specified for a procedure
--     or generic procedure. Check that the expression for aspect No_Return
--     cannot be nonstatic. Check that the aspect No_Return cannot be specified
--     for a null procedure nor a procedure instance.
--
-- TEST DESCRIPTION:
--     We try the aspect on functions, entries, and various non-subprograms:
--     objects, exceptions, packages, and types.
--
-- CHANGE HISTORY:
--     31 Mar 17   RLB     Created test.
--
--!
procedure B651003 is

    package Pack is
       procedure Check_1
          with No_Return => Is_No_Return;                   -- OK. {16;1}

       procedure Check_2
          with No_Return => Isnt_No_Return;                 -- ERROR: {16;1}

       procedure Check_3
          with No_Return => Good_QB = 12;                   -- OK. {16;1}

       procedure Check_4
          with No_Return => Old_QB = 4;                     -- ERROR: {16;1}

       function Check_5 return Natural
          with No_Return;                                   -- ERROR: {1:8;1}

       generic
       procedure Check_6
          with No_Return;                                   -- OK. {2:8;1}

       generic
       function Check_7 return Natural
          with No_Return;                                   -- ERROR: {2:8;1}

       generic
       procedure Check_8;

       procedure Check_9 is new Check_8
          with No_Return;                                   -- ERROR: {1:8;1}

       procedure Check_A is null
          with No_Return;                                   -- ERROR: {1:8;1}

       protected type PT is
          procedure Check_P1 with No_Return;                -- OK. {11;1}
          function Check_P2 return Natural
             with No_Return;                                -- ERROR: {1:11;1}
          entry Check_P3 with No_Return;                    -- ERROR: {11;1}
       private
          Foo : Natural := 12;
       end Pt;

       Obj : Natural := 52 with No_Return;                  -- ERROR: {8;1}

       type Check_T1 is range 1 .. 10 with No_Return;       -- ERROR: {8;1}

       Check_Error : exception with No_Return;              -- ERROR: {8;1}


       -- Values used in the aspect clauses above:
       Is_No_Return : constant Boolean := True;
       Isnt_No_Return : Boolean := True;
       Good_QB : constant Natural := 12;
       Old_QB : Natural := 4;

    end Pack;

    package body Pack is
       -- Completions for the above declarations:
       procedure Check_1 is null;
       procedure Check_2 is null;
       procedure Check_3 is null;
       procedure Check_4 is null;
       function Check_5 return Natural is (0);
       procedure Check_6 is null;
       function Check_7 return Natural is (0);
       procedure Check_8 is null;

       protected body PT is
          procedure Check_P1 is null;
          function Check_P2 return Natural is (0);
          entry Check_P3 when True is
          begin
             null;
          end Check_P3;
       end PT;

    end Pack;

begin
   Pack.Check_3;
end B651003;
