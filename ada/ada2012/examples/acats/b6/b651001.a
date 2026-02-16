-- B651001.A
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
--     Check that a return statement cannot be used in a nonreturning
--     procedure. Check that a renames-as-body that completes a
--     nonreturning procedure declaration renames a nonreturning procedure.
--     Case 1: pragma No_Return.
--
-- CHANGE HISTORY:
--     31 Mar 17   RLB     Created test.
--     29 Sep 20   RLB     Corrected pragmas.
--!
procedure B651001 is

   package Pack is

      procedure Raise_Error (Code : in Natural);
      pragma No_Return (Raise_Error);

      procedure Print_Error (Code : in Natural);
         -- Normal subprogram.

      generic
         with function Message (Error : in Natural) return String;
      procedure Generic_Raise_Error (Error : in Natural);
      pragma No_Return (Generic_Raise_Error);

      generic
         with function Message (Error : in Natural) return String;
      procedure Generic_Print_Error (Error : in Natural);

      function Message (A : in Natural) return String;

   end Pack;


   package body Pack is

      procedure Raise_Error (Code : in Natural) is
         procedure Inner is
         begin
            if Code > 10 then
               raise Constraint_Error;
            else
               return;                                     -- OK. {16;1}
            end if;
         end Inner;
      begin
         if Code = 1 then
            return;                                        -- ERROR: {13;1}
         elsif Code = 2 then
            loop
               if Code = 2 then
                  return;                                  -- ERROR: {19;1}
               else
                  raise Program_Error;
               end if;
            end loop;
         elsif Code = 3 then
            declare
               Yup : Natural := Code;
            begin
               if Yup = 3 then
                  return;                                  -- ERROR: {19;1}
               else
                  Inner;
               end if;
            end;
         end if;
         return;                                           -- ERROR: {10;1}
      end Raise_Error;


      procedure Generic_Raise_Error (Error : in Natural) is
         procedure Inner is
         begin
            if Error < 12 then
               raise Constraint_Error;
            else
               return;                                     -- OK. {16;1}
            end if;
         end Inner;
      begin
         if Error = 1 then
            return;                                        -- ERROR: {13;1}
         elsif Error = 2 then
            loop
               if Error = 2 then
                  return;                                  -- ERROR: {19;1}
               else
                  Inner;
               end if;
            end loop;
         elsif Error = 3 then
            declare
               Yup : Natural := Error;
            begin
               if Yup = 3 then
                  return;                                  -- ERROR: {19;1}
               else
                  raise Tasking_Error;
               end if;
            end;
         end if;
         return;                                           -- ERROR: {10;1}
      end Generic_Raise_Error;


      procedure Print_Error (Code : in Natural) is
      begin
         raise Program_Error;
      end Print_Error;


      procedure Generic_Print_Error (Error : in Natural) is
      begin
         raise Program_Error;
      end Generic_Print_Error;


      function Message (A : in Natural) return String is
      begin
         return "";
      end Message;

   end Pack;


   package Ren_Pack is

      procedure Inst_Raise_Error is new
                                    Pack.Generic_Raise_Error (Pack.Message);

      procedure Inst_Print_Error is new
                                    Pack.Generic_Print_Error (Pack.Message);

      procedure Check_Error_1 (Code : in Natural);
      pragma No_Return (Check_Error_1);

      procedure Check_Error_2 (Code : in Natural);
      pragma No_Return (Check_Error_2);

      procedure Check_Error_3 (Code : in Natural);
      pragma No_Return (Check_Error_3);

      procedure Check_Error_4 (Code : in Natural);
      pragma No_Return (Check_Error_4);

   private
      -- Renames-as-body checks:
      procedure Check_Error_1 (Code : in Natural)
         renames Pack.Raise_Error;                         -- OK. {1:7;1}

      procedure Check_Error_2 (Code : in Natural)
         renames Pack.Print_Error;                         -- ERROR: {1:7;1}

      procedure Check_Error_3 (Code : in Natural)
         renames Inst_Raise_Error;                         -- OK. {1:7;1}

      procedure Check_Error_4 (Code : in Natural)
         renames Inst_Print_Error;                         -- ERROR: {1:7;1}

   end Ren_Pack;

begin
   Pack.Raise_Error (4);
end B651001;
