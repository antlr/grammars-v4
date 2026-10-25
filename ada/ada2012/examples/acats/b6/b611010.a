-- B611010.A
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
--     Check that the Old attribute cannot be used inside a subprogram or
--     entry body, or within an accept statement.
--
-- CHANGE HISTORY:
--     26 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
procedure B611010 is

    procedure Swap (M, N : in out Natural)
       with Post => M'Old = N and N'Old = M;                 -- OK. {28;1}

    procedure Swap (M, N : in out Natural) is
       Tmp : Natural;
    begin
       Tmp := N;
       N := M;
       M := Tmp;
       pragma Assert (M'Old = N);                            -- ERROR: {23;6}
       if N'Old /= M then                                    -- ERROR: {11;10}
          raise Program_Error;
       end if;
    end Swap;

    protected Lock is
       function Lock_Holder return Natural;
       procedure Unlock (Id : in out Natural)
          with Post => Id = Lock_Holder'Old;                 -- OK. {29;1}
       entry Lock_It (Id : in out Natural)
          with Post => Id'Old = Lock_Holder;                 -- OK. {24;15}
    private
       Lock_Id : Natural := 0;
    end Lock;

    protected body Lock is
       function Lock_Holder return Natural is
       begin
          return Lock_Id;
       end Lock_Holder;

       procedure Unlock (Id : in out Natural) is
       begin
          if Lock_Id = Id then
             Lock_Id := 0;
          elsif Lock_Id = 0 then
             raise Program_Error with "not locked!";
          else
             raise Program_Error with "locked by other Id!";
          end if;
       end Unlock;

       entry Lock_It (Id : in out Natural) when Lock_Id = 0 is
       begin
          Lock_Id := Id;
          pragma Assert (Lock_Id'Old = 0);                   -- ERROR: {26;6}
          if Lock_Id /= Id'Old then                          -- ERROR: {25;5}
             raise Program_Error with "id not set!";
          end if;
       end Lock_It;

    end Lock;

    task Worker is
       entry Start_Job (Id : in out Natural) with
          Post => Id'Old = Id;                               -- OK. {19;6}
    end Worker;

    task body Worker is
       Current_Id : Natural := 0;
    begin
       loop
          select
             accept Start_Job (Id : in out Natural) do
                Current_Id := Id;
                -- Do the job.
                if Current_Id'Old /= 0 then                  -- ERROR: {20;10}
                   Id := Id / 2 + 2;
                end if;
                pragma Assert (Id'Old = Current_Id);         -- ERROR: {32;15}
             end Start_Job;
          or
             terminate;
          end select;
       end loop;
    end Worker;

begin
   declare
      QB1 : Natural := 4;
      QB2 : Natural := 12;
   begin
      Swap (QB1, QB2);
      pragma Assert (QB1'Old = QB2);                         -- ERROR: {22;8}
   end;
   declare
      My_Id : Natural := 12;
   begin
      Lock.Lock_It (Id => My_Id);
      if Lock.Lock_Holder'Old /= My_Id then                  -- ERROR: {10;14}
         raise Program_Error;
      end if;
      Worker.Start_Job (My_Id);
      pragma Assert (My_Id'Old = My_Id);                     -- ERROR: {22;10}
      Lock.Unlock (Id => My_Id);
      if My_Id'Old /= My_Id then                             -- ERROR: {10;14}
         raise Program_Error;
      end if;
   end;

end B611010;
