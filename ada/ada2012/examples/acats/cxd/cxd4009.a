-- CXD4009.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that when multiple alternatives of a selective_accept
--      have queued callers and the callers are all of different
--      priority then the accept_alternative that has the highest
--      priority task waiting is selected.
--
-- TEST DESCRIPTION:
--      This test contains declarations for several tasks, each of
--      which is at a different priority.  All of these tasks enqueue
--      themselves on a task entry of a single task.
--      This single task has a selective_accept that will block the
--      tasks until all the tasks are enqueued on the various entries.
--      Once all the tasks are enqueued, the main procedure
--      unblocks the entry queues.  The order in which the entries
--      are serviced is checked.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      22 SEP 95   SAIC    ACVC 2.1
--      06 JAN 98   EDS     Modify priority of Checker so that its priority
--                          is larger than other tasks.
--      07 JAN 05   RLB     Added a second set of entries to eliminate the
--                          race condition.

--!


-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (Priority_Queuing);

-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with ImpDef;
with Ada.Dynamic_Priorities;
procedure CXD4009 is
  Verbose : constant Boolean := False;

  --------------------------------------------------------------

  task Checker is
    -- order of declaration is not important so they are
    -- not declared in the order expected.
    entry E2;
    entry E3;
    entry E1;
    entry E5;
    entry E4;
    entry E3_2;
    entry E1_2;
    entry E5_2;
    entry E2_2;
    entry E4_2;
    entry Start_Section;
    entry Open_The_Gates;
    entry Section_Done;
    pragma Priority (System.Default_Priority + 3);
  end Checker;

  task body Checker is
    Last_Accepted : Integer := 0;
    Checked : Integer := 0;
    Gate : Boolean := False;
  begin
    -- check for order 1-5
    -- textual order is not the same as the priority order
    accept Start_Section;  -- enables polling loop
    loop  -- poll for all tasks waiting
      select
         when Gate =>
           accept E1 do
             if Last_Accepted >= 1 then
               Report.Failed ("E1 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 1;
             Checked := Checked + 1;
           end E1;
      or
         when Gate =>
           accept E3 do
             if Last_Accepted >= 3 then
               Report.Failed ("E3 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 3;
             Checked := Checked + 1;
           end E3;
      or
         when Gate =>
           accept E4 do
             if Last_Accepted >= 4 then
               Report.Failed ("E4 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 4;
             Checked := Checked + 1;
           end E4;
      or
         when Gate =>
           accept E2 do
             if Last_Accepted >= 2 then
               Report.Failed ("E2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 2;
             Checked := Checked + 1;
           end E2;
      or
         when Gate =>
           accept E5 do
             if Last_Accepted >= 5 then
               Report.Failed ("E5 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 5;
             Checked := Checked + 1;
           end E5;
      or
        -- allow test to start once everyone is here
        when E1'Count = 1 and
             E2'Count = 1 and
             E3'Count = 1 and
             E4'Count = 1 and
             E5'Count = 1 =>
          accept Open_The_Gates;
          Gate := True;
      else
        delay ImpDef.Clear_Ready_Queue;  -- poll
      end select;

      if Verbose then
        Report.Comment ("Have Checked" & Integer'Image (Checked) &
                        "  Last_Accepted" &
                        Integer'Image (Last_Accepted));
      end if;

      exit when Checked = 5;
    end loop;

    accept Section_Done;

    -- setup for next test
    Last_Accepted := 100;
    Checked := 0;
    Gate := False;

    accept Start_Section;
    -- check for order 5-1

    loop
      select
         when Gate =>
           accept E5_2 do
             if Last_Accepted <= 5 then
               Report.Failed ("E5_2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 5;
             Checked := Checked + 1;
           end E5_2;
      or
         when Gate =>
           accept E2_2 do
             if Last_Accepted <= 2 then
               Report.Failed ("E2_2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 2;
             Checked := Checked + 1;
           end E2_2;
      or
         when Gate =>
           accept E1_2 do
             if Last_Accepted <= 1 then
               Report.Failed ("E1_2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 1;
             Checked := Checked + 1;
           end E1_2;
      or
         when Gate =>
           accept E4_2 do
             if Last_Accepted <= 4 then
               Report.Failed ("E4_2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 4;
             Checked := Checked + 1;
           end E4_2;
      or
         when Gate =>
           accept E3_2 do
             if Last_Accepted <= 3 then
               Report.Failed ("E3_2 taken after" &
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 3;
             Checked := Checked + 1;
           end E3_2;
      or
        -- allow test to start once everyone is here
        when E1_2'Count = 1 and
             E2_2'Count = 1 and
             E3_2'Count = 1 and
             E4_2'Count = 1 and
             E5_2'Count = 1 =>
          accept Open_The_Gates;
          Gate := True;

      else
        delay ImpDef.Clear_Ready_Queue;  -- poll
      end select;

      if Verbose then
        Report.Comment ("Have Checked" & Integer'Image (Checked) &
                        "  Last_Accepted" &
                        Integer'Image (Last_Accepted));
      end if;

      exit when Checked = 5;
    end loop;

    accept Section_Done;

  end Checker;

  --------------------------------------------------------------

  task type T(Id : Natural) is
    -- make the priorities surround the default priority
    -- The priorities are set for the first test where they
    -- need to be in reverse order of Id
    pragma Priority (System.Default_Priority + 3 - Id);
  end T;

  task body T is
  begin
    -- The first check expects E1 to go first and E5 last.
    case Id is
      when 1 => Checker.E1;
      when 2 => Checker.E2;
      when 3 => Checker.E3;
      when 4 => Checker.E4;
      when 5 => Checker.E5;
      when others => Report.Failed ("bad Id in task");
    end case;

    -- The second check expects E5_2 to go first and E1_2 last.
    -- (Note: We use a second set of entries; if we didn't, we could queue
    -- again before the first subtest is finished, causing havoc.)
    -- We change our priority to make correspond to the Id
    -- instead of the reverse order of Id.
    Ada.Dynamic_Priorities.Set_Priority (System.Default_Priority - 3 + Id);

    case Id is
      when 1 => Checker.E1_2;
      when 2 => Checker.E2_2;
      when 3 => Checker.E3_2;
      when 4 => Checker.E4_2;
      when 5 => Checker.E5_2;
      when others => Report.Failed ("bad Id in task");
    end case;

  exception
    when others =>
       Report.Failed ("exception in task" & Integer'Image (Id));
  end T;

begin

   Report.Test ("CXD4009", "Check that task" &
                " entries are serviced in priority order when" &
                " all waiting tasks are of different priority");


   declare  -- encapsulate the test
      -- tasks not declared in order to prevent the source order
      -- of the declarations from affecting the outcome
      T3 : T(3);
      T1 : T(1);
      T5 : T(5);
      T2 : T(2);
      T4 : T(4);
   begin
     -- 1-5 check
     Checker.Start_Section;
     Checker.Open_The_Gates;
     Checker.Section_Done;

     if Verbose then
       Report.Comment ("starting check for order 5-1");
     end if;

     -- 5-1 check
     Checker.Start_Section;
     Checker.Open_The_Gates;
     Checker.Section_Done;
   end; -- declare

   Report.Result;

end CXD4009;
