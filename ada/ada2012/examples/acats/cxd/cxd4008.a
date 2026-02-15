-- CXD4008.A
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
--      Check that when:
--         multiple entry_barriers of a protected object become True,
--         more than one of the respective queues are nonempty, and
--         the callers are all of the same priority then
--         the entries are taken in textual order.
--      Check that when:
--         multiple alternatives of a selective_accept have queued
--           callers and 
--         the callers are all of the same priority then
--         the accept_alternative that is textually first in the
--         selective_accept is selected.
--
-- TEST DESCRIPTION: 
--      This test contains declarations for several tasks plus the
--      environment task.  All of these tasks enqueue themselves
--      on entry queues of a single protected record.  Once all the
--      tasks have had a chance to become enqueued, the main procedure
--      unblocks the entry queues.  The order in which the entries
--      are serviced is checked.  A similar check is made for the
--      selective_accept check.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      21 SEP 95   SAIC    ACVC 2.1
--      26 JUN 98   EDS     Made several changes to allow test to function
--                          as expected in a multi-processor environment.
--!


-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (Priority_Queuing);
pragma Locking_Policy (Ceiling_Locking);

-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with ImpDef;

procedure CXD4008 is
     -- priority for the server and protected objects
  Priority_High : constant System.Priority := 
           System.Default_Priority + 5;
     -- priority for the calling tasks - higher than main
  Priority_Tasks : constant System.Priority :=
           System.Default_Priority + 3;

  Verbose : constant Boolean := False; 
  Number_Of_Checks : constant := 5;
  type Failed_Flags is array (1..Number_Of_Checks) of Boolean;
  Failed : Failed_Flags := (others => True);
  Predecessor : array (1..Number_Of_Checks) of Integer :=
     (1..Number_Of_Checks => -1);

  protected Checker is
    pragma Priority (Priority_High);
    entry E1;
    entry E2;
    entry E3;
    entry E4;
    entry E5;
    entry Open_The_Gates;
  private
    Gate : Boolean := False;
    Last_Entry : Integer := 0;
  end Checker;

  protected body Checker is
    -- minor jumbling of order here.  The significant order
    -- for this part of the test is the order the entries
    -- were declared in the specification.

    entry E1 when Gate is
    begin
      Failed       (1) := Last_Entry >= 1;
      Predecessor  (1) := Last_Entry;
      Last_Entry :=  1;
    end E1;
 
    entry E3 when Gate is
    begin
      Failed       (3) := Last_Entry >= 3;
      Predecessor  (3) := Last_Entry;
      Last_Entry :=  3;
    end E3;
 
    entry E4 when Gate is
    begin
      Failed       (4) := Last_Entry >= 4;
      Predecessor  (4) := Last_Entry;
      Last_Entry :=  4;
    end E4;
 
    entry E2 when Gate is
    begin
      Failed       (2) := Last_Entry >= 2;
      Predecessor  (2) := Last_Entry;
      Last_Entry :=  2;
    end E2;
 
    entry E5 when Gate is
    begin
      Failed       (5) := Last_Entry > 5;
      Predecessor  (5) := Last_Entry;
      Last_Entry :=  5;
    end E5;
 

    entry Open_The_Gates when E1'Count = 1 and
                              E2'Count = 1 and
                              E3'Count = 1 and
                              E4'Count = 1 and
                              E5'Count = 1      is
    begin
      Gate := True;
    end Open_The_Gates;
  end Checker;


  --------------------------------------------------------------

  task Checker_2 is
    pragma Priority (Priority_High);
    -- order of declaration is not important so they are
    -- not declared in the order expected.
    entry E2;
    entry E3;
    entry E1;
    entry E5;
    entry E4;
    entry Start_Section;
    entry Open_The_Gates;
    entry Section_Done;
  end Checker_2;

  task body Checker_2 is
    Last_Accepted : Integer := 0;
    Checked : Integer := 0;
    Gate : Boolean := False;
    E1_Gate,
    E2_Gate,
    E3_Gate,
    E4_Gate,
    E5_Gate  : Boolean := True;
  begin
    -- check for order 1-5
    accept Start_Section;  -- enables polling loop
    loop  -- poll for all tasks waiting
      select
         when Gate and E1_Gate =>
           accept E1 do
             if Last_Accepted >= 1 then
               Report.Failed ("E1 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 1;
             Checked := Checked + 1;
             E1_Gate := False;
           end E1;
      or
         when Gate and E2_Gate =>
           accept E2 do
             if Last_Accepted >= 2 then
               Report.Failed ("E2 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 2;
             Checked := Checked + 1;
             E2_Gate := False;
           end E2;
      or
         when Gate and E3_Gate =>
           accept E3 do
             if Last_Accepted >= 3 then
               Report.Failed ("E3 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 3;
             Checked := Checked + 1;
             E3_Gate := False;
           end E3;
      or
         when Gate and E4_Gate =>
           accept E4 do
             if Last_Accepted >= 4 then
               Report.Failed ("E4 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 4;
             Checked := Checked + 1;
             E4_Gate := False;
           end E4;
      or
         when Gate and E5_Gate =>
           accept E5 do
             if Last_Accepted >= 5 then
               Report.Failed ("E5 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 5;
             Checked := Checked + 1;
             E5_Gate := False;
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
      or
        -- insure an open alternative (that won't be called)
        accept Start_Section;
        Report.Failed ("protocol violation");

      or 
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
           accept E5 do
             if Last_Accepted <= 5 then
               Report.Failed ("E5 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 5;
             Checked := Checked + 1;
           end E5;
      or
         when Gate =>
           accept E4 do
             if Last_Accepted <= 4 then
               Report.Failed ("E4 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 4;
             Checked := Checked + 1;
           end E4;
      or
         when Gate =>
           accept E3 do
             if Last_Accepted <= 3 then
               Report.Failed ("E3 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 3;
             Checked := Checked + 1;
           end E3;
      or
         when Gate =>
           accept E2 do
             if Last_Accepted <= 2 then
               Report.Failed ("E2 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 2;
             Checked := Checked + 1;
           end E2;
      or
         when Gate =>
           accept E1 do
             if Last_Accepted <= 1 then
               Report.Failed ("E1 taken after" & 
                              Integer'Image (Last_Accepted));
             end if;
             Last_Accepted := 1;
             Checked := Checked + 1;
           end E1;
      or
        -- allow test to start once everyone is here
        when E1'Count = 1 and
             E2'Count = 1 and
             E3'Count = 1 and
             E4'Count = 1 and
             E5'Count = 1 =>
          accept Open_The_Gates;
          Gate := True;
      or
        -- insure an open alternative (that won't be called)
        accept Start_Section;
        Report.Failed ("protocol violation");

      or 
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

  end Checker_2;

  --------------------------------------------------------------

  task type T(Id : Natural) is
    pragma Priority (Priority_Tasks);
  end T;

  task body T is
  begin
    case Id is
      when 1 => Checker.E1; Checker_2.E1; Checker_2.E1;
      when 2 => Checker.E2; Checker_2.E2; Checker_2.E2;
      when 3 => Checker.E3; Checker_2.E3; Checker_2.E3;
      when 4 => Checker.E4; Checker_2.E4; Checker_2.E4;
      when 5 => Checker.E5; Checker_2.E5; Checker_2.E5;
      when others => Report.Failed ("bad Id in task");
    end case;
  end T;

begin

   Report.Test ("CXD4008", "Check that protected object and task" &
                " entries are serviced in textual order when" &
                " all waiting tasks are of equal priority");


   declare  -- encapsulate the test
      -- tasks not declared in order to prevent the source order
      -- of the declarations from affecting the outcome
      T3 : T(3);
      T1 : T(1);
      T5 : T(5);
      T2 : T(2);
      T4 : T(4);
   begin
     -- do all the protected actions order test
     Checker.Open_The_Gates;
     Checker.E5;
     -- check the results
     for I in Failed'Range loop
       if Failed (I) then
         Report.Failed ("entry" & Integer'Image (I) &
                        " processed out of order after" &
                        Integer'Image (Predecessor (I)));
       end if;
     end loop; 
   
     if Verbose then
       Report.Comment ("starting check for order 1-5");
     end if;

     -- 1-5 check
     Checker_2.Start_Section;
     Checker_2.Open_The_Gates;
     Checker_2.Section_Done;
   
     if Verbose then
       Report.Comment ("starting check for order 5-1");
     end if;

     -- 5-1 check
     Checker_2.Start_Section;
     Checker_2.Open_The_Gates;
     Checker_2.Section_Done;
   end; -- declare
   
   Report.Result;

end CXD4008;
