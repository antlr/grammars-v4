-- CXD4007.A
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
--      Check that when multiple entry_barriers of a protected
--      object become True and more than one of the respective
--      queues are nonempty, the call with the highest priority is
--      selected.  
--      Check that a minimum of 30 different priorities can be
--      specified and that the priorities make a difference in the
--      task scheduling.
--
-- TEST DESCRIPTION: 
--      This test contains declarations for 30 tasks plus the
--      environment task.  All of these tasks enqueue themselves
--      on entry queues of a single protected record.  Once all the
--      tasks have had a chance to become enqueued, the main procedure
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
--      13 SEP 95   SAIC    ACVC 2.1
--       4 APR 96   SAIC    Commentary fixed.
--
--!


-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (Priority_Queuing);
pragma Locking_Policy (Ceiling_Locking);

-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with ImpDef;

procedure CXD4007 is

  Number_of_Levels : constant := 30;
  type Failed_Flags is array (1..Number_of_Levels) of Boolean;
  Failed : Failed_Flags := (others => True);
  Predecessor : array (1..Number_of_Levels) of Integer :=
     (1..Number_of_Levels => -1);

  protected Checker is
      -- 30 numbered entries in jumbled order so that declaration
      -- order bares no relationship to expected order for them to
      -- be taken.
    entry E25;
    entry E26;
    entry E1;
    entry E2;
    entry E3;
    entry E4;
    entry E10;
    entry E11;
    entry E12;
    entry E13;
    entry E14;
    entry E15;
    entry E5;
    entry E6;
    entry E7;
    entry E8;
    entry E9;
    entry E16;
    entry E17;
    entry E18;
    entry E19;
    entry E20;
    entry E27;
    entry E28;
    entry E29;
    entry E30;
    entry E21;
    entry E22;
    entry E23;
    entry E24;
    procedure Open_The_Gates;
  private
    Gate : Boolean := False;
    Last_Entry : Integer := Number_of_Levels + 1;
  end Checker;

  protected body Checker is
    -- minor jumbling of order here

    entry E1 when Gate is
    begin
      Failed       (1) := Last_Entry <= 1;
      Predecessor  (1) := Last_Entry;
      Last_Entry :=  1;
    end E1;
 
    entry E3 when Gate is
    begin
      Failed       (3) := Last_Entry <= 3;
      Predecessor  (3) := Last_Entry;
      Last_Entry :=  3;
    end E3;
 
    entry E4 when Gate is
    begin
      Failed       (4) := Last_Entry <= 4;
      Predecessor  (4) := Last_Entry;
      Last_Entry :=  4;
    end E4;
 
    entry E2 when Gate is
    begin
      Failed       (2) := Last_Entry <= 2;
      Predecessor  (2) := Last_Entry;
      Last_Entry :=  2;
    end E2;
 
    entry E5 when Gate is
    begin
      Failed       (5) := Last_Entry <= 5;
      Predecessor  (5) := Last_Entry;
      Last_Entry :=  5;
    end E5;
 
    entry E6 when Gate is
    begin
      Failed       (6) := Last_Entry <= 6;
      Predecessor  (6) := Last_Entry;
      Last_Entry :=  6;
    end E6;
 
    entry E7 when Gate is
    begin
      Failed       (7) := Last_Entry <= 7;
      Predecessor  (7) := Last_Entry;
      Last_Entry :=  7;
    end E7;
 
    entry E8 when Gate is
    begin
      Failed       (8) := Last_Entry <= 8;
      Predecessor  (8) := Last_Entry;
      Last_Entry :=  8;
    end E8;
 
    entry E9 when Gate is
    begin
      Failed       (9) := Last_Entry <= 9;
      Predecessor  (9) := Last_Entry;
      Last_Entry :=  9;
    end E9;
 
    entry E11 when Gate is
    begin
      Failed       (11) := Last_Entry <= 11;
      Predecessor  (11) := Last_Entry;
      Last_Entry :=  11;
    end E11;
 
    entry E10 when Gate is
    begin
      Failed       (10) := Last_Entry <= 10;
      Predecessor  (10) := Last_Entry;
      Last_Entry :=  10;
    end E10;
 
    entry E12 when Gate is
    begin
      Failed       (12) := Last_Entry <= 12;
      Predecessor  (12) := Last_Entry;
      Last_Entry :=  12;
    end E12;
 
    entry E13 when Gate is
    begin
      Failed       (13) := Last_Entry <= 13;
      Predecessor  (13) := Last_Entry;
      Last_Entry :=  13;
    end E13;
 
    entry E14 when Gate is
    begin
      Failed       (14) := Last_Entry <= 14;
      Predecessor  (14) := Last_Entry;
      Last_Entry :=  14;
    end E14;
 
    entry E15 when Gate is
    begin
      Failed       (15) := Last_Entry <= 15;
      Predecessor  (15) := Last_Entry;
      Last_Entry :=  15;
    end E15;
 
    entry E16 when Gate is
    begin
      Failed       (16) := Last_Entry <= 16;
      Predecessor  (16) := Last_Entry;
      Last_Entry :=  16;
    end E16;
 
    entry E17 when Gate is
    begin
      Failed       (17) := Last_Entry <= 17;
      Predecessor  (17) := Last_Entry;
      Last_Entry :=  17;
    end E17;
 
    entry E18 when Gate is
    begin
      Failed       (18) := Last_Entry <= 18;
      Predecessor  (18) := Last_Entry;
      Last_Entry :=  18;
    end E18;
 
    entry E19 when Gate is
    begin
      Failed       (19) := Last_Entry <= 19;
      Predecessor  (19) := Last_Entry;
      Last_Entry :=  19;
    end E19;
 
    entry E20 when Gate is
    begin
      Failed       (20) := Last_Entry <= 20;
      Predecessor  (20) := Last_Entry;
      Last_Entry :=  20;
    end E20;
 
    entry E21 when Gate is
    begin
      Failed       (21) := Last_Entry <= 21;
      Predecessor  (21) := Last_Entry;
      Last_Entry :=  21;
    end E21;
 
    entry E22 when Gate is
    begin
      Failed       (22) := Last_Entry <= 22;
      Predecessor  (22) := Last_Entry;
      Last_Entry :=  22;
    end E22;
 
    entry E23 when Gate is
    begin
      Failed       (23) := Last_Entry <= 23;
      Predecessor  (23) := Last_Entry;
      Last_Entry :=  23;
    end E23;
 
    entry E24 when Gate is
    begin
      Failed       (24) := Last_Entry <= 24;
      Predecessor  (24) := Last_Entry;
      Last_Entry :=  24;
    end E24;
 
    entry E25 when Gate is
    begin
      Failed       (25) := Last_Entry <= 25;
      Predecessor  (25) := Last_Entry;
      Last_Entry :=  25;
    end E25;
 
    entry E26 when Gate is
    begin
      Failed       (26) := Last_Entry <= 26;
      Predecessor  (26) := Last_Entry;
      Last_Entry :=  26;
    end E26;
 
    entry E27 when Gate is
    begin
      Failed       (27) := Last_Entry <= 27;
      Predecessor  (27) := Last_Entry;
      Last_Entry :=  27;
    end E27;
 
    entry E28 when Gate is
    begin
      Failed       (28) := Last_Entry <= 28;
      Predecessor  (28) := Last_Entry;
      Last_Entry :=  28;
    end E28;
 
    entry E29 when Gate is
    begin
      Failed       (29) := Last_Entry <= 29;
      Predecessor  (29) := Last_Entry;
      Last_Entry :=  29;
    end E29;
 
    entry E30 when Gate is
    begin
      Failed       (30) := Last_Entry <= 30;
      Predecessor  (30) := Last_Entry;
      Last_Entry :=  30;
    end E30;

    procedure Open_The_Gates is
    begin
      Gate := True;
    end Open_The_Gates;
  end Checker;


  task type T(Id : Natural) is
    pragma Priority (System.Priority'First + Id - 1);
  end T;

  task body T is
  begin
    case Id is
      when 1 => Checker.E1;
      when 2 => Checker.E2;
      when 3 => Checker.E3;
      when 4 => Checker.E4;
      when 5 => Checker.E5;
      when 6 => Checker.E6;
      when 7 => Checker.E7;
      when 8 => Checker.E8;
      when 9 => Checker.E9;
      when 10 => Checker.E10;
      when 11 => Checker.E11;
      when 12 => Checker.E12;
      when 13 => Checker.E13;
      when 14 => Checker.E14;
      when 15 => Checker.E15;
      when 16 => Checker.E16;
      when 17 => Checker.E17;
      when 18 => Checker.E18;
      when 19 => Checker.E19;
      when 20 => Checker.E20;
      when 21 => Checker.E21;
      when 22 => Checker.E22;
      when 23 => Checker.E23;
      when 24 => Checker.E24;
      when 25 => Checker.E25;
      when 26 => Checker.E26;
      when 27 => Checker.E27;
      when 28 => Checker.E28;
      when 29 => Checker.E29;
      when 30 => Checker.E30;
      
      when others => Report.Failed ("bad Id in task");
    end case;
  end T;

begin

   Report.Test ("CXD4007", "Check that protected object entries are" &
                " serviced based upon the priority of the tasks waiting");


   declare  -- encapsulate the test
      -- 30 tasks but not declared in order to prevent the source order
      -- of the declarations from affecting the outcome
      T26 : T(26);
      T27 : T(27);
      T28 : T(28);
      T29 : T(29);
      T30 : T(30);
      T10 : T(10);
      T11 : T(11);
      T12 : T(12);
      T13 : T(13);
      T14 : T(14);
      T15 : T(15);
      T16 : T(16);
      T17 : T(17);
      T18 : T(18);
      T19 : T(19);
      T1 : T(1);
      T2 : T(2);
      T3 : T(3);
      T4 : T(4);
      T5 : T(5);
      T6 : T(6);
      T7 : T(7);
      T8 : T(8);
      T9 : T(9);
      T25 : T(25);
      T24 : T(24);
      T23 : T(23);
      T22 : T(22);
      T21 : T(21);
      T20 : T(20);
   begin
     -- allow all the tasks to get onto the Checker queue
     delay 31 * ImpDef.Switch_To_New_Task;
     -- do all the protected actions now
     Checker.Open_The_Gates;
     -- all the protected operations should complete before
     -- the main procedure gets a chance to run again.

     -- check the results
     for I in Failed'Range loop
       if Failed (I) then
         Report.Failed ("entry" & Integer'Image (I) &
                        " processed out of order after" &
                        Integer'Image (Predecessor (I)));
       end if;
     end loop; 
   end; -- declare
   
   Report.Result;

end CXD4007;
