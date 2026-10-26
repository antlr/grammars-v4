-- CXD4010.A
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
--      Check that if the expiration time of two open delay_alternatives
--      is the same and no other accept_alternatives are open then the
--      sequence_of_statements of the delay_alternative that is first in
--      textual order in the selective_accept is executed.
--
-- TEST DESCRIPTION: 
--      This test contains a single task that checks the various
--      combinations of delay expiration time in a select statement
--      where none of the accept_alternatives are open.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      07 Apr 96   SAIC    Initial Version for ACVC 2.1
--
--!


-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (Priority_Queuing);

-------------------  End of Configuration Pragmas --------------------


with Calendar;   use type Calendar.Time;
with Report;
procedure CXD4010 is
  Verbose : constant Boolean := False; 

  --------------------------------------------------------------

  task Checker is
    entry Start;
    entry Done;
  end Checker;

  task body Checker is
    type Test_Data is 
      record
         First_Delay_Open : Boolean;
         First_Delay_Long : Boolean;
         Second_Delay_Open : Boolean;
         Second_Delay_Long : Boolean;
         Expected : Integer;
      end record;

    -- enumeration of all the interesting cases where the
    -- two delay alternatives may have different delay 
    -- expiration times and either of the delay alternatives
    -- may be closed.
    -- The second delay alternative will be taken only if
    -- it has a shorter delay time than the first alternative
    -- or if the first alternative is closed.
    Tests : constant array (Positive range <>) of Test_Data := (
     --  1 open   long   2 open  long  expected
        (True,    False, False,  False,    1),
        (True,    False, True,   False,    1),
        (True,    True,  True,   False,    2),
        (True,    True,  True,   True,     1),
        (False,   False, True,   True,     2)   );

    Delay_Times : array (Boolean) of Calendar.Time;
    Taken : Integer;   -- which delay was executed?
    Now : Calendar.Time;

  begin
    accept Start;  --  don't start until told to do so

    for Test_No in Tests'Range loop 
      Now := Calendar.Clock;
      Delay_Times (False) := Now + 1.0;  -- short time into the future
      Delay_Times (True ) := Now + 2.0;  -- further into the future

      Taken := 0;    -- known bad value
      select
        -- satisfy 9.7.1(8) and include an accept alternative
        when Report.Ident_Bool (False) => -- never open
           accept Start;  
        or 
          when Tests (Test_No).First_Delay_Open =>
            delay until Delay_Times (Tests (Test_No).First_Delay_Long);
            Taken := 1;   -- note we were executed.
        or 
          when Tests (Test_No).Second_Delay_Open =>
            delay until Delay_Times (Tests (Test_No).Second_Delay_Long);
            Taken := 2;   -- note we were executed.
      end select;

      if Taken = Tests (Test_No).Expected then
        if Verbose then
          Report.Comment ("Test" & Integer'Image (Test_No) & " ok");
        end if;
      else
        Report.Failed ("Test" & Integer'Image (Test_No) &
                       "  taken:" & Integer'Image (Taken) &
                       "  expected:" & 
                       Integer'Image (Tests (Test_No).Expected));
      end if;
    end loop;
    accept Done;   -- let main know we finished the test
  exception
    when others =>
       Report.Failed ("unexpected exception in Checker");
  end Checker;

  --------------------------------------------------------------

begin

   Report.Test ("CXD4010", "Check that when two delay alternatives" &
                " have the same expiration time that the one that" &
                " occurs textually first is selected");

     Checker.Start;
     Checker.Done;
   
   Report.Result;

end CXD4010;
