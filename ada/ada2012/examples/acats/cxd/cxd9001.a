-- CXD9001.A
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
--      Check that when a delay_statement appears in a delay_alternative of 
--      a timed_entry_call the entry call is attempted regardless of the 
--      specified expiration time.
--
-- TEST DESCRIPTION:  
--      The main program attempts entry calls to both protected objects and
--      tasks using a select statement with a delay alternative.  The delay
--      alternatives used are both relative delay and delay until.
--      In all cases the value specified for the delay is either negative or
--      a time in the past.   
--      
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--
--
-- CHANGE HISTORY:
--     28 AUG 95   SAIC    Initial version
--
--!


with Report;
with Ada.Calendar;
with ImpDef;
with System;

procedure CXD9001 is

   Verbose     : constant Boolean := False;
   Time_in_the_Past : Ada.Calendar.Time := Ada.Calendar.Time_of (1901,1,1);
   No_Delay_Amount  : Duration := -0.1;


begin


   Report.Test ("CXD9001", "Test that a timed entry call is attempted" &
                           " regardless of the specified expiration time");


   -- Some implementations will boot with the time set to 1901/1/1/0.0
   -- This delay is such that the implementation guarantees that a 
   -- subsequent call to Ada.Calendar.Time_Of(1901,1,1) will yeild a 
   -- time that has already passed 
   --
   delay ImpDef.Delay_For_Time_Past;

   declare  -- encapsulate the test
      Count : Integer := 0;

      protected Protected_Object is 
         entry Available_Entry (Result : in out Integer);
      end Protected_Object;

      protected body Protected_Object is
         entry Available_Entry (Result : in out Integer) when True is
         begin
           Result := Result + 1;
         end Available_Entry;
      end Protected_Object;

      --==============================
      
      task Task_1 is 
         entry Available_Entry (Result : in out Integer);
      end  Task_1;

      task body Task_1 is
      begin
         accept Available_Entry (Result : in out Integer) do
            Result := Result + 1;
         end Available_Entry;
         accept Available_Entry (Result : in out Integer) do
            Result := Result + 10;
         end Available_Entry;
      end Task_1;

      --==============================


   begin    -- encapsulation
      if Verbose then
         Report.Comment ("testing protected object");
      end if;

      select
         Protected_Object.Available_Entry (Count);
         if Verbose then
            Report.Comment ("protected object relative delay passed");
         end if;
      or 
         delay No_Delay_Amount;
         Report.Failed ("available protected object entry not selected" &
                        " with relative delay in select statement");
      end select;

      select
         Protected_Object.Available_Entry (Count);
         if Verbose then
            Report.Comment ("protected object absolute delay passed");
         end if;
      or 
         delay until Time_in_the_Past;
         Report.Failed ("available protected object entry not selected" &
                        " with delay until in select statement");
      end select;

      if Count /= 2 then
         Report.Failed ("Result from protected object was expected to" &
                        " to be 2 but was" &
                        Integer'Image (Count));
         Count := 2;
      end if;

      delay ImpDef.Clear_Ready_Queue;

      if Verbose then
        Report.Comment ("now testing task rendezvous");
      end if;

      select
         Task_1.Available_Entry (Count);
         if Verbose then
            Report.Comment ("task entry relative delay passed");
         end if;
      or 
         delay No_Delay_Amount;
         Report.Failed ("available task entry not selected" &
                        " with relative delay in select statement");
      end select;

      delay ImpDef.Clear_Ready_Queue;

      select
         Task_1.Available_Entry (Count);
         if Verbose then
            Report.Comment ("task entry absolute delay passed");
         end if;
      or 
         delay until Time_in_the_Past;
         Report.Failed ("available protected object entry not selected" &
                        " with delay until in select statement");
      end select;

      if Count /= 13 then
         Report.Failed ("Result from protected object was expected to" &
                        " to be 13 but was" &
                        Integer'Image (Count));
      end if;

   end;     -- encapsulation



   Report.Result;

end CXD9001;
