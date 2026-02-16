-- CXD8003.A
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
--      Check that the Ada.Real_Time package operations Split and
--      Time_Of operations work properly.
--      Check that the clock does not jump backwards.
--
-- TEST DESCRIPTION:  
--      The checks of Split and Time_Of check a variety of samples to
--      be sure that the correct value is returned.
--      To check that there are no backward clock jumps, the clock 
--      is sampled as frequently as possible and compared to the previous
--      sample.
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--
--
-- CHANGE HISTORY:
--     02 OCT 95   SAIC    Initial version
--
--!

with Report;
with ImpDef;
with System;
with Ada.Real_Time;
procedure CXD8003 is
   Verbose     : constant Boolean := False;
   type Int is range 0 .. System.Max_Int;
   package RT renames Ada.Real_Time;
   use RT;  -- for all the operators


   procedure Print_Time (Note : String; T : RT.Time) is
      SC : RT.Seconds_Count;
      TS : RT.Time_Span;
   begin
      RT.Split (T, SC, TS);
      Report.Comment (Note & RT.Seconds_Count'Image (SC) &
         Duration'Image (RT.To_Duration (TS)) &
         " [" &
         Integer'Image (TS / RT.Time_Span_Unit) &
         "]");
   end Print_Time;


   procedure Check_Split is
      T                 : RT.Time;
      One_Second        : constant RT.Time_Span := 
                             RT.Milliseconds (1000);
      Almost_5_Seconds,
      Almost_A_Second,
      TS                : RT.Time_Span;
      SC                : RT.Seconds_Count;
      Dif               : Integer;
   begin
      TS := RT.Milliseconds (Report.Ident_Int (1000));  -- 1 second
      T := RT.Time_Of (1, TS);  -- 2 seconds
      RT.Split (T, SC, TS);
      if SC /= 2 then
        Report.Failed ("SC expected 2 got" &
                       RT.Seconds_Count'Image (SC));
      end if;
      if TS /= RT.Time_Span_Zero then
        Report.Failed ("TS not 0 got (in units)" &
                       Integer'Image (TS / RT.Time_Span_Unit));
      end if;

      -- compute a time_span that has one less time unit than is found
      -- in a second.
      T := RT.Time_Of (1, RT.Time_Span_Zero) - RT.Time_Span_Unit;
      -- Almost_A_Second is just below 1 second
      RT.Split (T, SC, Almost_A_Second);
      if Almost_A_Second >= One_Second then
        Report.Failed ("Almost_A_Second >= One_Second");
      end if;
      if Verbose then
         Report.Comment ("time units in a second" &
             Integer'Image (Almost_A_Second / RT.Time_Span_Unit + 1));
      end if;
      if SC /= 0 then
         Report.Failed ("SC expected 0 got" &
                        RT.Seconds_Count'Image (SC));
      end if;
      Almost_5_Seconds := Almost_A_Second * 5;  -- just under 5 seconds
      RT.Split (RT.Time_Of (0, Almost_5_Seconds), SC, TS);
      if SC /= 4 then
         Report.Failed ("SC expected 4 got" &
                        RT.Seconds_Count'Image (SC));
      end if;
      -- TS should be 5 time_span_unit below one second
      Dif := (RT.Milliseconds(1000) - TS) / RT.Time_Span_Unit;
      if Dif /= 5 then
         Report.Failed ("expected 5 time units difference, got" &
                        Integer'Image (Dif));
      end if;
   end Check_Split;


   procedure Check_Clock is
      -- determine how long the test is to run
      Time_To_Run : RT.Time_Span := To_Time_Span (2.0);
      Time_To_Finish : RT.Time := RT.Clock + Time_To_Run;
      Tick_Cnt : Int := 0;
      Previous : RT.Time := RT.Clock;
      This : RT.Time;
   begin
      loop
         This := RT.Clock;
         if This /= Previous then
            if This < Previous then
               Report.Failed ("clock jumped backwards");
               Print_Time ("Previous time ", Previous);
               Print_Time ("This time ", This);
            else  -- normal tick
               Tick_Cnt := Tick_Cnt + 1;
            end if;
            Previous := This;
            exit when This > Time_To_Finish;
         end if;
      end loop;
      if Verbose then
         Report.Comment ("clock ticks detected in 2 seconds:" &
                         Int'Image (Tick_Cnt));
      end if;
   end Check_Clock;

begin
   Report.Test ("CXD8003", "Check the Clock, Split and Time_Of routines " &
                           " of the Ada.Real_Time package");

   Check_Split;
   Check_Clock;

   Report.Result;
end CXD8003;
