-- B960001.A
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
--      Check that an argument to the delay_until_statement must have type
--      Calendar.Time.  In particular check that the delay_expressions of
--      Duration, Float and Integer are flagged as errors
--
-- TEST DESCRIPTION
--      Four of the most likely representations of the time "15:45 hrs"
--      are used.  Only the Calendar.Time should be correct
--
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Jul 12   RLB     Minor formatting and text fix.
--!


with Ada.Calendar;

procedure B960001 is

   type Real is digits 3;

   Current_Year    :   Ada.Calendar.Year_Number;
   Current_Month   :   Ada.Calendar.Month_Number;
   Current_Day     :   Ada.Calendar.Day_Number;
   Current_Seconds :   Ada.Calendar.Day_Duration;

   -- Time as seconds from the start of day
   Time_Correct_Use : Ada.Calendar.Day_Duration :=
                      Ada.Calendar.Day_Duration (15*3600 + 45*60); -- 15:45 hrs

   -- Error usage
   Time_as_Integer  : integer       := 1545;      -- 15:45 hrs
   Time_as_Duration : duration      := 15.45;     -- 15:45 hrs
   Time_as_Float    : constant Real := 15.45;     -- 15:45 hrs

begin

   --===================================================================

   -- First check for delay_expression being Calendar.Time

   -- Establish current time
   Ada.calendar.Split ( Ada.Calendar.Clock,
                        Year    => Current_Year,
                        Month   => Current_Month,
                        Day     => Current_Day,
                        Seconds => Current_Seconds);

   -- Set alarm time
   Current_Seconds := Time_Correct_Use;  -- 15:45 hrs

   delay until ( Ada.Calendar.Time_Of ( Current_Year,
                                    Current_Month,
                                    Current_Day,
                                    Current_Seconds) );   -- OK

   --===================================================================

   -- Now check the likely error usages

   delay until Time_as_Integer;                                      -- ERROR:
                                                  -- delay_expression bad type

   delay until Time_as_Duration;                                     -- ERROR:
                                                  -- delay_expression bad type

   delay until Time_as_Float;                                        -- ERROR:
                                                  -- delay_expression bad type


end B960001;
