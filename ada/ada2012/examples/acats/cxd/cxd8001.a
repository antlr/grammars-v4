-- CXD8001.A
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
--      Check the basic functions in the Package Ada.Real_Time.
--
-- TEST DESCRIPTION:
--      Simple calls to the basic functions are made using the private
--      constants.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      06 Nov 95   SAIC    ACVC 2.0.1
--
--!

with Report;
with Ada.Real_Time;


procedure CXD8001 is
   
   package ART renames Ada.Real_Time;

   Test_Time_Span : ART.Time_Span;
   Time_Span_Two  : ART.Time_Span;      -- used in 04f and 04g
   Time_Span_Neg  : ART.Time_Span;      -- used in 04c and 05a

begin

   Report.Test ("CXD8001", "Monotonic Time: Check the basic functions");

   --=============================================
   
   -- Subtest  01 - check the comparison functions - Time_Span
   declare
   begin
   
      -- 01a   function "<"  (Left, Right: Time_Span) Return Boolean;
      if    not ART."<"(ART.Time_Span_First, ART.Time_Span_Last) 
         OR     ART."<"(ART.Time_Span_First, ART.Time_Span_First) 
         OR     ART."<"(ART.Time_Span_Last,  ART.Time_Span_First)  then

         Report.Failed ("Test 01a failed");
      else     
         Report.Comment ("test 01a");     
      end if;
   
      -- 01b   function "<=" (Left, Right: Time_Span) Return Boolean;
      if    not ART."<=" (ART.Time_Span_First, ART.Time_Span_First) 
         OR not ART."<=" (ART.Time_Span_First, ART.Time_Span_Last) 
         OR     ART."<=" (ART.Time_Span_Last,  ART.Time_Span_First) then

         Report.Failed ("Test 01b failed");
      else     
         Report.Comment ("test 01b");     
      end if;
   
      -- 01c   function ">"  (Left, Right: Time_Span) Return Boolean;
      if    not ART.">" (ART.Time_Span_Last,  ART.Time_Span_First) 
         OR     ART.">" (ART.Time_Span_Last,  ART.Time_Span_Last) 
         OR     ART.">" (ART.Time_Span_First, ART.Time_Span_Last) then

         Report.Failed ("Test 01c failed");
      else     
         Report.Comment ("test 01c");     
      end if;
      
      -- 01d   function ">=" (Left, Right: Time_Span) Return Boolean;
      if    not ART.">=" (ART.Time_Span_Last,  ART.Time_Span_Last) 
         OR not ART.">=" (ART.Time_Span_Last,  ART.Time_Span_First) 
         OR     ART.">=" (ART.Time_Span_First, ART.Time_Span_Last) then

         Report.Failed ("Test 01d failed");
      else     
         Report.Comment ("test 01d");     
      end if;
      Report.Comment ("Subtest 01 complete");
   exception
      when others => Report.Failed ("Unexpected Exception in Subtest 01");
   end; -- declare
   
      --=============================================
   
   -- Subtest  02 - check the comparison functions - Time
   declare
   begin
   
      -- 02a   function  "<"  (Left, Right: Time) Return Boolean;
      if    not ART."<"(ART.Time_First, ART.Time_Last) 
         OR     ART."<"(ART.Time_First, ART.Time_First) 
         OR     ART."<"(ART.Time_Last,  ART.Time_First)  then

         Report.Failed ("Test 02a failed");
      else     
         Report.Comment ("test 02a");     
      end if;
   
      -- 02b   function  "<=" (Left, Right: Time) Return Boolean;
      if    not ART."<=" (ART.Time_First, ART.Time_First) 
         OR not ART."<=" (ART.Time_First, ART.Time_Last) 
         OR     ART."<=" (ART.Time_Last,  ART.Time_First) then

         Report.Failed ("Test 02b failed");
      else     
         Report.Comment ("test 02b");     
      end if;
   
      -- 02c   function  ">"  (Left, Right: Time) Return Boolean;
      if    not ART.">" (ART.Time_Last,  ART.Time_First) 
         OR     ART.">" (ART.Time_Last,  ART.Time_Last) 
         OR     ART.">" (ART.Time_First, ART.Time_Last) then

         Report.Failed ("Test 02c failed");
      else     
         Report.Comment ("test 02c");     
      end if;
   
      -- 02d   function  ">=" (Left, Right: Time) Return Boolean;
      if    not ART.">=" (ART.Time_Last,  ART.Time_Last) 
         OR not ART.">=" (ART.Time_Last,  ART.Time_First) 
         OR     ART.">=" (ART.Time_First, ART.Time_Last) then

         Report.Failed ("Test 02d failed");
      else     
         Report.Comment ("test 02d");     
      end if;
     
      Report.Comment ("Subtest 02 complete");
   exception
      when others => Report.Failed ("Unexpected Exception in Subtest 02");
   end; -- declare
      
      --=============================================
   
   -- Subtest  03 - check the arithmetic functions - Time/Time_Span
   declare
      Test_Time      : ART.Time;
      Test_Time_2    : ART.Time;
      Start_Time     : ART.Time;
   begin
   
      -- 03a   function "+" (Left: Time; Right: Time_Span) return Time;
      --
      Test_Time :=  ART.Time_First;
      -- Call the function
      Test_Time := ART."+" (Test_Time, ART.Time_Span_Unit);  
      -- Check the result
      if ART."<=" (Test_Time, ART.Time_First)  then
         Report.Failed ("Test 03a failed");
      else     
         Report.Comment ("test 03a");     
      end if;
   
      -- 03b   function "+" (Left: Time_Span; Right: Time) return Time;
      --
      Test_Time :=  ART.Time_First;
      -- call the function
      Test_Time := ART."+" (ART.Time_Span_Unit, Test_Time); 
      -- check the result
      if ART."<=" (Test_Time, ART.Time_First) then
         Report.Failed ("Test 03b failed");
      end if;
   
      -- 03c   function "-" (Left: Time; Right: Time_Span) return Time;
      Test_Time_2 :=  ART.Time_Last;
      -- Call the function
      Test_Time_2 := ART."-" (Test_Time_2, ART.Time_Span_Unit);  
      -- check the result
      if ART.">="(Test_Time_2, ART.Time_Last) then
         Report.Failed ("Test 03c failed");
      end if;
      -- Now use the result from 03b
      Test_Time := ART."-" (Test_Time, ART.Time_Span_Unit);  

      -- check for equality (for a private type, "=" is automatically
      -- available and is qualifiable)
      if not ART."=" (Test_Time, ART.Time_First) then
         Report.Failed ("Test 03c part II failed");
      end if;
   
      -- 03d   function "-" (Left: Time; Right: Time) return Time_Span;
      -- Set Start_Time to a bit more than Time_First
      Start_Time := ART."+" (ART.Time_First, ART.Time_Span_Unit);
      Test_Time  := ART."+" (Start_Time, ART.Time_Span_Unit);
      -- Test the function
      Test_Time_Span := ART."-" (Test_Time, Start_Time);
      -- Check the result
      if not ART."=" (Test_Time_Span, ART.Time_Span_Unit) then
         Report.Failed ("Test 03d failed");
      else     
         Report.Comment ("test 03d");     
      end if;
   
   
      Report.Comment ("Subtest 03 complete");
   exception
      when others => Report.Failed ("Unexpected Exception in Subtest 03");
   end; -- declare
   
      --=============================================
      
   -- Subtest  04 - check the arithmetic functions - Time_Span
   declare
      Test_Integer   : integer;
      Integer_One    : constant integer := 1;
      Integer_Two    : constant integer := 2;
   begin
   
      -- 04a   function "+"  (Left, Right: Time_Span) Return Time_Span;
      --
      -- call the function
      Test_Time_Span := ART."+" (ART.Time_Span_First, ART.Time_Span_Unit);
      -- check the result
      if ART."<=" (Test_Time_Span, ART.Time_Span_First) then
         Report.Failed ("Test 04a failed");
      else     
         Report.Comment ("test 04a");     
      end if;
   
   
      -- 04b   function "-"  (Left, Right: Time_Span) Return Time_Span;
      --
      -- call the function
      -- use the results of the last test
      Test_Time_Span := ART."-" (Test_Time_Span, ART.Time_Span_Unit);
      -- check the result
      -- Note: for the private type "=" is automatically available and is 
      -- qualifiable
      if not ART."=" (Test_Time_Span, ART.Time_Span_First) then 
         Report.Failed ("Test 04b failed");
      else     
         Report.Comment ("test 04b");     
      end if;
   
   
      -- 04c   function "-"  (Right: Time_Span) Return Time_Span;
      --
      -- call the function
      Time_Span_Neg := ART."-"(ART.Time_Span_Unit); 
      -- check the result
      Test_Time_Span := ART."+"(ART.Time_Span_Unit, Time_Span_Neg);
      --
      if not ART."=" (Test_Time_Span, ART.Time_Span_Zero) then
         Report.Failed ("Test 04c failed"); 
      else     
         Report.Comment ("test 04c");     
      end if;
   
      -- 04d   function "/"  (Left, Right: Time_Span) Return Integer; 
      --
      -- call the function
      Test_Integer := ART."/" (ART.Time_Span_Unit, ART.Time_Span_Unit);
      -- check the result
      if Test_Integer /= 1  then
         Report.Failed ("Test 04d failed");
      else     
         Report.Comment ("test 04d");     
      end if;
   
      -- 04e  function "/"(Left : Time_Span; Right : Integer) Return Time_Span;
      --
      -- call the function
      Test_Time_Span := ART."/" (ART.Time_Span_Unit, Integer_One);
      -- check the result
      if not ART."=" (Test_Time_Span, ART.Time_Span_Unit) then   
         Report.Failed ("Test 04e failed");
      else     
         Report.Comment ("test 04e");     
      end if;
   
      -- 04f  function "*"(Left : Time_Span; Right : Integer) Return Time_Span;
      --
      -- call the function
      Test_Time_Span := ART."*" (ART.Time_Span_Unit, Integer_Two);
      -- check the result
      Time_Span_Two := ART."+" (ART.Time_Span_Unit, ART.Time_Span_Unit);
      if not ART."=" (Time_Span_Two, Test_Time_Span) then   
         Report.Failed ("Test 04f failed ");
      else     
         Report.Comment ("test 04f");     
      end if;
   
      -- 04g  function "*"(Left : Integer; Right : Time_Span) Return Time_Span;
      --
      -- call the function
      Test_Time_Span := ART."*" (Integer_Two, ART.Time_Span_Unit);
      -- check the result
      -- use the result of 04f
      if not ART."=" (Time_Span_Two, Test_Time_Span) then   
         Report.Failed ("Test 04g failed ");
      else     
         Report.Comment ("test 04g");     
      end if;
   
      Report.Comment ("Subtest 04 complete");
   exception
      when others => Report.Failed ("Unexpected Exception in Subtest 04");
   end; -- declare
   
      --=============================================
   
   -- Subtest  05 - Miscellaneous 
   declare
      Test_Duration   : duration;             
      Time_Span_Nano  : ART.Time_Span;
      Time_Span_Micro : ART.Time_Span;
      Time_Span_Milli : ART.Time_Span;
   begin
      -- 05a   function "abs" (Right : Time_Span) return Time_Span;
      -- Call the function
      Test_Time_Span := ART."abs" (Time_Span_Neg);  -- (-Time_Span_Unit)
      -- Check the result
      Test_Time_Span := ART."+" (Test_Time_Span, ART.Time_Span_Unit);
      if not ART."=" (Test_Time_Span, Time_Span_Two) then
         Report.Failed ("Test 05a failed ");
      else     
         Report.Comment ("test 05a");     
      end if;
   
   
      -- 05b   function To_Duration  (TS : Time_Span) return Duration;
      --       function To_Time_Span (D  : Duration)  return Time_Span;
      -- Call the functions
      Test_Duration  := ART.To_Duration  (ART.Time_Span_Unit);
      Test_Time_Span := ART.To_Time_Span (Test_Duration);
      -- No check of the results because information could
      -- be lost in the conversion process.
      Report.Comment ("test 05b");     
   
   
   
      -- 05c   function Nanoseconds  (NS : Integer) return Time_Span;
      --       function Microseconds (US : Integer) return Time_Span;
      -- call the functions
      Time_Span_Nano  := ART.Nanoseconds (1_000);
      Time_Span_Micro := ART.Microseconds (1);
   
      -- check the results
      if not ART."=" (Time_Span_Nano, Time_Span_Micro) then
         Report.Failed ("Test 05c failed ");
      else     
         Report.Comment ("test 05c");     
      end if;
   
   
      -- 05d   function Microseconds (US : Integer) return Time_Span;
      --       function Milliseconds (MS : Integer) return Time_Span;
      -- call the functions
      Time_Span_Micro := ART.Microseconds (1_000);
      Time_Span_Milli := ART.Milliseconds (1);
   
      if not ART."=" (Time_Span_Micro, Time_Span_Milli) then
         Report.Failed ("Test 05d failed ");
      else     
         Report.Comment ("test 05d");     
      end if;
      
      Report.Comment ("Subtest 05 complete");
   exception
      when others => Report.Failed ("Unexpected Exception in Subtest 05");
   end; -- declare
   
   --=============================================

   Report.Result;
   
end CXD8001;
