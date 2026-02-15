-- CXC6003.A
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
--      Check that all reads and updates of atomic and volatile
--      objects are performed directly to memory.
--      Check that reads and updates of atomic objects are indivisible.
--      Check that Atomic_Components can be used even when the Component_Size
--      of an array is specified.
--
-- TEST DESCRIPTION:
--      A generic package is used to create multiple tasks, each
--      of a different priority and Id.  These tasks coordinate
--      with the main procedure to perform the various checks
--      on the atomic and volatile objects.
--      The main procedure initiates each section of the test and
--      checks the result. The result is in the form of a count
--      of the number of times an unexpected result was encountered.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting:
--        the Systems Programming Annex,
--        pragma Priority from the Real-Time Annex, and
--        pragma Atomic on objects of type Integer.
--
--
-- CHANGE HISTORY:
--       4 FEB 96   SAIC    Initial release for 2.1
--       7 MAY 96   SAIC    Incorporated Reviewer comments.
--       7 JUL 12   RLB     Fixed potentially illegal test case, revised
--                          dubious objective.
--
--!


------------------------------------------------------------------

with System;
with ImpDef.Annex_C;
with Report;
procedure CXC6003 is
   Verbose : constant Boolean := False;

      -- the number of tasks is the number of instantiations of
      -- the package Checker.
   Number_Of_Tasks : constant := 5;

      -- the number of times a task gives up control in the
      -- looping tests.
   Iterations : constant := 100;

      -- computed value that indicates how many bytes are in
      -- the standard type Integer.
   Num_Bytes_In_Integer : Integer;

      -- Counter is used by the tasks to record the number of
      -- events being counted in a given test section.
   protected Counter is
      procedure Reset;
      procedure Increment;
      function Value return Integer;
   private
      Count : Integer := 0;
   end Counter;

   protected body Counter is
      procedure Reset is
      begin
	 Count := 0;
      end Reset;

      procedure Increment is
      begin
	 Count := Count + 1;
      end Increment;

      function Value return Integer is
      begin
	 return Count;
      end Value;
   end Counter;


      -- Test_Section is used to coordinate between the tasks
      -- and the main procedure.
      -- The tasks performing a test call Start_Section before
      -- starting and Done when the test is complete.
      -- The main procedure calls Start_All to start a test.
      -- Start_All waits until all the tasks are waiting at
      -- Start_Section and then releases them all at once.
      -- The main procedure calls All_Done to wait for all
      -- the tasks to complete the test.  At that point the
      -- main procedure can check the results of the test.
   protected Test_Section is
	 -- for coordinating the start of a test section
      entry Start_Section;
      entry Start_All;

	 -- for coordinating the end of a test section
      procedure Done;
      entry All_Done;
   private
      entry While_Starting;
      Go_Ahead : Boolean := False;
      Done_Count : Integer := 0;
   end Test_Section;

   protected body Test_Section is
      entry Start_Section when Go_Ahead is
      begin
	 null;
      end Start_Section;

      entry Start_All when Start_Section'Count = Number_Of_Tasks is
      begin
	 Go_Ahead := True;
	   -- block until all tasks have been freed
	 requeue While_Starting;
      end Start_All;

      entry While_Starting when Start_Section'Count = 0 is
      begin
	 Go_Ahead := False;
      end While_Starting;

      procedure Done is
      begin
	 Done_Count := Done_Count + 1;
      end Done;

      entry All_Done when Done_Count = Number_Of_Tasks is
      begin
	 Done_Count := 0;
      end All_Done;
   end Test_Section;

begin
   Report.Test ("CXC6003",
                "Check that reads and updates of atomic and" &
                " volatile objects are performed directly to memory");

   case Integer'Size is
      when  0..15 =>  Num_Bytes_In_Integer := 1;
      when 16..23 =>  Num_Bytes_In_Integer := 2;
      when 24..31 =>  Num_Bytes_In_Integer := 3;
      when 32..39 =>  Num_Bytes_In_Integer := 4;
      when 40..47 =>  Num_Bytes_In_Integer := 5;
      when 48..55 =>  Num_Bytes_In_Integer := 6;
      when 56..63 =>  Num_Bytes_In_Integer := 7;
      when others =>  Num_Bytes_In_Integer := 8;
   end case;

   declare   -- encapsulation
      Atomic_Int : Integer := 0;
      pragma Atomic (Atomic_Int);

      Volatile_Int : Integer := 0;
      pragma Volatile (Volatile_Int);

      type Small_Num is range 0..15;
      type Nibble_Array is array (1..20) of Small_Num;
      for Nibble_Array'Component_Size use
          Impdef.Annex_C.Minimum_Atomic_Component_Size;
      pragma Atomic_Components (Nibble_Array);
      Nibbles : Nibble_Array := (others => 0);

      procedure Thwart_Optimizer (Nibbles : in out Nibble_Array) is
      begin
         if Report.Ident_Bool(False) then
            -- never executed but the compiler doesn't know that
            Nibbles := (others => 15);
         end if;
      end Thwart_Optimizer;


      generic
         Id : Integer;
         Priority : System.Priority;
      package Checker is
      end Checker;

      package body Checker is
         task Check_It is
	    pragma Priority (Priority);
         end Check_It;

         task body Check_It is
	    Copy_Of_Atomic_Value : Integer;
	    My_Atomic_Value : Integer := 0;
	    X : Integer;
         begin
	    -- Check that reads and writes to volatile objects
	    -- occur directly to memory.  Each task sets Volatile_Int
	    -- and then delays long enough for the other tasks to
	    -- perform the assignment.
	    Test_Section.Start_Section;
	    Volatile_Int := Id;
	    -- wait for all the tasks to do the setting.
	    -- note that delay is not one of the operations that would
	    -- make the operations "sequential"   9.10;6.0
	    delay ImpDef.Clear_Ready_Queue;
	    -- see if the values in the Volatile object
	    -- has changed.  At most, one task should see the same
	    -- value as it just set.
	    if Volatile_Int = Id then
	       Counter.Increment;
	    end if;
	    Test_Section.Done;


	    -- Check that reads and writes to atomic objects
	    -- occur directly to memory.  Each task sets Volatile_Int
	    -- and then delays long enough for the other tasks to
	    -- perform the assignment.
	    Test_Section.Start_Section;
	    Atomic_Int := Id;
	    -- wait for all the tasks to do the setting.
	    -- note that delay is not one of the operations that would
	    -- make the operations "sequential"   9.10;6.0
	    delay ImpDef.Clear_Ready_Queue;
	    -- see if the values in the Atomic object
	    -- has changed.  At most, one task should see the same
	    -- value as it just set.
	    if Atomic_Int = Id then
	       Counter.Increment;
	    end if;
	    Test_Section.Done;

            -- Check that reads and writes to elements of the packed
            -- array of atomic components do not disturb the surrounding
	    -- components.
            -- Note that each task writes to a different element
            -- of the array Nibbles.  These writes should not
            -- affect the values read or written by any of the
            -- other tasks.  However, if the compiler allocates the
	    -- array such that shifts and masks are required to read and
	    -- write the elements of the array then the writes may well
	    -- impact the values of adjacent elements if preempted at
	    -- a bad time.
	    Test_Section.Start_Section;
	    for I in 1 .. Iterations loop
               -- try to get some preemption occurring
	       delay ImpDef.Minimum_Task_Switch;
	       for J in 1..1000 loop

	          Nibbles (Id) := Small_Num(Id);
                  Thwart_Optimizer (Nibbles);
	          if Nibbles (Id) /= Small_Num(Id) then
		      Counter.Increment;
	          end if;

	          Nibbles (Id) := 0;
                  Thwart_Optimizer (Nibbles);
	          if Nibbles (Id) /= 0 then
		      Counter.Increment;
	          end if;

	          Nibbles (Id) := Small_Num(Id);
                  Thwart_Optimizer (Nibbles);
	          if Nibbles (Id) /= Small_Num(Id) then
		      Counter.Increment;
	          end if;

 	          Nibbles (Id) := Small_Num'Last - Small_Num (Id);
                  Thwart_Optimizer (Nibbles);
	          if Nibbles (Id) /= Small_Num'Last - Small_Num(Id) then
		      Counter.Increment;
	          end if;
	       end loop;
	    end loop;
	    Test_Section.Done;

	    -- Check that only "complete" values are stored in an
	    -- atomic object.  This is checked by having each task
	    -- write a value to the atomic object that is composed
	    -- of the Id value replicated in each byte of the value
	    -- written.  When a value is read from the atomic object
	    -- it is checked to be sure the same value is replicated
	    -- in the bytes.
	    Test_Section.Start_Section;
	    for I in 1.. Num_Bytes_In_Integer loop
	       My_Atomic_Value := My_Atomic_Value * 256 + Id;
            end loop;

               -- make sure it has a valid value to start
	    Atomic_Int := My_Atomic_Value;

	    for I in 1 .. Iterations loop
	       delay ImpDef.Minimum_Task_Switch;
	       for J in 1 .. 1000 loop
                  -- read what is currently there and put
                  -- our own value in there.
		  Copy_Of_Atomic_Value := Atomic_Int;
		  Atomic_Int := My_Atomic_Value;

                  -- check the value we read to see if it is ok.
		  -- use the low byte as the check value
		  X := Copy_Of_Atomic_Value mod 256;
		  -- do the remaining bytes equal the check value?
		  for K in 1 .. Num_Bytes_In_Integer - 1 loop
                     Copy_Of_Atomic_Value := Copy_Of_Atomic_Value / 256;
	             if Copy_Of_Atomic_Value mod 256 /= X then
			 -- note error
		         Counter.Increment;
	             end if;
		  end loop;
	       end loop;
	    end loop;
	    Test_Section.Done;

         end Check_It;

      end Checker;

      package C1 is new Checker (1, System.Priority'First + 0);
      package C2 is new Checker (2, System.Priority'First + 1);
      package C3 is new Checker (3, System.Priority'First + 2);
      package C4 is new Checker (4, System.Priority'First + 3);
      package C5 is new Checker (5, System.Priority'First + 4);
   begin  -- encapsulation
      Counter.Reset;
      Test_Section.Start_All;
      Test_Section.All_Done;
      if Counter.Value > 1 then
	 Report.Failed ("volatile objects not read from memory");
      elsif Verbose then
         Report.Comment ("volatile objects read from memory");
      end if;

      Counter.Reset;
      Test_Section.Start_All;
      Test_Section.All_Done;
      if Counter.Value > 1 then
	 Report.Failed ("atomic objects not read from memory");
      elsif Verbose then
         Report.Comment ("atomic objects read from memory");
      end if;

      Counter.Reset;
      Test_Section.Start_All;
      Test_Section.All_Done;
      -- check that Nibbles ends up with the expected values.
      for I in 1..5 loop
	 if Nibbles (I) /= Small_Num'Last - Small_Num(I) then
            Report.Failed ("nibble array final value bad at index" &
                           Integer'Image (I) &
                           " value is" &
                           Small_Num'Image (Nibbles (I)));
         end if;
      end loop;
      if Counter.Value > 0 then
	 Report.Failed ("assignment to atomic components of array" &
			" affect surrounding components");
      elsif Verbose then
         Report.Comment ("array element assignment passed");
      end if;

      Counter.Reset;
      Test_Section.Start_All;
      Test_Section.All_Done;
      if Counter.Value > 0 then
	 Report.Failed ("atomic value corrupted by multiple tasks" &
                        Integer'Image (Counter.Value) &
                        " times");
      elsif Verbose then
	 Report.Comment ("atomic value not corrupted by multiple tasks");
      end if;

   exception
      when others => Report.Failed ("unexpected exception");
   end;   -- encapsulation

   Report.Result;
end CXC6003;
