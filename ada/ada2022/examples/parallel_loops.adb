-- Ada 2022: Parallel loops and parallel block statements (RM 5.5, 5.6.1)
procedure Parallel_Loops is
   type Int_Array is array (1 .. 100) of Integer;
   A : Int_Array;
   B : Int_Array;
   Sum1 : Integer := 0;
   Sum2 : Integer := 0;
begin
   -- Parallel for loop over a range
   parallel for I in A'Range loop
      A (I) := I;
   end loop;

   -- Parallel for loop with chunk specification
   parallel (4) for I in A'Range loop
      B (I) := A (I) * 2;
   end loop;

   -- Parallel block statement: two branches execute concurrently
   parallel do
      for I in 1 .. 50 loop
         Sum1 := Sum1 + A (I);
      end loop;
   and
      for I in 51 .. 100 loop
         Sum2 := Sum2 + A (I);
      end loop;
   end do;

   pragma Assert (Sum1 + Sum2 = 5050);
end Parallel_Loops;
