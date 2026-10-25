-- Ada 2022: Iterator filters (RM 5.5.2)
procedure Iterator_Filter is
   type Int_Array is array (1 .. 10) of Integer;
   Arr : Int_Array := (1, -2, 3, -4, 5, -6, 7, -8, 9, -10);

   Sum_Pos : Integer := 0;
   Count   : Integer := 0;
begin
   -- Loop over range with filter: only positive values
   for I in 1 .. 10 when Arr (I) > 0 loop
      Sum_Pos := Sum_Pos + Arr (I);
      Count   := Count + 1;
   end loop;

   pragma Assert (Sum_Pos = 25);  -- 1+3+5+7+9
   pragma Assert (Count = 5);

   -- For-of loop with filter
   for V of Arr when V < 0 loop
      Count := Count - 1;
   end loop;

   pragma Assert (Count = 0);
end Iterator_Filter;
