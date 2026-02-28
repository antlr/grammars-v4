-- Ada 2022: Bracket array aggregates (RM 4.3.3)
procedure Bracket_Aggregates is
   type Int_Array is array (Positive range <>) of Integer;

   -- Null bracket aggregate
   Empty : Int_Array (1 .. 0) := [];

   -- Positional bracket aggregate
   A : Int_Array (1 .. 3) := [1, 2, 3];

   -- Positional with OTHERS
   B : Int_Array (1 .. 5) := [1, 2, others => 0];

   -- Named bracket aggregate
   C : Int_Array (1 .. 4) := [1 => 10, 2 => 20, 3 => 30, 4 => 40];

   -- Named bracket with OTHERS => BOX (for in/out parameter defaults)
   type Fixed_Array is array (1 .. 3) of Integer;
   D : Fixed_Array := [1 | 2 => 5, others => 0];
begin
   pragma Assert (A (1) = 1 and A (2) = 2 and A (3) = 3);
   pragma Assert (B (1) = 1 and B (3) = 0);
   pragma Assert (C (2) = 20);
   pragma Assert (D (3) = 0);
end Bracket_Aggregates;
