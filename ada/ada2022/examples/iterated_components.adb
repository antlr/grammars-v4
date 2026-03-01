-- Ada 2022: Iterated component associations (RM 4.3.3)
procedure Iterated_Components is
   type Int_Array is array (1 .. 5) of Integer;

   -- Iterated component with index variable
   Squares : Int_Array := (for I in 1 .. 5 => I * I);

   -- Iterated component using iterator_specification (of-loop)
   type Float_Array is array (1 .. 3) of Float;
   Src  : Int_Array  := (1, 2, 3, 4, 5);

   -- Bracket form of iterated component
   Doubled : Int_Array := [for I in 1 .. 5 => Src (I) * 2];
begin
   null;
   pragma Assert (Squares (1) = 1);
   pragma Assert (Squares (3) = 9);
   pragma Assert (Squares (5) = 25);
   pragma Assert (Doubled (2) = 4);
end Iterated_Components;
