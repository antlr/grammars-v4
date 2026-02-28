-- Ada 2022: Delta aggregates (RM 4.3.4)
procedure Delta_Aggregates is
   type Point is record
      X, Y : Integer := 0;
   end record;

   type Int_Array is array (1 .. 5) of Integer;

   P  : Point     := (X => 1, Y => 2);
   A  : Int_Array := (1, 2, 3, 4, 5);

   -- Record delta aggregate: copy P but override field Y
   P2 : Point     := (P with delta Y => 99);

   -- Array delta aggregate (paren form): copy A but override index 3
   A2 : Int_Array := (A with delta 3 => 30);

   -- Array delta aggregate (bracket form): copy A but override index 1
   A3 : Int_Array := [A with delta 1 => 10];
begin
   pragma Assert (P2.X = 1 and P2.Y = 99);
   pragma Assert (A2 (3) = 30);
   pragma Assert (A3 (1) = 10);
end Delta_Aggregates;
