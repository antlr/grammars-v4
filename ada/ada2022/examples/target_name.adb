-- Ada 2022: Target name (@) in assignments (RM 5.2.1)
procedure Target_Name is
   X : Integer := 10;
   Y : Integer := 3;
begin
   X := @ + 1;          -- same as X := X + 1
   X := @ * 2;          -- same as X := X * 2
   Y := @ + @ + 1;      -- same as Y := Y + Y + 1
   pragma Assert (X = 22);
   pragma Assert (Y = 7);
end Target_Name;
