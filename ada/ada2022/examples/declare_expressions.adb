-- Ada 2022: Declare expressions (RM 4.5.9)
procedure Declare_Expressions is
   X : Integer := 10;
   Y : Integer;
begin
   -- Declare expression introducing a local constant
   Y := (declare
            Tmp : constant Integer := X * 2;
         begin
            Tmp + 1);

   pragma Assert (Y = 21);

   -- Nested declare expression
   declare
      Z : Integer := (declare
                         A : constant Integer := 5;
                         B : constant Integer := 3;
                      begin
                         A * B);
   begin
      pragma Assert (Z = 15);
   end;
end Declare_Expressions;
