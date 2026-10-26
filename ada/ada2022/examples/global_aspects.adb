package body Global_Aspects is

   procedure Copy_X_To_Y is
   begin
      Y := X;
   end Copy_X_To_Y;

   function Pure_Compute (A, B : Integer) return Integer is
   begin
      return A + B;
   end Pure_Compute;

   procedure Increment_X is
   begin
      X := X + 1;
   end Increment_X;

end Global_Aspects;
