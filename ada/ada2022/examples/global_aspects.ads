-- Ada 2022: Global aspects (RM 6.1.2)
package Global_Aspects is

   X : Integer := 0;
   Y : Integer := 0;

   -- Procedure with Global aspect specifying read of X and write to Y
   procedure Copy_X_To_Y
      with Global => (Input => X, Output => Y);

   -- Function with null global (no globals accessed)
   function Pure_Compute (A, B : Integer) return Integer
      with Global => null;

   -- Procedure with In_Out global
   procedure Increment_X
      with Global => (In_Out => X);

end Global_Aspects;
