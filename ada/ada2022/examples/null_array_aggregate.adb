-- Ada 2022: Null bracket aggregate [] (RM 4.3.3)
procedure Null_Array_Aggregate is
   type Int_Array is array (Positive range <>) of Integer;

   -- A constant zero-length array using null bracket aggregate
   Empty : constant Int_Array := [];

   -- Returned from a function
   function Make_Empty return Int_Array is
   begin
      return [];
   end Make_Empty;

   Result : Int_Array := Make_Empty;
begin
   null;
   pragma Assert (Empty'Length = 0);
   pragma Assert (Result'Length = 0);
end Null_Array_Aggregate;
