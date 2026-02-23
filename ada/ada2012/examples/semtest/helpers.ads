package Helpers is

    type Color is (Red, Green, Blue);

    type Pair is record
        X : Integer;
        Y : Integer;
    end record;

    subtype Small_Int is Integer range 1 .. 100;

    type Int_Array is array (1 .. 3) of Integer;

    subtype Short_Array is Int_Array;

    function Add (A, B : Integer) return Integer;
    function Make_Pair (X, Y : Integer) return Pair;
    procedure Swap (P : in out Pair);

end Helpers;
