package body Helpers is

    function Add (A, B : Integer) return Integer is
    begin
        return A + B;
    end Add;

    function Make_Pair (X, Y : Integer) return Pair is
    begin
        return (X => X, Y => Y);
    end Make_Pair;

    procedure Swap (P : in out Pair) is
        Temp : Integer;
    begin
        Temp := P.X;
        P.X := P.Y;
        P.Y := Temp;
    end Swap;

end Helpers;
