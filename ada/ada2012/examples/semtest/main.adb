with Helpers;
use Helpers;

procedure Main is
    subtype Local_Pair is Pair;
    subtype Local_Small is Small_Int;
    subtype Local_Arr is Int_Array;

    P : Local_Pair;
    S : Local_Small;
    A : Local_Arr;
    C : Color;
    R : Integer;
begin
    -- Record aggregate (named)
    P := (X => 10, Y => 20);

    -- Record qualified expression using local subtype (should work: Local_Pair is locally defined)
    P := Local_Pair'(X => 1, Y => 2);

    -- Qualified expression using type from package (Pair is NOT in local symbol table)
    -- This tests whether the symbol table carries over from helpers.ads.
    -- Expected: fails parse if symbol table does NOT carry over.
    P := Pair'(X => 3, Y => 4);

    -- Type conversion using type from package
    -- Small_Int is defined in helpers.ads, not locally.
    S := Small_Int(R);

    -- Procedure call
    Swap (P);

    -- Function call and assignment
    R := Add (P.X, P.Y);

    -- Subtype qualified expression (local)
    S := Local_Small'(42);

    -- Array aggregate (positional)
    A := (1, 2, 3);

    -- Array qualified expression (local subtype)
    A := Local_Arr'(10, 20, 30);

    -- Parenthesized expression (not aggregate)
    R := (R + 1);

    -- Enumeration
    C := Green;

    -- Block with local declarations
    declare
        type Matrix is array (1 .. 2, 1 .. 2) of Integer;
        M : Matrix;
        Local : Local_Pair;
    begin
        M := ((1, 0), (0, 1));
        Local := Make_Pair (3, 4);
        R := Local.X + Local.Y;
    end;

end Main;
