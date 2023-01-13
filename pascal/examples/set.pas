program setstuff(input, output);
 
(*** Demonstrate the set datatype. ***)
(* You are only responsible for sets of integers              *)
(* You are only responsible for the operators in this example *)
 
(* union -- elements contained in either or both sets               *)
(* intersection -- elements common to both sets                     *)
(* difference -- say A-B, all elements in set A but not in set B    *)
(* subset -- say A <= B, true if all elements in set A are in set B *)
(* in -- says whether an item is in the set                         *)
 
type
   Digit = set of 0..9;
 
var
   odds, evens, empty, stuff1, stuff2, morestuff: Digit;
 
begin
   odds := [1, 3, 5, 7, 9];
   evens := [0, 2, 4, 6, 8];
   empty := [];                   (* empty set *)
   stuff1 := odds + [2, 4];       (* union of 2 sets *)
   stuff2 := evens * [2, 4];      (* intersection of 2 sets *)
   morestuff := stuff1 - stuff2;  (* difference of 2 sets *)
 
   if 3 in stuff1 then
      writeln('3 in the set')
   else
      writeln('3 not in the set');
 
   if 4 in stuff2 then
      writeln('4 in the set')
   else
      writeln('4 not in the set');
 
   if stuff2 <= stuff1 then
      writeln('is contained in')
   else
      writeln('is not contained in');
 
end.

