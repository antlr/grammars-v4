program array2(input,output);
 
type
  array1dim = array ['a'..'e'] of integer;
 
var
  a: array1dim;
  i: char;
  n: integer;
 
(******************************* main **********************************)
begin
   n := 0;
   (* loop to show you can use chars to index arrays *)
   for i := 'a' to 'e' do
   begin
      n := n + 5;
      a[i] := n;                         { cool, huh }
      write(a[i], '' );
   end;
   writeln;
end.
