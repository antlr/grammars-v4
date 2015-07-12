program subscripts(input, output);
 
(*** Demonstrate arrays differently subscripted.  ***)
 
const
  startSubscript = 5;
  endSubscript = 10;
 
var
   i,j,k: integer;
   a: array [startSubscript .. endSubscript] of integer;
 
(* **************************    main      ********************************** *)
 
begin
  for i := startSubscript to endSubscript do 
     a[i] := i*100;
 
  writeln('The array:');
  for j := startSubscript to endSubscript do 
     write (a[j]);
  writeln
end.