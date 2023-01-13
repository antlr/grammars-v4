program ifprog(input, output);
 
(*** Demonstrate the if.  ***)
 
var
   number: integer;
 
(* **************************    main      ********************************** *)
 
begin
  writeln('Please enter an integer between 0 and 100');
  read(number);
  while (number < 0) or (number > 100) do begin
     writeln('Try again please, integer between 0 and 100');
     read(number);
  end;
 
  write('If this was a grade, you would receive a grade of:  ');
  if number >= 90 then
     write('A')
  else if number >= 80 then
     write('B')
  else if number >= 70 then
     write('C')
  else if number >= 60 then
     write('D')
  else
     write('F');
  writeln;
end.

