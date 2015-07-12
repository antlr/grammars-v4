program factorial(input,output);
 
const
  number = 5;
 
var
  result: integer;
 
(* demonstrate that a function name is how a function returns its value *)
 
function fact(n: integer): integer;   
var
    i, answer: integer;
 
begin
   fact := 1;
   answer := 1;
   if n > 1 then
      for i := 2 to n do
          answer := answer * i;
      fact := answer;
end;
 
begin                (* main program *)
   result := fact(number);
   writeln('Factorial of ', number, ' is ', result);
end.

