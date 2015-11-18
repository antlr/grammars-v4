program caseRepeat(input, output);
 
(*** Demonstrate the repeat and case statements.  ***)
 
var
   number: integer;
 
begin
  repeat 
     writeln('Enter year number, between 1 and 5, negative number to finish:');
     read(number);
     if (number >= 1) and (number <= 5) then begin
      case number of
        1: writeln('First year student');
        2: writeln('Sophomore');
        3: writeln('Junior');
        4: writeln('Senior');
        5: begin
             writeln('Congratulations, you have graduated ... ');
             writeln('Graduate student???');
            end
      end
    end
  until number < 0
end.
