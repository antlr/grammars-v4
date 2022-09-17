program nesting(input, output);
 
(*** Demonstrate nesting of procedures.  ***)
 
var
   i,j,k: integer;
 
(* ***************************    procA    ********************************** *)
 
procedure procA;
 
   var  i,j: integer;
 
   procedure procA2;
      var  i: integer;
   begin
      writeln('in procA2');
      i := 120;
      j := 121;
      writeln('i=', i, '  j=', j, '  k=', k);
   end;
 
   procedure procA3;
      var  i,j: integer;
   begin
      writeln('in procA3');
      i := 130;
      j := 131;
      k := 132;
      writeln('i=', i, '  j=', j, '  k=', k);
   end;
 
begin
   writeln('in procA');
   i := 10;
   j := 11;
   k := 12;
   writeln('i=', i, '  j=', j, '  k=', k);
   procA2;
   procA3;
end;
 
(* ***************************    procB    ********************************** *)
 
procedure procB;
 
   var  i: integer;
 
begin
   writeln('in procB');
   i := 20;
   j := 21;
   writeln('i=', i, '  j=', j, '  k=', k);
end;
 
(* **************************    main      ********************************** *)
 
begin
   writeln('in main');
   i := 1;
   j := 2;
   k := 3;
   writeln('i=', i, '  j=', j, '  k=', k);
   procA;
   procB;
end.
