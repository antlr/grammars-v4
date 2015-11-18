program linkedlist2(input,output);
 
(*  Example linked list Pascal Program                          *)
(*                                                              *)
(*  The file list.data would be used as the data file.          *)
(*  If there was an executable, a.out, run:  a.out < list.data  *)
 
const
  grades = 5;                             (* number of grades to be averaged *)
  avgPosition = 6;                        (* position of grade average *)
  size = 4;                               (* number of students *)
 
type
  integerArray = array [1..avgPosition] of integer;
 
  cellPtr = ^cell;
  cell = record
     id: integer;
     info: integerArray; 
     next: cellPtr
  end;
 
var
  list, newrec: cellPtr;
  count, classNum: integer;
 
(* ************************************************************************* *)
(* procedure insert                                                          *)
(* ************************************************************************* *)
 
procedure insert(var list: cellPtr; newrec: cellPtr);
var
  current: cellPtr;
  found: boolean;
 
begin
    current := list;
    found := false;
    if (list = nil) then 
      begin
        newrec^.next := list;               (* fix bug so next is set to nil *)
        list := newrec;
      end
    else if (newrec^.id < list^.id) then 
      begin
        newrec^.next := list;
        list := newrec;
      end
    else 
      begin
        while (current <> nil) and (not found) do 
          begin
            if (current^.next = nil) then 
              begin 
                newrec^.next := nil;        (* fix bug so next is set to nil *)
                current^.next := newrec; 
                found := true;
              end
            else if (newrec^.id < current^.next^.id) then 
              begin
                newrec^.next := current^.next;
                current^.next := newrec;
                found := true;
              end;
            current := current^.next;
          end;
      end;
end;
 
(* ************************************************************************* *)
(* function average                                                          *)
(* ************************************************************************* *)
 
function average(newrec: cellPtr): integer;   
var
    i, sum : integer;
 
begin
    sum := 0;
    for i := 1 to grades do
       sum:=sum + newrec^.info[i];
    average:=sum div grades;
end;
 
(* ************************************************************************* *)
(* procedure makeNewrec                                                      *)
(* ************************************************************************* *)
 
procedure makeNewrec(var newrec : cellPtr);
var
    i: integer;
 
begin
    new(newrec);
    read(newrec^.id);
    for i := 1 to grades do 
       read(newrec^.info[i]);
    newrec^.info[avgPosition] := average(newrec);
end;
 
(* ************************************************************************* *)
(* procedure displayInfo                                                     *)
(* ************************************************************************* *)
 
procedure displayInfo(var list : cellPtr);
var
  i: integer;
  current: cellPtr;
 
begin
    current := list;
    if (list <> nil) then 
    begin
        write('            ');
        for i := 1 to grades do 
           write('Grade     ');  
        writeln;
        write('Student');
        for i := 1 to grades do 
           write(i);  
        writeln('        Average');
        for i := 1 to grades+2 do
           write('-----------');
        writeln;
 
        while (current <> nil) do 
        begin
          write(current^.id);
          for i := 1 to (grades + 1) do
            write(current^.info[i]);
          writeln; 
          current:= current^.next;
        end;
    end;
end;
 
(* ************************************************************************* *)
(* procedure cleanup                                                         *)
(* ************************************************************************* *)
 
procedure cleanup(var list : cellPtr);
var
  current: cellPtr;
 
begin
   while (list <> nil) do 
   begin
      current := list;
      list := list^.next;
      current^.next := nil;
      dispose(current);
   end;
   current := nil
end;
(* ************************************************************************** *)
(* main program                                                               *)
(* ************************************************************************** *)
 
begin
  read(classNum);
  list := nil;
  for count := 1 to size do 
  begin
    makeNewrec(newrec);
    insert(list, newrec);
  end;
  writeln('Here are the class grades for class:', classNum); writeln;
  displayInfo(list);
  cleanup(list);
end.