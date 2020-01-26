---- MODULE hanoi ----
EXTENDS TLC, Sequences, Integers
                
(* --algorithm hanoi
variables tower = <<<<1, 2, 3>>, <<>>, <<>>>>, 

define 
  D == DOMAIN tower
end define;

begin
while TRUE do
  assert tower[3] /= <<1, 2, 3>>;
  with from \in {x \in D : tower[x] /= <<>>},
       to \in {
                y \in D : 
                  \/ tower[y] = <<>>
                  \/ Head(tower[from]) < Head(tower[y])
              } 
  do
    tower[from] := Tail(tower[from]) ||
    tower[to] := <<Head(tower[from])>> \o tower[to];
  end with;
end while;
end algorithm; *)
====
