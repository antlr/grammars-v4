---- MODULE Counter ----
EXTENDS Naturals

(* --algorithm counter
variables count = 0;
begin
Count:
  while count < 10 do
    count := count + 1;
  end while;
Check:
  assert count = 10;
end algorithm; *)

TypeOK == count \in 0..10
====
