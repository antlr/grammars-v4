---- MODULE Choice ----
EXTENDS Naturals

(* --algorithm choice
variables chosen = 0;
begin
Choose:
  with n \in {1, 2, 3}, offset = 1 do
    chosen := n + offset;
  end with;
Check:
  assert chosen \in 2..4;
end algorithm *)

====
