---- MODULE Processes ----
EXTENDS Naturals, TLC

(* --algorithm workers
variables total = 0;
process Worker \in {1, 2}
  variable finished = FALSE;
begin
Work:
  await total < 2;
  total := total + 1 || finished := TRUE;
Check:
  assert finished;
  print total;
end process
end algorithm *)

Bounded == total <= 2
====
