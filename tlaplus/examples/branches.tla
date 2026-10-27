---- MODULE Branches ----
EXTENDS Integers

(* --algorithm branches
variables value = -1, result = "";
begin
Classify:
  if value < 0 then
    result := "negative";
  elsif value = 0 then
    result := "zero";
  else
    result := "positive";
  end if;
Check:
  assert result = "negative";
end algorithm *)

Sign(n) == IF n < 0 THEN -1 ELSE IF n = 0 THEN 0 ELSE 1
====
