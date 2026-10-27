---- MODULE SetsAndActions ----
EXTENDS Naturals, Sequences

VARIABLE x
Small == {0, 1, 2}
Positive == {n \in Small : n > 0}
Sequence == <<1, 2>> \o <<3>>
Init == x = 0
Next == x' = x + 1
Stable == UNCHANGED x
Safety == [] (x >= 0)
Possible == \E n \in Positive : n = 1
Implication == (1 \in Positive) => ~(0 \in Positive)
====
