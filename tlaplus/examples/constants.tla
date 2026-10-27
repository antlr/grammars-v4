---- MODULE Constants ----
EXTENDS Naturals

CONSTANT Limit
ASSUME Limit > 0

Double(n) == n * 2
Arithmetic == Double(1 + 2) = 6
Bounded == \A n \in 0..Limit : n <= Limit

====
