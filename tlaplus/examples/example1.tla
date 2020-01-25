---- MODULE Transfer ----
EXTENDS Naturals, TLC

(* --algorithm transfer
variables alice_account = 10, bob_account = 10, money \in 1..20;

begin
Transfer:
  if alice_account >= money then
    A: alice_account := alice_account - money;
    B: bob_account := bob_account + money;
end if;
C: assert alice_account >= 0;

end algorithm *)

\* BEGIN TRANSLATION
\* Removed for clarity...
\* END TRANSLATION

MoneyNotNegative == money >= 0

====