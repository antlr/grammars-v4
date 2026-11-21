---- MODULE Transfer ----
EXTENDS Naturals, TLC

(* --algorithm transfer
variables alice_account = 10, bob_account = 10, money \in 1..20,
          account_total = alice_account + bob_account;

begin
Transfer:
  if alice_account >= money then
    A: alice_account := alice_account - money;
       bob_account := bob_account + money; \* Both now part of A
end if;
C: assert alice_account >= 0;

end algorithm *)

MoneyNotNegative == money >= 0
MoneyInvariant == alice_account + bob_account = account_total

====