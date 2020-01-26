---- MODULE Transfer ----
EXTENDS Naturals, TLC

(* --algorithm transfer
variables alice_account = 10, bob_account = 10,
          account_total = alice_account + bob_account;

process Transfer \in 1..2
  variable money \in 1..20;
begin
Transfer:
  if alice_account >= money then
    A: alice_account := alice_account - money;
       bob_account := bob_account + money;
end if;
C: assert alice_account >= 0;
end process

end algorithm *)

MoneyNotNegative == money >= 0
MoneyInvariant == alice_account + bob_account = account_total

====
