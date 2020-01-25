---- MODULE transfer ----
EXTENDS Naturals, TLC

(* --algorithm transfer
variables alice_account = 10, bob_account = 10, money = 5;

begin
A: alice_account := alice_account - money;
B: bob_account := bob_account + money;

end algorithm *)
====
