open Address
open Token
open Wallet

let a = Address.addr "a"

let t0 = init "t0"
let t1 = init "t1"
let t2 = mintAMM(t0,t1)

let w1 =
  Wallet.empty a
|> Wallet.update t0 1
|> Wallet.update t0 2
|> Wallet.update t1 4

let w2 = Wallet.set_balance (Wallet.balance_of_list [(t0,5)]) w1

let w3 = Wallet.update t0 (-1) w2;;

print_endline (Wallet.to_string w1);;
print_endline (Wallet.to_string w2);;
print_endline (Wallet.to_string w3);;
