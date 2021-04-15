open Address
open Token
open Wallet

let a = Address.addr "a"
    
let t0 = Token.initTok "t0"
let t1 = Token.initTok "t1"
let t2 = Token.ammTok(t0,t1)

let w1 = Wallet.(
  empty a
|> Wallet.update t0 1
|> Wallet.update t0 2
|> Wallet.update t1 4)
;;

print_endline (Wallet.to_string w1)
