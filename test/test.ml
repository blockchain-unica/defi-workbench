open Address
open Token
open Wallet
open State
  
print_endline "Hello, world!"

let s0 = State.(
  empty
|> add_wallet a [(t0,100);(t1,50)]
|> add_wallet b [(t0,200)]
)
;;

State.to_string s0;;
  
