open Token
open Address
open Wallet

module State =
  struct

    module WMap = Map.Make(Address)
    module TMap = Map.Make(Token)

    exception SameAddress
    exception InsufficientBalance of string

    let empty = (WMap.empty,TMap.empty)

    let add_wallet a bal (w,lp) =
      let w' = WMap.add a bal w
      in (w',lp)
	  
    let get_wallet a (w,_) =
      Wallet.make a (WMap.find a w)

    let xfer a b v tau (w,lp) =
      if a=b then raise (SameAddress);
      let wa = get_wallet a (w,lp) in
      (* fails if a's balance of tau is < v *)
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let wb = get_wallet b (w,lp) in
      let wa' = Wallet.update tau (-v) wa in
      let wb' = Wallet.update tau v wb in
      let w' = 
	(w
	 (* removes v:tau from a's balance *)
      |> WMap.add a (Wallet.get_balance wa')
         (* adds v:tau to b's balance *)
      |> WMap.add b (Wallet.get_balance wb'))
      in (w',lp)
	    
    let to_string (w,lp) = WMap.fold (fun a bal s -> s ^ (if s="" then "" else " | ") ^ (Wallet.to_string (Wallet.make a bal))) w ""
  end
;;


let a = Address.addr "A";;
let b = Address.addr "B";;
let t0 = Token.init "t0";;
let t1 = Token.init "t1";;
    
let s0 = State.(
  empty
|> add_wallet a [(t0,100);(t1,50)]
|> add_wallet b [(t0,200)]
)
;;

print_string (State.to_string s0);;
let wa = State.get_wallet a s0;;
Wallet.balance t0 wa;;

let s1 = State.xfer a b 10 t0 s0;;
print_string (State.to_string s1);;

let s2 = State.xfer a b 10 t1 s1;;
print_string (State.to_string s2);;

let s3 = State.xfer a b 100 t1 s2;;
print_string (State.to_string s3);;

let s3 = State.xfer a a 10 t1 s2;;
print_string (State.to_string s3);;
