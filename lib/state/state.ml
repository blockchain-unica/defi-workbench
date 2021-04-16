open Token
open Address
open Wallet

module LP = struct

  type t = Token.t * int * (Address.t * int) list
	
  let make tau n debtfun = (tau,n,debtfun)
      
  let empty tau = (tau,0,[])

  let rec string_of_debt d = match d with
    [] -> ""
  | [(a,v)] -> (string_of_int v) ^ "/" ^ (Address.to_string a)
  | x::d' -> (string_of_debt [x]) ^ "," ^ (string_of_debt d')
    
  let to_string (tau,n,d) =
    "(" ^ (string_of_int n) ^ ":" ^
    (Token.to_string tau) ^ ",{" ^ (string_of_debt d) ^ "})"
end

   
module type StateType =
  sig
    type tw
    type tlp
    type t = tw * tlp

    val empty : t
    
    val get_wallet : Address.t -> t -> Wallet.t
	
    val get_lp : Token.t -> t -> LP.t
	
    val add_wallet : Address.t -> (Token.t * int) list -> t -> t

    val xfer : Address.t -> Address.t -> int -> Token.t -> t -> t

    val dep : Address.t -> int -> Token.t -> t -> t

    val to_string : t -> string
  end

 
module State : StateType =
  struct
 
    module WMap = Map.Make(Address)
    module LPMap = Map.Make(Token)
	
    type tw = Wallet.bt WMap.t
    type tlp = LP.t LPMap.t
    type t = tw * tlp
	  
    exception SameAddress
    exception InsufficientBalance of string
	
    let empty = (WMap.empty,LPMap.empty)
	
    let get_wallet a s = Wallet.make a (WMap.find a (fst s))
	
    let add_wallet a bal s =
      (WMap.add a (Wallet.balance_of_list bal) (fst s), snd s)
	
    let get_lp tau s = LPMap.find tau (snd s)
	
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
	
    let dep a v tau (w,lp) =
      let wa = get_wallet a (w,lp) in
      (* fails if a's balance of tau is < v *)
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let tau' = Token.mintLP tau in
      let wa' =	(wa
      |> Wallet.update tau (-v)
      |> Wallet.update tau' v) in 
      let w' = WMap.add a (Wallet.get_balance wa') w in
      try
	let (_,n,debt) = get_lp tau (w,lp) in
	let v' = v in
	(w',LPMap.add tau (LP.make tau (n+v') debt) lp)
      with Not_found -> (w', LPMap.add tau (LP.make tau v []) lp)

    let to_string (w,lp) =
      let ws = WMap.fold (fun a bal s -> s ^ (if s="" then "" else " | ") ^ (Wallet.to_string (Wallet.make a bal))) w "" in
      let lps = LPMap.fold (fun t lp0 s -> s ^ (if s="" then "" else " | ") ^ (LP.to_string lp0)) lp "" in
      ws ^ (if lps = "" then "" else " | " ^ lps)
	
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

let s3 = State.dep a 50 t0 s2;;
print_string (State.to_string s3);;

let s4 = State.dep b 10 t0 s3;;
print_string (State.to_string s4);;
