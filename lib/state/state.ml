open Token
open Address
open Wallet
open Lp
     
module type StateType =
  sig
    type t
	  
    val empty : t
    
    val get_wallet : Address.t -> t -> Wallet.t
	
    val get_lp : Token.t -> t -> Lp.t
	
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
    type tlp = (int * Lp.dt) LPMap.t
    type t = tw * tlp

    exception SameAddress
    exception InsufficientBalance of string
	
    let empty = (WMap.empty,LPMap.empty)
	
    let get_wallet a s = Wallet.make a (WMap.find a (fst s))
	
    let add_wallet a bal s =
      (WMap.add a (Wallet.balance_of_list bal) (fst s), snd s)
	
    let get_lp tau s =
      let (n,d) = (LPMap.find tau (snd s)) in Lp.make tau n d
	
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
	let lp0 = get_lp tau (w,lp) in
	let v' = v in
	let n' = v' + Lp.get_balance lp0 in
	let d = Lp.get_debt lp0 in
	(w',LPMap.add tau (n',d) lp)
      with Not_found ->	(w', LPMap.add tau (v,Lp.debt_of_list []) lp)

    let to_string (w,lp) =
      let ws = WMap.fold (fun a bal s -> s ^ (if s="" then "" else " | ") ^ (Wallet.to_string (Wallet.make a bal))) w "" in
      let lps = LPMap.fold (fun t p s -> s ^ (if s="" then "" else " | ") ^ (Lp.to_string (Lp.make t (fst p) (snd p)))) lp "" in
      ws ^ (if lps = "" then "" else " | " ^ lps)
	
  end

;;
   
let a = Address.addr "A";;
let b = Address.addr "B";;
let t0 = Token.init "t0";;
let t1 = Token.init "t1";;

let print_state = fun s -> (print_endline (State.to_string s); s);;
    
let s = State.(
  empty
  |> print_state    
  |> add_wallet a [(t0,100);(t1,50)]
  |> print_state
  |> add_wallet b [(t0,200)]
  |> print_state
  |> xfer a b 10 t0
  |> print_state
  |> xfer a b 10 t1
  |> print_state
  |> dep a 50 t0      
  |> print_state      
  |> dep b 10 t0
  |> print_state      
)
;;
