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

    (* Supply of a token in a state *)
    val supply : Token.t -> t -> int

    (* Exchange rate ER of a non-minted token in a state *)	
    val er : Token.t -> t -> float
	
    val xfer : Address.t -> Address.t -> int -> Token.t -> t -> t

    val dep : Address.t -> int -> Token.t -> t -> t

    val to_string : t -> string

    val id_print : t -> t

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

    let supply tau (wM,lpM) =
      let nw = WMap.fold
	  (fun a bal n -> n + Wallet.balance tau (Wallet.make a bal)) wM 0 in
      try 
	let (r,_) = LPMap.find tau lpM in nw + r
      with Not_found -> nw
	  
    let er tau s =
      (* TODO: check that tau is non-minted *)
      try 
	let (r,d) = LPMap.find tau (snd s) in
	let dsum = List.fold_right (fun x n -> n + snd x) (Lp.list_of_debt d) 0 in
	float_of_int (r + dsum) /. float_of_int (supply (Token.mintLP tau) s)
      with Not_found -> 1.
	  
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
      let ws = WMap.fold
	  (fun a bal s -> s ^ (if s="" then "" else " | ") ^ (Wallet.to_string (Wallet.make a bal)))
	  w "" in
      let lps = LPMap.fold
	  (fun t p s -> s ^ (if s="" then "" else " | ") ^ (Lp.to_string (Lp.make t (fst p) (snd p))))
	  lp "" in
      ws ^ (if lps = "" then "" else " | " ^ lps)

    let id_print s = print_endline (to_string s); s
	
  end

;;
   
let a = Address.addr "A";;
let b = Address.addr "B";;
let t0 = Token.init "t0";;
let t1 = Token.init "t1";;
    
let s = State.(
  empty
  |> id_print    
  |> add_wallet a [(t0,100);(t1,50)]
  |> id_print
  |> add_wallet b [(t0,200)]
  |> id_print
  |> xfer a b 10 t0
  |> id_print
  |> xfer a b 10 t1
  |> id_print
  |> dep a 50 t0      
  |> id_print      
  |> dep b 10 t0
  |> id_print      
)
;;

State.supply t0 s;;
State.supply t1 s;;
State.supply (Token.mintLP t0) s;;
State.supply (Token.mintLP t1) s;;
State.er t0 s;;
State.er t1 s;;
