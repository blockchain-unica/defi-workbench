open Address
open Lp

module type StateType =
  sig
    type t

    type collType = Infty | Val of float

    val empty : t

    val get_wallet : Address.t -> t -> Wallet.t

    val get_lp : Token.t -> t -> Lp.t

    val get_price : Token.t -> t -> float

    val add_wallet : Address.t -> (Token.t * int) list -> t -> t

    (** [tok s] is the set of all tokens in state [s] *)
    val tok : t -> Token.t list

    (** [supply tau s] is the supply of token [tau] in state [s] *)
    val supply : Token.t -> t -> int

    (** [er tau s] is the exchange rate of a non-minted token in state [s] *)
    val er : Token.t -> t -> float

    (** value of free (non-collateralized) tokens *)
    val val_free : Address.t -> t -> float

    (** value of collateralized tokens *)
    val val_collateralized : Address.t -> t -> float

    (** value of non-minted tokens *)
    val val_debt : Address.t -> t -> float

    (** [networth a s] is the net worth of address [a] in state [s] *)
    val networth : Address.t -> t -> float

    (** collateralization of a user in a state *)
    val coll : Address.t -> t -> collType

    val xfer : Address.t -> Address.t -> int -> Token.t -> t -> t

    val dep : Address.t -> int -> Token.t -> t -> t

    val bor : Address.t -> int -> Token.t -> t -> t

    val accrue_int : t -> t

    val rep : Address.t -> int -> Token.t -> t -> t

    val rdm : Address.t -> int -> Token.t -> t -> t

    val liq : Address.t -> Address.t -> int -> Token.t -> Token.t -> t -> t

    val px : Token.t -> float -> t -> t

    val to_string : t -> string

    val id_print : t -> t

  end


module State : StateType =
  struct

    module WMap = Map.Make(Address)
    module LPMap = Map.Make(Token)

    type t = { wM : Wallet.bt WMap.t;
               lpM : (int * Lp.dt) LPMap.t;
               pF : (Token.t -> float) }

    type collType = Infty | Val of float

    let coll_min = 1.5
    let r_liq = 1.1

    exception MintedLP of string
    exception InsufficientBalance of string
    exception InsufficientDebt of string

    let empty = { wM = WMap.empty;
                  lpM = LPMap.empty;
                  pF = fun (_:Token.t) -> 0. }

    let get_wallet a s = Wallet.make a (WMap.find a s.wM)

    let add_wallet a bal s =
      { s with wM = WMap.add a (Wallet.balance_of_list bal) s.wM }

    let get_lp tau s =
      let (n,d) = (LPMap.find tau s.lpM) in Lp.make tau n d

    let supply tau s =
      let nw = WMap.fold
	  (fun a bal n -> n + Wallet.balance tau (Wallet.make a bal)) s.wM 0 in
      try
	let (r,_) = LPMap.find tau s.lpM in nw + r
      with Not_found -> nw

    let er tau s =
      (* TODO: check that tau is non-minted *)
      try
	let (r,d) = LPMap.find tau s.lpM in
	let dsum = List.fold_right (fun x n -> n + snd x) (Lp.list_of_debt d) 0 in
	float_of_int (r + dsum) /. float_of_int (supply (Token.mintLP tau) s)
      with Not_found -> 1.

    let get_price tau s = s.pF tau

    let val_free a s =
      let bl = Wallet.list_of_balance (WMap.find a s.wM)
      in List.fold_right
      (fun x n -> n +. (float_of_int (snd x) *. get_price (fst x) s))
      (List.filter (fun x -> not (Token.isMintedLP (fst x))) bl)
      0.
      
    let rec filter_map f l = match l with
        [] -> []
      | x::l' -> (match f x with 
        None -> filter_map f l'
        | Some y -> y::(filter_map f l'))
         
    let val_collateralized a s =
      let bl = Wallet.list_of_balance (WMap.find a s.wM)
      in List.fold_right
      (fun x n -> n +. (float_of_int (snd x) *. (er (fst x) s) *. get_price (fst x) s))
      (filter_map (fun (tau,v) -> match Token.uLP(tau) with 
        None -> None 
      | Some tau' -> Some (tau',v)) bl)
      0.

    let val_debt a s =
      LPMap.fold
      (fun t p acc ->
        acc +.
          (float_of_int (Lp.debt_of a (snd p))) *.
          (get_price t s))
      s.lpM
      0.

    let networth a s = 
      (val_free a s +. val_collateralized a s) -. (val_debt a s)

    let coll a s =
      if val_debt a s > 0.
      then Val ((val_collateralized a s) /. (val_debt a s))
      else Infty

    let rec set_of_list l = match l with
        [] -> []
      | x::l' -> let s = set_of_list l' in 
                 if List.mem x s then s else x::s

    let dom l =
      let rec dom_list l = (match l with
          [] -> []
        | (x,_)::l' -> x::(dom_list l'))
      in set_of_list (dom_list l)

    let tok s =
      set_of_list (WMap.fold (fun _ bal acc -> (dom (Wallet.list_of_balance bal)) @ acc) s.wM [])

    let tokFree s = List.filter (fun x -> not (Token.isMintedLP x)) (tok s)


    (**************************************************)
    (*                        Xfer                    *)
    (**************************************************)

    let xfer a b v tau s =
      if v<0 then invalid_arg "Xfer: trying to transfer a negative amount";
      if a=b then invalid_arg "Xfer: trying to transfer to the same address";
      let wa = get_wallet a s in
      (* fails if a's balance of tau is < v *)
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let wb = get_wallet b s in
      let wa' = Wallet.update tau (-v) wa in
      let wb' = Wallet.update tau v wb in
      let wM' =
	(s.wM
	   (* removes v:tau from a's balance *)
      |> WMap.add a (Wallet.get_balance wa')
          (* adds v:tau to b's balance *)
      |> WMap.add b (Wallet.get_balance wb'))
      in { s with wM = wM' }


    (**************************************************)
    (*                        Dep                     *)
    (**************************************************)

    let dep a v tau s =
      if v<0 then invalid_arg "Dep: trying to transfer a negative amount";
      let wa = get_wallet a s in
      (* fails if a's balance of tau is < v *)
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let tau' = Token.mintLP tau in
      let wa' =	(wa
      |> Wallet.update tau (-v)
      |> Wallet.update tau' v) in
      let wM' = WMap.add a (Wallet.get_balance wa') s.wM in
      try
	let lp0 = get_lp tau s in
	let v' = int_of_float ((float_of_int v) /. (er tau s)) in
	let n' = v' + Lp.get_balance lp0 in
	let d = Lp.get_debt lp0 in
	{ s with wM = wM'; lpM = LPMap.add tau (n',d) s.lpM }
      with Not_found ->	
        { s with wM = wM'; lpM = LPMap.add tau (v,Lp.debt_of_list []) s.lpM }


    (**************************************************)
    (*                        Bor                     *)
    (**************************************************)

    let bor a v tau s =
      if v<0 then invalid_arg "Bor: trying to transfer a negative amount";
      let wa = get_wallet a s in
      let wa' =	(wa |> Wallet.update tau v) in
      let wM' = WMap.add a (Wallet.get_balance wa') s.wM in
      let lp = get_lp tau s in
      let r = Lp.get_balance lp in
      if r<v then raise (InsufficientBalance (Lp.to_string lp));
      let d' = Lp.update_debt a v (Lp.get_debt lp) in
      let s' = { s with wM = wM'; lpM = LPMap.add tau (r-v,d') s.lpM } in
      match coll a s' with
	Val c when c < coll_min -> 
          failwith ("Bor: " ^ (Address.to_string a) ^ " collateralization after action is " ^ (string_of_float c) ^ " < " ^ (string_of_float coll_min))
      | _ -> s'


    (**************************************************)
    (*                        Int                     *)
    (**************************************************)

    let accrue_int s =
      (* intr is the interest function (Token.t -> t -> float) *)
      let intr _ _ = 0.14 in
      let lpM' = LPMap.mapi
        (fun tau p ->
          let d' = Lp.accrue_int (1. +. (intr tau s)) (snd p)
          in (fst p, d'))
        s.lpM
      in { s with lpM = lpM' }


    (**************************************************)
    (*                        Rep                     *)
    (**************************************************)

    let rep a v tau s =
      if v<0 then invalid_arg "Rep: trying to transfer a negative amount";
      let wa = get_wallet a s in
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let wa' =	(wa |> Wallet.update tau (-v)) in
      let wM' = WMap.add a (Wallet.get_balance wa') s.wM in
      let (r,d) = LPMap.find tau s.lpM in
      if Lp.debt_of a d < v then raise (InsufficientDebt "Rep");
      let d' = Lp.update_debt a (-v) d in
      let lpM' = LPMap.add tau (r+v,d') s.lpM in
      { s with wM = wM'; lpM = lpM' }


    (**************************************************)
    (*                        Rdm                     *)
    (**************************************************)

    let rdm a v tau s =
      if v<0 then invalid_arg "Rdm: trying to transfer a negative amount";
      if (Token.isMintedLP tau) then raise (MintedLP (Token.to_string tau));
      let wa = get_wallet a s in
      if Wallet.balance (Token.mintLP tau) wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let v' = int_of_float ((float_of_int v) *. (er tau s)) in
      let wa' =	(wa
                    |> Wallet.update (Token.mintLP tau) (-v) 
                    |> Wallet.update tau v') in
      let wM' = WMap.add a (Wallet.get_balance wa') s.wM in
      let (r,d) = LPMap.find tau s.lpM in
      if r < v' then raise (InsufficientBalance "Rdm");
      let lpM' = LPMap.add tau (r-v',d) s.lpM in
      { s with wM = wM'; lpM = lpM' }


    (**************************************************)
    (*                        Liq                     *)
    (**************************************************)

    let liq a b v tau tau' s =
      if v<0 then invalid_arg "Liq: trying to transfer a negative amount";
      if a=b then invalid_arg "Xfer: trying to transfer to the same address";
      (match coll b s with
	Val c when c < coll_min -> ()
      | Val c -> failwith ("Liq: " ^ (Address.to_string b) ^ " collateralization before action is " ^ (string_of_float c) ^ " >= " ^ (string_of_float coll_min))
      | Infty -> failwith ("Liq: " ^ (Address.to_string b) ^ " collateralization before action is Infty >= " ^ (string_of_float coll_min)));
      if (Token.isMintedLP tau') then raise (MintedLP (Token.to_string tau'));
      let wa = get_wallet a s in
      (* fails if a's balance of tau is < v *)
      if Wallet.balance tau wa < v
      then raise (InsufficientBalance (Address.to_string a));
      let v' = int_of_float (((float_of_int v) *. r_liq *. (get_price tau s)) /. ((er tau' s) *. (get_price tau' s))) in
      let wb = get_wallet b s in
      if Wallet.balance (Token.mintLP tau') wb < v'
      then raise (InsufficientBalance (Address.to_string b));
      let wa' = Wallet.(wa |> update tau (-v) |> update (Token.mintLP tau') v')  in
      let wb' = Wallet.(wb |> update (Token.mintLP tau') (-v')) in
      let wM' =
	(s.wM
            |> WMap.add a (Wallet.get_balance wa')
            |> WMap.add b (Wallet.get_balance wb')) in
      let (r,d) = LPMap.find tau s.lpM in
      if Lp.debt_of b d < v then raise (InsufficientDebt (Address.to_string b));
      let d' = Lp.update_debt b (-v) d in
      let lpM' = LPMap.add tau (r+v,d') s.lpM in
      let s' = { s with wM = wM'; lpM = lpM' } in
      (match coll b s' with
	Val c when c <= coll_min -> s'
      | Val c -> failwith ("Liq: " ^ (Address.to_string b) ^ " collateralization after action is " ^ (string_of_float c) ^ " >= " ^ (string_of_float coll_min))
      | Infty -> failwith ("Liq: " ^ (Address.to_string b) ^ " collateralization after action is Infty >= " ^ (string_of_float coll_min)))


    (**************************************************)
    (*                        Px                      *)
    (**************************************************)

    let px tau v s =
      if v<=0. then invalid_arg "Px: trying to set a negative price";
      { s with pF = (fun x -> if x=tau then v else s.pF x) }


    let to_string s =
      let ws = WMap.fold
	  (fun a bal s -> s ^ (if s="" then "" else " | ") ^ (Wallet.to_string (Wallet.make a bal)))
	  s.wM "" in
      let lps = LPMap.fold
	  (fun t p s -> s ^ (if s="" then "" else " | ") ^ (Lp.to_string (Lp.make t (fst p) (snd p))))
	  s.lpM "" in
      let ps = "{" ^ List.fold_right
        (fun x acc -> (Token.to_string x) ^ "->" ^ (string_of_float (s.pF x)) ^ "," ^ acc) (tokFree s) "_->0}" in
      ws ^ (if lps = "" then "" else " | " ^ lps) ^ " | " ^ ps

    let id_print s = print_endline (to_string s); s

  end

;;

(*

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
  |> bor a 15 t0
  |> id_print
)
;;

State.val_free a s;;

State.coll a s;;
State.supply t0 s;;
State.supply t1 s;;
State.supply (Token.mintLP t0) s;;
State.supply (Token.mintLP t1) s;;
State.er t0 s;;
State.er t1 s;;

*)
