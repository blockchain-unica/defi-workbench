(* types *)

(* Users a, b, ... *)
type userT = string;;
(* Tokens t, t0, t1, ... *)
type tokT = Init of string | AmmTok of tokT * tokT;;
(* Wallets wa, wb, ... *)
type walletT = userT * (tokT * int) list;;
(* AMMs *)
type ammT = int * tokT * int * tokT;;

(* Transactions tx, tx0, ... *)
type txT =
    Xfer of userT * int * tokT * userT
  | Dep of userT * int * tokT * int * tokT
  | SwapL of userT * int * tokT * int * tokT
  | SwapR of userT * int * tokT * int * tokT	
  | Rdm of userT * int * tokT;;

(* terms s, s0, s1, ... *)
type termT = Wal of walletT | Amm of ammT;;
(* states sl, sl', ... *)
type confT = termT list;;


(* balance manipulation *)

type 'a applyT = Unbound | Val of 'a;;
    
let rec apply f x = match f with
  [] -> Unbound
| (y,v)::f' -> if y=x then Val v else apply f' x
;;

let rec update f x v = match f with
  [] -> [(x,v)]
| (x',v')::f' -> if x'=x then (x,v + v')::f' else (x',v')::(update f' x v)
;;

let balance s t = match s with
  Wal(a,f) -> (match (apply f t) with Unbound -> 0 | Val v -> v)
| Amm(v0,t0,v1,t1) -> if t=t0 then v0 else if t=t1 then v1 else 0
;;

let updateBal s v t = match s with
  Wal(a,f) -> Wal(a,update f t v)
| Amm(v0,t0,v1,t1) when t=t0 -> Amm(v0+v,t0,v1,t1)
| Amm(v0,t0,v1,t1) when t=t1 -> Amm(v0,t0,v1+v,t1)
| _ -> s
;;

(* token supply *)

let rec supply sl t = match sl with
  [] -> 0
| s::sl' -> balance s t + supply sl' t
;;

(* string conversion *)

let rec string_of_tok t = match t with
  Init s -> s
| AmmTok(t0,t1) -> "(" ^ (string_of_tok t0) ^ "," ^ (string_of_tok t1) ^ ")";;
  
let rec string_of_bal f = match f with
  [] -> ""
| [(t,v)] -> (string_of_int v) ^ ":" ^ (string_of_tok t)
| b::f' -> (string_of_bal [b]) ^ "," ^ (string_of_bal f')
;;

let string_of_tx tx = match tx with
  Xfer(a,v,t,b) -> "Xfer(" ^ a ^ "," ^ string_of_int v ^ "," ^ string_of_tok t ^ "," ^ b ^ ")"
| Dep(a,v0,t0,v1,t1) -> "Dep(" ^ a ^ "," ^ string_of_int v0 ^ "," ^ string_of_tok t0 ^ "," ^ string_of_int v1 ^ "," ^ string_of_tok t1 ^ ")"
| SwapL(a,v0,t0,v1,t1) -> "SwapL(" ^ a ^ "," ^ string_of_int v0 ^ "," ^ string_of_tok t0 ^ "," ^ string_of_int v1 ^ "," ^ string_of_tok t1 ^ ")"
| SwapR(a,v0,t0,v1,t1) -> "SwapR(" ^ a ^ "," ^ string_of_int v0 ^ "," ^ string_of_tok t0 ^ "," ^ string_of_int v1 ^ "," ^ string_of_tok t1 ^ ")"
| Rdm(a,v,t) -> "Rdm(" ^ a ^ "," ^ string_of_int v ^ "," ^ string_of_tok t ^ ")"
;;

let string_of_term term = match term with
  Wal(a,f) -> a ^ "[" ^ (string_of_bal f) ^ "]"
| Amm(v0,t0,v1,t1) -> "(" ^ (string_of_int v0) ^ ":" ^ (string_of_tok t0) ^ "," ^ (string_of_int v1) ^ ":" ^ (string_of_tok t1) ^ ")"
;;

let rec string_of_conf c = match c with
  [] -> ""
| [t] -> string_of_term t
| t::c' -> string_of_term t ^ " | " ^ string_of_conf c'
;;


(* pattern matching on configurations *)

(* match 1 wallet *)

type matchWT = NoMatchW | MatchW of termT * confT;;

let rec matchW a conf = match conf with
  [] -> NoMatchW
| Wal(a',s)::conf' when a'=a -> MatchW(Wal(a',s),conf')
| t::conf' -> (match (matchW a conf') with
    NoMatchW -> NoMatchW
  | MatchW(w,c) -> MatchW(w,t::c))
;;

(* match 2 wallets *)

type matchWWT = NoMatchWW | MatchWW of termT * termT * confT;;

let rec matchWW a b conf = match matchW a conf with
  NoMatchW -> NoMatchWW
| MatchW(wa,c) -> (match (matchW b c) with
    NoMatchW -> NoMatchWW
  | MatchW(wb,c') -> MatchWW(wa,wb,c'))
;;

(* match 1 AMM *)

type matchAT = NoMatchA | MatchA of termT * confT;;

let rec matchA (t0,t1) sl = match sl with
  [] -> NoMatchA
| Amm(v0,t0',v1,t1')::sl' when t0'=t0 && t1=t1' -> MatchA(Amm(v0,t0',v1,t1'),sl')
| s::sl' -> (match (matchA (t0,t1) sl') with
    NoMatchA -> NoMatchA
  | MatchA(amm,sl'') -> MatchA(amm,s::sl''))
;;


let stepXfer(sl,a,v,t,b) = match (matchWW a b sl) with
  NoMatchWW -> failwith "Xfer: no wallets"
| MatchWW(wa,wb,sl') ->
    if balance wa t < v then failwith "Xfer: insufficient balance"
    else (updateBal wa (-v) t)::(updateBal wb v t)::sl'
;;

let stepDep(sl,a,v0,t0,v1,t1) = match (matchW a sl) with
  NoMatchW -> failwith "Dep: no wallet"
| MatchW(wa,sl') ->
    if balance wa t0 < v0 then failwith "Dep: insufficient balance"
    else if balance wa t1 < v1 then failwith "Dep: insufficient balance"
    else (match (matchA (t0,t1) sl') with
      NoMatchA ->
	(match (matchA (t1,t0) sl') with
	  NoMatchA -> (* token pair t0,t1 does not exist yet *)
	    let wa' =
	      updateBal
		(updateBal
		   (updateBal wa (-v0) t0)
		   (-v1) t1)
		v0 (AmmTok(t0,t1))
	    in wa'::Amm(v0,t0,v1,t1)::sl'
	| MatchA(s,sl'') -> failwith "Dep: AMM pair exists in the other order")
    | MatchA(s,sl'') -> (match s with
	Amm(r0,t0',r1,t1') ->
	  if r1 * v0 != r0 * v1 then failwith "Dep: wrong ratio"
	  else let amm' = Amm(r0+v0,t0,r1+v1,t1)
	  in let v = (v0 * supply (wa::s::sl'') (AmmTok(t0,t1))) / r0
	  in let wa' =
	    updateBal
	      (updateBal
		 (updateBal wa (-v0) t0)
		 (-v1) t1)
	      v (AmmTok(t0,t1))
	  in wa'::amm'::sl''
      | _ -> failwith "this cannot happen"))
;;


let stepSwapL(sl,a,v0,t0,v1,t1,invF) = match (matchW a sl) with
  NoMatchW -> failwith "SwapL: no wallet"
| MatchW(wa,sl') ->
    if balance wa t0 < v0 then failwith "SwapL: insufficient balance"
    else (match (matchA (t0,t1) sl') with
      NoMatchA -> failwith "SwapL: token pair does not exist"
    | MatchA(s,sl'') -> (match s with
	Amm(r0,t0',r1,t1') ->
	  let v1' = invF r0 r1 v0 in
	  if (v1' < v1 || v1' < 0)
	  then failwith "SwapL: assert on funds invariant failed"
	  else let wa' =
	    updateBal
	      (updateBal wa (-v0) t0)
	      (+v1') t1
	  in wa'::Amm(r0+v0,t0,r1-v1',t1)::sl''
      | _ -> assert false))
;;

let stepSwapR(sl,a,v0,t0,v1,t1,invF) = match (matchW a sl) with
  NoMatchW -> failwith "SwapR: no wallet"
| MatchW(wa,sl') ->
    if balance wa t1 < v1 then failwith "SwapR: insufficient balance"
    else (match (matchA (t0,t1) sl') with
      NoMatchA -> failwith "SwapR: token pair does not exist"
    | MatchA(s,sl'') -> (match s with
	Amm(r0,t0',r1,t1') ->
	  let v0' = invF r1 r0 v1 in
	  if (v0' < v0 || v0' < 0)
	  then failwith "SwapR: assert on funds invariant failed"
	  else let wa' =
	    updateBal
	      (updateBal wa (+v0') t0)
	      (-v1) t1
	  in wa'::Amm(r0-v0',t0,r1+v1,t1)::sl''
      | _ -> assert false))
;;

let stepRdm(sl,a,v,t) = match (matchW a sl) with
  NoMatchW -> failwith "Rdm: no wallet"
| MatchW(wa,sl') ->
    if balance wa t < v then failwith "Rdm: insufficient balance"
    else match t with
      AmmTok(t0,t1) -> 
	(match (matchA (t0,t1) sl') with
	  NoMatchA -> failwith "Rdm: token pair does not exist"
	| MatchA(s,sl'') -> (match s with
	    Amm(r0,t0',r1,t1') ->
	      let r = supply sl t
	      in let v0 = (v * r0) / r
	      in let v1 = (v * r1) / r
	      in let wa' =
		updateBal		
		  (updateBal
		     (updateBal wa (+v0) t0)
		     (+v1) t1)
		  (-v) t
	      in wa'::Amm(r0-v0,t0,r1-v1,t1)::sl''
	  | _ -> assert false))
    | _ -> failwith "Rdm: cannot redeem an initial token"
;;

(* swapL: r=r0, p=r1, rx=v0 *)
(* swapR: r=r1, p=r0, rx=v1 *)
let invProdFun r p rx = (p * rx) / (r+rx);;
  
let step1 tx sl invF = match tx with
  Xfer(a,v,t,b) -> stepXfer(sl,a,v,t,b)
| Dep(a,v0,t0,v1,t1) -> stepDep(sl,a,v0,t0,v1,t1)
| SwapL(a,v0,t0,v1,t1) -> stepSwapL(sl,a,v0,t0,v1,t1,invF)
| SwapR(a,v0,t0,v1,t1) -> stepSwapR(sl,a,v0,t0,v1,t1,invF)
| Rdm(a,v,t) -> stepRdm(sl,a,v,t)      
;;

let rec step txl sl invF = match txl with
  [] -> sl
| tx::txl' -> let sl' = step1 tx sl invF in step txl' sl' invF
;;

let rec trace txl sl invF = match txl with
  [] -> [sl]
| tx::txl' -> let sl' = step1 tx sl invF in sl::(trace txl' sl' invF)
;;

let rec print_trace txl sl invF = match txl with
  [] -> print_string (string_of_conf sl)
| tx::txl' ->
    (print_string (string_of_conf sl));
    print_newline ();
    print_string "--";
    print_string (string_of_tx tx);
    print_string "-->";
    print_newline ();   
    let sl' = step1 tx sl invF
    in print_trace txl' sl' invF
;;

let rec price t sl o = match t with
  Init t' -> o t
| AmmTok(t0,t1) -> (match matchA (t0,t1) sl with
    MatchA(Amm(r0,t0',r1,t1'),sl') when t0'=t0 && t1'=t1 ->
      (((float_of_int r0) *. price t0 sl o) +. ((float_of_int r1) *. price t1 sl o)) /. (float_of_int (supply sl (AmmTok(t0,t1))))
  | _ -> failwith "Price: token not present in state")
;;

let rec networth_bal f sl o = match f with
  [] -> 0.
| (t,v)::f' when v=0 -> networth_bal f' sl o
| (t,v)::f' -> ((float_of_int v) *. price t sl o) +. networth_bal f' sl o      
;;

let networth a sl o = match (matchW a sl) with
  MatchW(Wal(a',f),sl') when a'=a -> networth_bal f sl o
| _ -> 0.
;;

let rec global_networth_rec sl slConst o = match sl with
  [] -> 0.
| Wal(a,f)::sl' ->
    (networth_bal f slConst o) +. (global_networth_rec sl' slConst o)
| Amm(v0,t0,v1,t1)::sl' -> global_networth_rec sl' slConst o
;;

let global_networth sl o = global_networth_rec sl sl o;;

let print_networth txl sl o inv =
  List.map (fun x ->
    print_string "W: ";
    print_float (global_networth x o);
    print_string " WA: ";
    print_float (networth "A" x o);
    print_string " WB: ";
    print_float (networth "B" x o);
    print_newline())
  (trace txl sl inv);;
