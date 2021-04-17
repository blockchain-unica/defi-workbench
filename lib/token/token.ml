type t = InitTok of string | LPTok of t | AMMTok of t * t

let init s = InitTok s

let mintAMM (t1,t2) = AMMTok (t1,t2)

let mintLP t1 = LPTok t1

let isMintedLP tau = match tau with LPTok _ -> true | _ -> false

let rec to_string tau = match tau with
    InitTok s -> s
  | LPTok tau' -> "{" ^ to_string(tau') ^ "}"
  | AMMTok(tau0,tau1) -> "(" ^ to_string(tau0) ^ "," ^ to_string(tau1) ^ ")"

let compare tau1 tau2 =
  String.compare (to_string tau1) (to_string tau2)

(*
  let rec is_free tau = match tau with
  InitTok _ -> true
  | LPTok _ -> false
  | AMMTok(tau0,tau1) -> is_free tau0 && is_free tau1
*)

