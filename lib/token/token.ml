module type TokenType =
  sig
    type t

    val init : string -> t
    val mintAMM : t * t -> t
    val mintLP : t -> t

    val to_string : t -> string

    val compare : t -> t -> int
  end
      
module Token : TokenType =
      struct
  type t = InitTok of string | LPTok of t | AMMTok of t * t

  let init s = InitTok s
      
  let mintAMM (t1,t2) = AMMTok (t1,t2)

  let mintLP t1 = LPTok t1

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

end
;;
