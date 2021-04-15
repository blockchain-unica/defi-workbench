module Token = struct
  type t = InitTok of string | LPTok of t | AMMTok of t * t

  let initTok s = InitTok s
      
  let ammTok (t1,t2) = AMMTok (t1,t2)

  let lpTok t1 = LPTok t1
    
  let rec compare tau1 tau2 = match tau1 with
    InitTok(s1) -> (match tau2 with
      InitTok(s2) -> String.compare s1 s2
    | _ -> -1
    )
  | LPTok(tau1') -> (match tau2 with
    | LPTok(tau2') -> compare tau1' tau2'
    | AMMTok(_,_) -> -1	
    | _ -> 1)
  | AMMTok(tau1l,tau1r) -> (match tau2 with
      AMMTok(tau2l,tau2r) ->
	let cmpL = compare tau1l tau2l in
	if cmpL <> 0 then cmpL
	else compare tau1r tau2r
    | _ -> 1)

  let rec to_string tau = match tau with
    InitTok(s) -> s
  | LPTok(tau') -> "{" ^ to_string(tau') ^ "}"
  | AMMTok(tau0,tau1) -> "(" ^ to_string(tau0) ^ "," ^ to_string(tau1) ^ ")"
    
end
;;
