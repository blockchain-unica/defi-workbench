String.compare "a" "bb";;
  
module Token = struct
  type t = InitTok of string | LPTok of t | AMMTok of t * t

  let initTok s = InitTok s
      
  let ammTok (t1,t2) = AMMTok (t1,t2)

  let lpTok t1 = LPTok t1
    
  let rec compare t1 t2 = match t1 with
    InitTok(s1) -> (match t2 with
      InitTok(s2) -> String.compare s1 s2
    | _ -> -1
    )
  | LPTok(t1') -> (match t2 with
    | LPTok(t2') -> compare t1' t2'
    | AMMTok(_,_) -> -1	
    | _ -> 1)
  | AMMTok(t1l,t1r) -> (match t2 with
      AMMTok(t2l,t2r) ->
	let cmpL = compare t1l t2l in
	if cmpL <> 0 then cmpL
	else compare t1r t2r
    | _ -> 1
)
end
;;

module TMap = Map.Make(Token);;

let t0 = Token.initTok "t0";;
let t1 = Token.initTok "t1";;
let t2 = Token.ammTok(t0,t1);;
let t3 = Token.lpTok(t2);;

Token.compare t0 t3;;
  
let tm1 = TMap.(
  empty
|> add t1 (1,2)
|> add t2 (2,3))
;;

TMap.iter (fun (p1,p2) (x,y) -> print_string p1; print_string p2; print_string " -> "; print_int x; print_int y ; print_newline()) pm1;;
