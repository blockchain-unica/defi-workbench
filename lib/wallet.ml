type bt = (Token.t * int) list
type t = Address.t * bt

type balType = Unbound | Val of int

let make a bfun = (a,bfun)

let empty a = (a,[])

let rec apply f x = match f with
    [] -> Unbound
  | (y,v)::f' -> if y=x then Val v else apply f' x

let balance tau w =
  match (apply (snd w) tau) with Unbound -> 0 | Val v -> v

let rec bind f x v = match f with
    [] -> [(x,v)]
  | (x',v')::f' -> if x'=x then (x,v + v')::f' else (x',v')::(bind f' x v)

let update tau v (a,bfun) = (a,bind bfun tau v)

let get_address w = fst w

let get_balance w = snd w

let list_of_balance l = l

let balance_of_list l = l

let set_balance bal (a,_) = (a,bal)

let rec string_of_bal f = match f with
    [] -> ""
  | [(t,v)] -> (string_of_int v) ^ ":" ^ (Token.to_string t)
  | b::f' -> (string_of_bal [b]) ^ "," ^ (string_of_bal f')

let to_string (a,bfun) = (Address.to_string a) ^ "[" ^ (string_of_bal bfun) ^ "]"


