open Token
open Address

module type WalletType = sig

  type t

  type balType = Unbound | Val of int
      
  val empty : Address.t -> t

  val balance : Token.t -> t -> int

  val update : Token.t -> int -> t -> t

  val get_address : t -> Address.t
      
  val to_string : t -> string
      
end
  
module Wallet : WalletType = struct

  type t = Address.t * (Token.t * int) list

  type balType = Unbound | Val of int

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

  let rec string_of_bal f = match f with
    [] -> ""
  | [(t,v)] -> (string_of_int v) ^ ":" ^ (Token.to_string t)
  | b::f' -> (string_of_bal [b]) ^ "," ^ (string_of_bal f')

  let to_string (a,bfun) = (Address.to_string a) ^ "[" ^ (string_of_bal bfun) ^ "]"
    
end
;;
