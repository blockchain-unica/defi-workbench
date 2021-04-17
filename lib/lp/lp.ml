open Address
open Token

module type LpType = sig

  type t

  val empty : Token.t -> t
	
  val make : Token.t -> int -> (Address.t * int) list -> t

  val to_string : t -> string

end
    
module Lp : LpType = struct

  type t = Token.t * int * (Address.t * int) list
	
  let make tau n debtfun = (tau,n,debtfun)
      
  let empty tau = (tau,0,[])

  let rec string_of_debt d = match d with
    [] -> ""
  | [(a,v)] -> (string_of_int v) ^ "/" ^ (Address.to_string a)
  | x::d' -> (string_of_debt [x]) ^ "," ^ (string_of_debt d')
    
  let to_string (tau,n,d) =
    "(" ^ (string_of_int n) ^ ":" ^
    (Token.to_string tau) ^ ",{" ^ (string_of_debt d) ^ "})"

end
