open Address
open Token

module type LpType = sig

  (* type of the LP *)
  type t
  (* type of the users' debt *)
  type dt
  
  val empty : Token.t -> t
	
  val make : Token.t -> int -> dt -> t

  val get_balance : t -> int
      
  val get_debt : t -> dt

  val set_debt : dt -> t -> t

  val list_of_debt : dt -> (Address.t * int) list
      
  val debt_of_list : (Address.t * int) list -> dt
      
  val to_string : t -> string

end
    
module Lp : LpType = struct

  type dt = (Address.t * int) list  
  type t = Token.t * int * dt
	
  let make tau n debt = (tau,n,debt)

  let empty tau = (tau,0,[])

  let get_balance (_,n,_) = n
      
  let get_debt (_,_,d) = d
     
  let set_debt d (t,n,_) = (t,n,d)

  let debt_of_list l = l

  let list_of_debt l = l

  let rec string_of_debt d = match d with
    [] -> ""
  | [(a,v)] -> (string_of_int v) ^ "/" ^ (Address.to_string a)
  | x::d' -> (string_of_debt [x]) ^ "," ^ (string_of_debt d')
    
  let to_string (tau,n,d) =
    "(" ^ (string_of_int n) ^ ":" ^
    (Token.to_string tau) ^ ",{" ^ (string_of_debt d) ^ "})"

end
