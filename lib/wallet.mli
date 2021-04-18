open Address

(* type of the wallet *)
type t

(* type of the users' balance *)
type bt

val make : Address.t -> bt -> t

val empty : Address.t -> t

val balance : Token.t -> t -> int

val update : Token.t -> int -> t -> t

val get_address : t -> Address.t

val get_balance : t -> bt

val set_balance : bt -> t -> t

val balance_of_list : (Token.t * int) list -> bt

val list_of_balance : bt -> (Token.t * int) list

val to_string : t -> string
