(* type of the LP *)
type t

(* type of the users' debt *)
type dt

val empty : Token.t -> t

val make : Token.t -> int -> dt -> t

val get_balance : t -> int

val get_debt : t -> dt

val debt_of : Address.t -> dt -> int

val set_debt : dt -> t -> t

val update_debt : Address.t -> int -> dt -> dt

val list_of_debt : dt -> (Address.t * int) list

val debt_of_list : (Address.t * int) list -> dt

val accrue_int : float -> dt -> dt

val to_string : t -> string

