type t

type collType = Infty | Val of float

val empty : t

(** [get_wallet a s] is the wallet of address [a] in state [s] 
    Raises: [Not_found] if the wallet is not present *)
val get_wallet : Address.t -> t -> Wallet.t

(** [get_lp t s] is the LP of token [t] in state [s] 
    Raises: [Not_found] if the LP is not present *)
val get_lp : Token.t -> t -> Lp.t

(** [get_price t s] is the price of the token [t] in [s].
    The price of all tokens is set to 0. in the initial state *)
val get_price : Token.t -> t -> float


(** [add_wallet a bal s] is the state [s] composed with the wallet [(a,bal)].
    Raises: [Failure] if a wallet of [a] is already present in [s] *)
val add_wallet : Address.t -> (Token.t * int) list -> t -> t

(** [tok s] is the set of all tokens in state [s] *)
val tok : t -> Token.t list

(** [addr s] is the set of all addresses in state [s] *)
val addr : t -> Address.t list


(** [supply tau s] is the supply of token [tau] in state [s].
    The supply is 0 if the token does not occur in [s] *)
val supply : Token.t -> t -> int

(** [er tau s] is the exchange rate of a non-minted token in state [s]. 
    Raises: [invalid_arg] is [tau] is minted by a LP *)
val er : Token.t -> t -> float

(** value of free (non-collateralized) tokens *)
val val_free : Address.t -> t -> float

(** value of collateralized tokens *)
val val_collateralized : Address.t -> t -> float

(** value of non-minted tokens *)
val val_debt : Address.t -> t -> float

(** [networth a s] is the net worth of address [a] in state [s] *)
val networth : Address.t -> t -> float

(** collateralization of a user in a state *)
val coll : Address.t -> t -> collType


(**************************************************)
(*              Lending pool actions              *)
(**************************************************)

val xfer : Address.t -> Address.t -> int -> Token.t -> t -> t

val dep : Address.t -> int -> Token.t -> t -> t

val bor : Address.t -> int -> Token.t -> t -> t

val accrue_int : t -> t

val rep : Address.t -> int -> Token.t -> t -> t

val rdm : Address.t -> int -> Token.t -> t -> t

val liq : Address.t -> Address.t -> int -> Token.t -> Token.t -> t -> t

val px : Token.t -> float -> t -> t



(**************************************************)
(*                 Pretty-printing                *)
(**************************************************)
 
val to_string : t -> string

val id_print : t -> t

val id_info : t -> t

