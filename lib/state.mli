open Address

type t

type collType = Infty | Val of float

val empty : t

val get_wallet : Address.t -> t -> Wallet.t

val get_lp : Token.t -> t -> Lp.t

val get_price : Token.t -> t -> float

val add_wallet : Address.t -> (Token.t * int) list -> t -> t

    (** [tok s] is the set of all tokens in state [s] *)
val tok : t -> Token.t list

    (** [supply tau s] is the supply of token [tau] in state [s] *)
val supply : Token.t -> t -> int

    (** [er tau s] is the exchange rate of a non-minted token in state [s] *)
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

val xfer : Address.t -> Address.t -> int -> Token.t -> t -> t

val dep : Address.t -> int -> Token.t -> t -> t

val bor : Address.t -> int -> Token.t -> t -> t

val accrue_int : t -> t

val rep : Address.t -> int -> Token.t -> t -> t

val rdm : Address.t -> int -> Token.t -> t -> t

val liq : Address.t -> Address.t -> int -> Token.t -> Token.t -> t -> t

val px : Token.t -> float -> t -> t

val to_string : t -> string

val id_print : t -> t

