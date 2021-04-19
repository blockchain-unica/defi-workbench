open State

let a = Address.addr "A"
let b = Address.addr "B"
let t0 = Token.init "t0"
let t1 = Token.init "t1"

module S = State(
  struct
    let coll_min = 1.5
    let r_liq = 1.1
    let intr _ = 0.14
  end)

let s = S.(
  empty
  |> px t0 1.
  |> px t1 1.
  |> id_print
  |> add_wallet a [(t0,100);(t1,150)]
  |> id_print
  |> add_wallet b [(t0,200)]
  |> id_print
  |> xfer a b 10 t0
  |> id_print
  |> xfer a b 10 t1
  |> id_print
  |> dep a 50 t0
  |> id_print
  |> dep b 10 t0
  |> id_print
  |> bor a 15 t0
  |> id_print
  |> accrue_int
  |> id_print
  |> dep a 40 t1
  |> id_print
  |> dep b 100 t0
  |> id_print
  |> bor b 40 t1
  |> id_print
  |> bor b 30 t0
  |> id_print
  |> accrue_int
  |> id_print
  |> accrue_int
  |> id_print
  |> accrue_int
  |> id_print
  |> liq a b 10 t1 t0
  |> id_print
  |> rep b 10 t0
  |> id_print
  |> liq a b 19 t1 t0
  |> id_print
  |> rdm a 77 t0
  |> id_print
  |> id_info
)
;;
