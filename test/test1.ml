open Address
open State
 
let a = Address.addr "A"
let b = Address.addr "B"
let t0 = Token.init "t0"
let t1 = Token.init "t1"

let s = State.(
  empty
  |> id_print
  |> add_wallet a [(t0,100)]
  |> id_print
  |> add_wallet b [(t1,50)]
  |> id_print
  |> dep a 50 t0
  |> id_print
  |> dep b 50 t1
  |> id_print
  |> bor b 30 t0
  |> id_print
  |> accrue_int
  |> id_print
  |> rep a 5 t0
  |> id_print
  |> px t0 1.3
  |> id_print
  |> liq a b 10 t0 t1
  |> id_print
  |> rdm a 10 t0
  |> id_print
)
;;
