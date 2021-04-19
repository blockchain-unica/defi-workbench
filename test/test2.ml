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

;;

print_endline "========== s0 ==========";;

let s0 = S.(
  empty
  |> px t0 1.
  |> px t1 1.
  |> add_wallet a [(t0,200)]
  |> add_wallet b [(t0,100)]
  |> dep a 150 t0
  |> dep b 50 t0
  |> bor a 100 t0
  |> accrue_int
  |> id_print
  |> id_info
 )
;;


print_endline "========== s1 ==========";;

let s1 = S.(s0 |> accrue_int |> id_print |> id_info );;


print_endline "========== s1' ==========";;

let s1' = S.(s0 |> liq b a 1 t0 t0 |> accrue_int |> id_print |> id_info );;

