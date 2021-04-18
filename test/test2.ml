let a = Address.addr "A"
let b = Address.addr "B"
let t0 = Token.init "t0"
let t1 = Token.init "t1"
;;

print_endline "========== s0 ==========";;

let s0 = State.(
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

let s1 = State.(s0 |> accrue_int |> id_print |> id_info );;


print_endline "========== s1' ==========";;

let s1' = State.(s0 |> liq b a 1 t0 t0 |> accrue_int |> id_print |> id_info );;

