open Address
open State
 
let a = Address.addr "A"
let b = Address.addr "B"
let t0 = Token.init "t0"
let t1 = Token.init "t1"

let s = State.(
  empty
  |> id_print
  |> add_wallet a [(t0,100);(t1,50)]
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
)
;;

print_endline "Collateralization: ";
print_string ( (Address.to_string a) ^ ": ");
print_endline (match State.coll a s with Val v -> (string_of_float v) | _ -> "Infty");;
print_string ( (Address.to_string b) ^ ": ");
print_endline (match State.coll b s with Val v -> (string_of_float v) | _ -> "Infty");;
