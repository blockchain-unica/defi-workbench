open Address
open State

let print_info al s = 
  List.iter 
    (fun x -> 
      print_string ( (Address.to_string x) ^ " net worth: ");
      print_endline (string_of_float (State.networth x s));
      print_string ( (Address.to_string x) ^ " collateralization: ");
      print_endline (match State.coll x s with Val v -> (string_of_float v) | _ -> "Infty"))
    al
;;

 
let a = Address.addr "A"
let b = Address.addr "B"
let t0 = Token.init "t0"
let t1 = Token.init "t1"
;;

print_endline "===== s0 =====";;

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
 )
;;

print_info [a;b] s0;;

print_endline "===== s1 =====";;

let s1 = State.(s0 |> accrue_int |> id_print);;

print_info [a;b] s1;;


print_endline "===== s1' =====";;

let s1' = State.(s0 |> liq b a 1 t0 t0 |> accrue_int |> id_print);;

print_info [a;b] s1';;
