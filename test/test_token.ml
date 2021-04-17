open Token

let t0 = init "t0"
let t1 = init "t1"
let t2 = mintAMM(t0,t1)
let t3 = mintLP(t2)

let () =
  print_endline (to_string t0);
  print_endline (to_string t1);
  print_endline (to_string t2);
  print_endline (to_string t3);

