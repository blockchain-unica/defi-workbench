open Token 

let t0 = Token.init "t0"
let t1 = Token.init "t1"
let t2 = Token.mintAMM(t0,t1)
let t3 = Token.mintLP(t2)

let () = Token.(
  print_endline (to_string t0);
  print_endline (to_string t1);
  print_endline (to_string t2);
  print_endline (to_string t3);  
)
