open Token 

let t0 = Token.initTok "t0"
let t1 = Token.initTok "t1"
let t2 = Token.ammTok(t0,t1)
let t3 = Token.lpTok(t2)

let () = Token.(
  assert (String.compare (to_string t0) "t0" = 0);
  assert (String.compare (to_string t1) "t1" = 0);    
  assert (String.compare (to_string t2) "(t0,t1)" = 0);
  assert (String.compare (to_string t3) "{(t0,t1)}" = 0);        
  assert (compare t0 t1 == -1);
  assert (compare t0 t2 == -1);
  assert (compare t0 t3 == -1);
  assert (compare t2 t3 == 1)    
)
