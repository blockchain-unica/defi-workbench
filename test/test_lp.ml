let a = Address.addr "A"
let b = Address.addr "B"

let t0 = Token.init "t0"
let t1 = Token.init "t1"
let t2 = Token.mintAMM(t0,t1);;

let lp0 = Lp.empty t0;;
print_endline (Lp.to_string lp0)

let lp1 = Lp.make t0 100 (Lp.debt_of_list [(a,5);(b,10)]);;
print_endline (Lp.to_string lp1)

