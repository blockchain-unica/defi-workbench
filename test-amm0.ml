#use "amm.ml";;

(* initial tokens *)
let t0 = Init "t0"
and t1 = Init "t1"
and t2 = Init "t2"
;;

(* wallets *)
let wA = Wal("A",[(t0,5);(t1,11)])
and wB = Wal("B",[(t0,15)])
;;

(* transactions *)
let tx0 = Xfer("A",6,t1,"B")
and tx1 = Dep("A",5,t0,5,t1)
and tx2 = Dep("B",5,t0,5,t1)    
and tx3 = SwapL("B",10,t0,4,t1)
and tx4 = Rdm("B",5,AmmTok(t0,t1))
and tx5 = SwapR("B",7,t0,7,t1)
;;

(* sl0 = initial state *)
let sl0 = [wA;wB]
and r0 = [tx0;tx1;tx2;tx3;tx4;tx5]
and o0 = fun t -> match t with (Init "t0") -> 5. | (Init "t1") -> 9. | _ -> failwith "price undefined";;

print_trace r0 c0 invProdFun;;

print_networth r0 c0 o0 invProdFun;;
