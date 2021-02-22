#use "amm.ml";;

let t0 = Init "t0"
and t1 = Init "t1"
;;

let wA = Wal("A",[(t0,700);(t1,800)])
and wB = Wal("B",[(t0,300)])
;;

let tx0 = Xfer("A",100,t1,"B")
and tx1 = Dep("A",700,t0,700,t1)
and tx2 = SwapL("B",300,t0,200,t1)
and tx3 = SwapR("B",290,t0,210,t1)
and tx4 = Rdm("A",300,AmmTok(t0,t1))
and tx5 = SwapL("B",300,t0,160,t1)
and tx6 = Rdm("A",300,AmmTok(t0,t1))
and tx7 = SwapR("B",30,t0,18,t1)
;;

let sl0 = [wA;wB]
and r0 = [tx0;tx1;tx2;tx3;tx4;tx5;tx6;tx7]
and o0 = fun t -> match t with (Init "t0") -> 5. | (Init "t1") -> 9. | _ -> failwith "price undefined";;

print_trace r0 sl0 invProdFun;;

print_networth r0 sl0 o0 invProdFun;;

let sl7 = step r0 sl0 invProdFun;;
let tx7 = Rdm("A",100,AmmTok(t0,t1));;
let sl8 = step1 tx7 sl7 invProdFun;;
networth "A" sl7 o0;;
networth "A" sl8 o0;;
