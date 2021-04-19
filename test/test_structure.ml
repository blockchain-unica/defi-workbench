module type AS = sig
  type t
  val zero : t
  val update : t -> t
  val to_int : t -> int
end
  
module type PS = sig
  include AS
  val f : t -> int
end


module type Z = sig
  include AS
  val f : t -> t
end

module A:Z = struct
  type t = int
  let zero = 0
  let update s = s+1
  let to_int a = a
  let f x = x+2
end

module B(P:PS) = struct 
  include A
  let g y = P.f(y) + 1000
end

module C = B(struct
  include A
  let f z = (to_int z)+1
end)

;;

let x0 = C.zero ;;

print_int (C.to_int x0);;
print_newline();;

let x1 = C.update x0;;

print_int (C.to_int x1);;
print_newline();;
print_int (C.g x1);;
print_newline();;
