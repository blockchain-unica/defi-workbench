module Address = struct

  type t = Addr of string

  let addr s = Addr s
    
  let compare (Addr s1) (Addr s2) = String.compare s1 s2

  let to_string (Addr a) = a
    
end
;;
