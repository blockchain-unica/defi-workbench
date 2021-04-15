(*
module type StateSig = sig

  type state

  (* The empty state *)
  val empty : state

  (* Whether the state is empty*)
  val is_empty : state -> bool

  (* [addWallet x s] is the stack [s] with [x] pushed on the top *)
  val addWallet : wallet -> state -> state

  (* Raises Failure if [s] is empty. *)
  val getWallet : user -> state -> wallet

  (* [pop s] pops and discards the top element of [s]. 
     Raises Failure if [s] is empty. *)
  val pop : state -> state

  val to_string : state -> string
end
*)
