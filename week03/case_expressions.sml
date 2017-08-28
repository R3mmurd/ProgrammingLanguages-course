datatype mytype = TwoInts of int * int
		| Str of string
       | Pizza

(* fun f (x : mytype) -> int *)
fun f x =
  case x of
      Pizza => 3
    | Str s => String.size s
    | TwoInts(i1, i2) => i1 + i2
(*    | Pizza => 4; (* redundant case: error *) *)


(* fun g x = case x of Pizza => 3 (* missing cases: warning *) *)
