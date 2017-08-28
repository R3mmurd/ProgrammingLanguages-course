(*
  f : T1 -> T2 [must be a function; all function take 1 arg]
  x : T1

  y : T3
  z : T4

  T1 = T3 * T4 [else pattern match does not type-check]
  T3 = int [abs has type int -> int]
  T4 = int [because we added z to an int]
  So T1 = int * int
  So (abs y) + z = int, to let-expression : int, so body : int
  T2 : int
  So f : int * int -> int 
*)
fun f x =
  let val (y,z) = x in
      (abs y) + z
  end

(*
  sum : T1 -> T2
  xs : T1

  x : T3
  xs' : T3 list [pattern match a T1]

  T1 = T3 list
  T2 = int [because 0 might be returned]
  T3 = int [because x : T3 and we add x to something]
  from T1 = T3 list and T3 = int we know T3 = int list
  from that and T2 = int, we know f : int list -> int
*)
fun sum xs =
  case xs of
      [] => 0
   | x::xs' =>  x + (sum xs')
