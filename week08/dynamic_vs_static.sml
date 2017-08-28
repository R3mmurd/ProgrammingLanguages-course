datatype t = Int of int | String of string

fun f x = if x > 0 then Int(x+x) else String "hi"

fun callF x =
  case (f x) of
      Int i   => Int.toString i
    | String s => s

fun cube x = x * x * x

(* fun f g = (g 7, g true) 

val pair_of_pairs = f (fn x => (x,x)) 

not type-check *)
