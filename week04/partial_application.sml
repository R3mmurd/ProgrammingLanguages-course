fun sorted3 x y z = x <= y andalso y <= z

fun fold f acc xs =
  case xs of
      [] => []
    | x::xs' => fold f (f(acc,x)) xs'

val is_nonnegative = sorted3 0 0

val sum = fold (fn (x,y) => x + y) 0

fun is_nonnegative_inferior x = sorted3 0 0 x
				       
fun sum_inferior xs = fold (fn (x,y) => x+y) 0 xs

fun range i j = if i > j then [] else i::range (i+1) j

val countup = range 1

fun countup_inferior x = range 1 x
 
