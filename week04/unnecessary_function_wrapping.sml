fun n_times (f, n, x) =
  if n=0
  then x
  else f(n_times(f,n-1,x))

(* poor style *)
fun nth_tail_poor (n,xs) = n_times((fn y => tl y),n,xs)

(* good style *)
fun nth_tail (n,xs) = n_times(tl,n,xs)

val x1 = nth_tail_poor(2,[4,8,12,16])
val x2 = nth_tail(2,[4,8,12,16])
