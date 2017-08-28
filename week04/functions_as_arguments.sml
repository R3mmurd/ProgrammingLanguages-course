fun increment_n_times_lame (n,x) =
  if n=0
  then x
  else 1 + increment_n_times_lame(n-1,x)

fun double_n_times_lame (n,x) =
  if n=0
  then x
  else 2 * double_n_times_lame(n-1,x)

fun nth_tail_lame (n, xs) =
  if n=0
  then xs
  else tl (nth_tail_lame(n-1,xs))

fun n_times (f, n, x) =
  if n=0
  then x
  else f(n_times(f, n-1, x))
	
fun double x = 2 * x
fun triple x = 3 * x
fun incr x = x + 1
fun tail xs = tl xs

val x1 = n_times(double,4,7)
val x2 = n_times(incr,4,7)
val x3 = n_times(tail, 2, [4,8,12,16])
val x4 = n_times(triple,4,7)
