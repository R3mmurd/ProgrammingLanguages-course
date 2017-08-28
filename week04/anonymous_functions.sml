fun n_times (f, n, x) =
  if n=0
  then x
  else f(n_times(f,n-1,x))

fun triple_n_times1 (n,x) =
  let fun triple y = 3*y
  in n_times(triple,n,x)
  end

fun triple_n_times2 (n,x) =
  n_times(let fun triple y = 3*y in triple end,n,x)

fun triple_n_times3 (n,x) =
  n_times((fn y => 3*y),n,x)

	 
val x1 = triple_n_times1(4,7)
val x2 = triple_n_times2(4,7)
val x3 = triple_n_times3(4,7)
	
