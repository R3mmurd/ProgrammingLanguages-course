fun fact1 x =
  if x = 0
  then 1
  else x * fact1(x-1)

fun fact x =
  let fun acc(x, a) =
	if x = 0
	then a
	else acc(x-1,x*a)
  in
      acc(x,1)
  end
      
fun sum xs =
  let fun acc(xs, a) =
	case xs of
	    [] => a
	  | x::xs' => acc(xs', x + a)
  in
      acc(xs, 0)
  end


fun rev xs =
  let fun acc(xs, a) =
	case xs of
	    [] => a
	  | x::xs' => acc(xs', x::a)
  in
      acc(xs, [])
  end
