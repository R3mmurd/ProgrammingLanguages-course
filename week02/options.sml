fun opt_max (xs : int list) =
  if null xs
  then NONE
  else if null (tl xs)
  then SOME (hd xs)
  else let
      val tl_max = opt_max(tl xs)
  in
      if hd xs > valOf tl_max
      then SOME (hd xs)
      else tl_max
  end
			      
fun best_max (xs : int list) =
  if null xs
  then NONE
  else let
      fun max_nonempty (xs : int list) =
	if null (tl xs)
	then hd xs
	else let
	    val tl_max = max_nonempty(tl xs)
	in
	    if (hd xs > tl_max)
	    then hd xs
	    else tl_max
	end
  in
      SOME (max_nonempty xs)
  end
