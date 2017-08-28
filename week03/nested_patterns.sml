exception ListLengthMismatch

fun zip3 triple_list =
  case triple_list of
      ([],[],[]) => []
    | (x::xs, y::ys, z::zs) => (x, y, z) :: zip3 (xs, ys, zs)
    | _ => raise ListLengthMismatch

fun unzip3 list =
  case list of
      [] => ([],[],[])
    | (x,y,z)::l => let	val (xs, ys, zs) = unzip3 l
		    in
			(x::xs, y::ys, z::zs)
		    end
			
fun nondecreasing xs =
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck
			    andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign (x, y) =
  let fun sign x = if x = 0 then Z else if x < 0 then N else P
  in
      case (sign x, sign y) of
	  (Z,_) => Z
	| (_,Z) => Z
	| (P,P) => P
	| (N,N) => P
	| _ => N
  end

fun len xs =
  case xs of
      [] => 0
   | _::xs' => 1 + len xs'
