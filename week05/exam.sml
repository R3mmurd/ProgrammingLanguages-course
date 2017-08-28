fun mystery f = fn xs =>
  let
      fun g xs =
        case xs of
            [] => NONE
          | x::xs' => if f x then SOME x else g xs'
  in
      case xs of
          [] => NONE
        | x::xs' => if f x then g xs' else mystery f xs'
  end

fun null xs = ((fn z => false) (hd xs)) handle List.Empty => true

(*
signature COUNTER11 =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end

signature COUNTER12 =
sig
    type t = int
    val newCounter : int -> t
    val first_larger : t * t -> bool
end

signature COUNTER13 =
sig
    type t
    val newCounter : int -> int
    val increment : t -> t
    val first_larger : t * t -> bool
end

signature COUNTER14 =
sig
    type t
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end

signature COUNTER15 =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t
end
    
structure NoNegativeCounter :> COUNTER15 =
struct

exception InvariantViolated
	      
type t = int
	     
fun newCounter i = if i <= 0 then 1 else i
					     
fun increment i = i + 1
			  
fun first_larger (i1,i2) =
  if i1 <= 0 orelse i2 <= 0
  then raise InvariantViolated
  else (i1 - i2) > 0
		       
end
*)
