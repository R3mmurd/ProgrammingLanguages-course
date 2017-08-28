datatype exp =
	 Int of int
	 | Negate of exp
	 | Add of exp * exp
	 | Mult of exp * exp

exception BadResult of string
			   
fun add_values (v1,v2) =
  case (v1,v2) of
      (Int i, Int j) => Int(i+j)
    | _ => raise BadResult "non-ints in addition"

fun multiply_values (v1,v2) =
  case (v1,v2) of
      (Int i, Int j) => Int(i*j)
    | _ => raise BadResult "non-ints in multiplication"
		 

fun eval e =
  case e of
      Int _ => e
    | Negate e1 =>  (case eval e1 of
			 Int i => Int(~i)
		       | _ => raise BadResult "non-int in negation")
    | Add(e1,e2) => add_values(eval e1, eval e2)
    | Mult(e1,e2) => multiply_values(eval e1, eval e2)

fun toString e =
  case e of
      Int i => Int.toString i
    | Negate e1 => "-(" ^ (toString e1) ^ ")"
    | Add(e1,e2) => "(" ^ (toString e1) ^ "+" ^ (toString e2) ^ ")"
    | Mult(e1,e2) => "(" ^ (toString e1) ^ "*" ^ (toString e2) ^ ")"

fun hasZero e =
  case e of
      Int i => i = 0
    | Negate e1 => hasZero e1
    | Add(e1,e2) => (hasZero e1) orelse (hasZero e2)
    | Mult(e1,e2) => (hasZero e1) orelse (hasZero e2)
				    
fun noNegConstants e =
  case e of
      Int i => if i < 0 then Negate (Int(~i)) else e
    | Negate e1 => Negate(noNegConstants e1)
    | Add(e1,e2) => Add(noNegConstants e1, noNegConstants e2)
    | Mult(e1,e2) => Mult(noNegConstants e1, noNegConstants e2)
