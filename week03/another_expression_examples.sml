datatype exp = Constant of int
	     | Negate   of exp
	     | Add      of exp * exp
	     | Multiply of exp * exp


fun max_constant e =
  case e of
      Constant i      => i
    | Negate e1       => max_constant e1
    | Add(e1,e2)      => Int.max (max_constant e1, max_constant e2)
    | Multiply(e1,e2) => Int.max (max_constant e1, max_constant e2)

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
