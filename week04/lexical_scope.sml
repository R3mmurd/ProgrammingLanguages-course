val x = 1 (* x maps to 1 *)

fun f y = x + y (* f maps to a function that adds 1 to its agrument *)

val x = 2 (* x map to 2 *)

val y = 3 (* y maps to 3 *)

val z = f(x + y) (* call f with argument 5, the result is 6 *)
