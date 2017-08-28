(* first example *)
val x = 1

fun f1 y =
  let
      val x = y + 1
  in
      fn z => x + y + z (* here x = y + 1 *)
  end

val x = 3 (* irrelevant *)

val g = f1 4 (* return a function that adds 9 to its argument *)

val y = 5 (* irrelevant *)

val z = g 6 (* get 15 *)

(* second example *)
fun f2 g =
  let
      val x = 3 (* irrelevant *)
  in
      g 2
  end

val x = 4 (* maps x to 4 *)

fun h y = x + y (* this adds 4 to y *)

val z = f2 h (* z is 6 *)
