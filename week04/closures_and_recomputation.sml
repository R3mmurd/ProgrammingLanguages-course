fun filter (f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x then x::filter(f, xs') else filter(f, xs')

fun allShorterThan1 (xs, s) =
  filter ((fn x => String.size x < (print "!"; String.size s)), xs)

fun allShorterThan2 (xs, s) =
  let
      val sz = (print "!"; String.size s)
  in
      filter ((fn x => String.size x < sz), xs)
  end
				 
val xs = ["aaaaaa", "aaaaaaa", "aaaa", "a", "aa", "aaa", "aaaaa"]

val _ = print "\nwithAllShorterThan1: "

val ys = allShorterThan1 (xs, "aaaa")

val _ = print "\nwithAllShorterThan2: "

val zs = allShorterThan2 (xs, "aaaa")

val _ = print "\n"
