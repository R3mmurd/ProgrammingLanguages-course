fun even x = (print "in even "; if x = 0 then true else odd(x-1))
and odd x = (print "in odd "; if x = 0 then false else even(x-1))

val a1 = odd 7
val _ = print "\n"

fun even x = (x mod 2) = 0
val a2 = odd 7
val _ = print "\n"

fun even x = false

val a3 = odd 7
val _ = print "\n"
