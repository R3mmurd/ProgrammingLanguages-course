(* old way to get the effect of multiple arguments *)
fun sorted3_tupled (x,y,z) = x <= y andalso y <= z

val t1 = sorted3_tupled (7,9,11)

(* new way: currying *)
val sorted3 = fn x => fn y => fn z => x <= y andalso y <= z

val t2 = ((sorted3 7) 9) 11

val t3 = sorted3 7 9 11
		 
fun sorted3_nicer x y z = x <= y andalso y <= z

val t4 = sorted3_nicer 7 9 11

val t5 = ((sorted3_nicer 7) 9) 11
