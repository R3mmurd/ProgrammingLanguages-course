fun sum_triple (x,y,z) =
  x + y + z

fun full_name {first = x, second = y, third = z} =
  x ^ " " ^ y ^ " " ^ z

fun rotate_left (x,y,z) = (y,z,x)

fun rotate_right t = rotate_left(rotate_left t)
