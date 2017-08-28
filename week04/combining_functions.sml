

fun sqrt_of_abs1 i = Math.sqrt (Real.fromInt (abs i))

fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i

infix !>

fun x !> f = f x

fun sqrt_of_abs3 i = i !> abs !> Real.fromInt !> Math.sqrt
 
