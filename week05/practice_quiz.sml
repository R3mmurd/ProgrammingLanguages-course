signature DIGIT11 = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end
    
signature DIGIT12 = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
end
    
signature DIGIT13 = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val test : digit -> unit
end

signature DIGIT14 = 
sig
    type digit
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end

signature DIGIT15 = 
sig
    type digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end
    
structure Digit :> DIGIT15 =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end
