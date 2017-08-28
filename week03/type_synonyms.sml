datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = (suit * rank)

type name_record = { student_num : int option,
		     first       : string,
		     middle      : string option,
		     last        : string }

fun is_Queen_of_Spades (c : card) =
  #1 c = Spade andalso #2 c = Queen
				  
fun is_Queen_of_Spades2 c =
  case c of
      (Spade, Queen) => true
    | _ => false

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)
	     
val t1 = is_Queen_of_Spades c1
val t2 = is_Queen_of_Spades c2
val t3 = is_Queen_of_Spades c3
	     
val t4 = is_Queen_of_Spades2 c1
val t5 = is_Queen_of_Spades2 c2
val t6 = is_Queen_of_Spades2 c3

