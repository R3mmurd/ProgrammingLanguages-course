(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option (str, strs) =
  let fun aux (s, ss) =
	case ss of
	    [] => (false, [])
	  | s'::ss' => if same_string(s, s')
		       then let val (r, l) = aux(s, ss')
			    in
				(true, l)
			    end
		       else let val (r, l) = aux(s, ss')
			    in
				(r orelse false, s'::l)
			    end

      val (r, lst) = aux (str, strs)
  in
      if r then SOME lst else NONE
  end

  
(* b *)
fun get_substitutions1 (strss, str) =
  case strss of
      [] => []
    | strs::strss' => let
	val subs = get_substitutions1(strss', str)
	val res = all_except_option(str, strs)
    in
	case res of
	    NONE => subs
	  | SOME l => l @ subs
    end

(* c *)
fun get_substitutions2 (strss, str) =
  let
      fun aux1 (ss, acc) =
	case ss of
	    [] => acc
	  | s'::ss' => aux1(ss', s'::acc)
			  
      fun aux2 (sss, acc) =
	case sss of
	    [] => acc
	  | ss::sss' => let val r = all_except_option(str, ss)
			in
			    case r of
				NONE => aux2(sss', acc)
			      | SOME l => let val new_acc = aux1(l, acc)
					  in
					      aux2(sss', new_acc)
					  end
			end
  in
      aux2 (strss, [])
  end
      
(* d *)
fun similar_names (strss, {first=f, middle=m, last=l}) =
  let fun build_names (subs, acc) =
	case subs of
	    [] => acc
	  | f'::subs' =>
	    build_names(subs', {first=f', middle=m, last=l}::acc)

      val res = build_names(get_substitutions2(strss, f), [])	
  in
      {first=f, middle=m, last=l}::res
  end
	    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color c =
  case c of
      (Clubs,_) => Black
    | (Spades,_) => Black
    |  _ => Red

(* b *)
fun card_value c =
  case c of
      (_,Num n) => n
    | (_,Ace) => 11
    | _ => 10

(* c *)
fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | c'::cs' => if c' = c
		 then cs'
		 else c'::remove_card(cs', c, e)
				     
(* d *)
fun all_same_color cs =
  case cs of
      [] => true
    | _::[] => true
    | c1::c2::cs' => (card_color c1 = card_color c2) andalso
		     all_same_color(c2::cs')

(* e *)
fun sum_cards cs =
  let fun aux (cards, acc) =
	case cards of
	    [] => acc
	  | c::cards' => aux(cards', card_value c + acc)
  in
      aux(cs, 0)
  end

(* f *)
fun score (hs, goal) =
  let val sum = sum_cards hs
      val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
  in
      if all_same_color hs
      then pre_score div 2
      else pre_score
  end
		   
(* g *)
fun officiate (cs, ms, goal) =
  let fun run(cl, ml, hl, g) =
	case ml of
	    [] => score(hl, g)
	  | (Discard c)::ml' => run(cl, ml', remove_card(hl, c, IllegalMove), g)
	  | Draw::ml' => case cl of
			     [] => score(hl, g)
			   | c'::cl' => if sum_cards(c'::hl) > g
					then score(c'::hl, g)
					else run(cl', ml', c'::hl, g)
  in
      run(cs, ms, [], goal)
  end

(* solutions for problem 3 *)
(* a *)
(* helper sum_cards_challenge *)
fun sum_cards_challenge (cs, ace_value) =
  let fun aux (cards, acc) =
	case cards of
	    [] => acc
	  | c::cards' => case c of
			     (_,Ace) => aux(cards', ace_value + acc)
			    |_  =>  aux(cards', card_value c + acc)
  in
      aux(cs, 0)
  end

fun score_challenge_helper (hs, goal, ace_value) =
  let val sum = sum_cards_challenge(hs, ace_value)
      val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
  in
      if all_same_color hs
      then pre_score div 2
      else pre_score
  end  
      
fun score_challenge (hs, goal) =
  Int.min(score_challenge_helper(hs, goal, 1),
	  score_challenge_helper(hs, goal, 11))

(* This function is not working yet for some cases :( *)
fun officiate_challenge (cs, ms, goal) =
  let fun run(cl, ml, hl, g, ace_value) =
	case ml of
	    [] => score_challenge_helper(hl, g, ace_value)
	  | (Discard c)::ml' =>
	    run(cl, ml', remove_card(hl, c, IllegalMove), g, ace_value)
	  | Draw::ml' => case cl of
			     [] => score_challenge_helper(hl, g, ace_value)
			   | c'::cl' =>
			     if sum_cards_challenge(c'::hl, ace_value) > g
			     then score_challenge_helper(c'::hl, g, ace_value)
			     else run(cl', ml', c'::hl, g, ace_value)
  in
      Int.min(run(cs, ms, [], goal, 1), run(cs, ms, [], goal, 11))
  end

(* b *)
fun careful_player (cs, goal) =
  let fun play (cl, hl, g) =
	if score(hl, g) = 0
	then []
	else
	    case cl of
		[] => [Draw]
	      | c::cl' => if g > (sum_cards hl) + 10
			  then Draw::play(cl', c::hl, g)
			  else if sum_cards(c::hl) <= g
			  then Draw::play(cl', c::hl, g)
			  else case hl of
				   [] => [Draw]
				 | c'::hl' => (Discard c')::play(cl, hl', g)
			     
  in
      play (cs, [], goal)
  end
