(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals ss =
  List.filter (fn s => (Char.isUpper o String.sub) (s, 0)) ss

(* 2 *)
fun longest_string1 ss =
  List.foldl (fn (s, acc) =>
		 if (String.size s) > (String.size acc)
		 then s else acc) "" ss

(* 3 *)
fun longest_string2 ss =
  List.foldl (fn (s, acc) =>
		 if (String.size s) >= (String.size acc)
		 then s else acc) "" ss

(* 4 *)
fun longest_string_helper f ss =
  List.foldl (fn (s, acc) =>
		 if f(String.size s, String.size acc)
		 then s else acc) "" ss

val longest_string3 = longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn (s1, s2) => s1 >= s2)

(* 5 *)
val longest_capitalized = (fn ss => (longest_string1 o only_capitals) ss)

(* 6 *)
fun rev_string s =
  (String.implode o List.rev o String.explode) s


(* 7 *)
fun first_answer f l =
  case l of
      []   => raise NoAnswer
    | h::t => case f(h) of
		  SOME v => v
		| NONE   => first_answer f t
					       
					       
(* 8 *)
fun all_answers f l =
  let fun aux xs acc =
	case xs of
	    []     => acc
	  | x::xs' => case f(x) of
			  NONE   => raise NoAnswer
			| SOME v => aux xs' v@acc
  in
      ((SOME (aux l [])) handle NoAnswer => NONE)
  end

(* 9 *)
fun count_wildcards p =
  g (fn () => 1) (fn s => 0) p

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn s => String.size s) p
  
fun count_some_var (s, p) =
  g (fn () => 0) (fn s' => if s = s' then 1 else 0) p


(* 10 *)
fun check_pat p =
  let fun get_strings pattern =
	case pattern of
	    Variable s => [s]
	  | TupleP ps  => List.foldl (fn (i,acc) => get_strings(i)@acc) [] ps
	  | ConstructorP (_,p) => get_strings p
	  | _          => []

      fun has_unique_vars ss =
	case ss of
	    []     => true
	  | [_]    => true
	  | s::ss' => not (List.exists (fn s' => s = s') ss') andalso
		      has_unique_vars ss'
  in
      (has_unique_vars o get_strings) p
  end


(* 11 *)
fun match (va, pa) =
  let fun build_result (v,p) =
	case (v,p) of
	    (_,Wildcard)                             => (true, [])
	  | (v',Variable s)                          => (true,[(s,v')])
	  | (Unit,UnitP)                             => (true,[])
	  | (Const c1,ConstP c2)                     => if c1 = c2
							then (true,[])
							else (false,[])
	  | (Tuple vs,TupleP ps)                     =>
	    let	val matches = all_answers (fn vp =>
					      let val (r,s) = build_result(vp)
					      in if r then SOME s else NONE
					      end)
					  
	    in	
		((case (matches o ListPair.zipEq) (vs,ps)  of
		      SOME s => (true,s) 
		    | _      => (false,[]))
		 handle UnequalLengths => (false,[]))
	    end
		
	    
	  | (Constructor(s1,v1),ConstructorP(s2,p1)) =>
	    let val (r,svs) = build_result(v1,p1)
	    in
		if r andalso s1 = s2
		then (true,svs)
		else (false,[])
	    end
	    | (_,_)                                  => (false,[])

      val (result,set) = build_result(va,pa)
  in
      if result then SOME set else NONE
  end
      
(* 12 *)
fun first_match v ps =
  ((SOME (first_answer (fn p => match(v,p)) ps))
   handle NoAnswer => NONE)
