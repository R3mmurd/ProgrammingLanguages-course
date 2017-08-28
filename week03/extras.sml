(* Problems 1-4 use these type definitions: *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail
				
(* 1- Write a function pass_or_fail of type {grade : int option, id : 'a} ->
pass_fail that takes a final_grade (or, as the type indicates, 
a more general type) and returns pass if the grade field contains
 SOME i for an i >= 75 (else fail). *)
fun pass_or_fail { id = i, grade=opt } =
  case opt of
      SOME g => if g >= 75 then pass else fail
    | _ => fail

(* 2- Using pass_or_fail as a helper function, write a functio
n has_passed of type {grade : int option, id : 'a} -> bool that
 returns true if and only if the the grade field contains 
SOMEi for an i >=75. *)
fun has_passed fg =
  (pass_or_fail fg) = pass

(* 3- Using has_passed as a helper function, write a function
 number_passed that takes a list of type final_grade
 (or a more general type) and returns how many list elements
 have passing (again, ≥75) grades *)
fun number_passed l =
  case l of
      [] => 0
    | fg::l' => let val n = number_passed l'
		in
		    if has_passed fg then 1 + n else n
		end

(* 4-Write a function number_misgraded of type
 (pass_fail * final_grade) list -> int that indicates
 how many list elements are "mislabeled" where mislabeling
 means a pair (pass,x) where has_passed x is false or (fail,x)
 where has_passed x is true. *)
fun number_misgraded l =
  case l of
      [] => 0
    | (pass, fg)::l' => let val n = number_misgraded l'
			in
			    if has_passed fg then n else 1 + n
			end
    | (fail, fg)::l' => let val n = number_misgraded l'
			in
			    if has_passed fg then 1 + n else n
			end
			    
(* Problems 5-7 use these type definitions: *)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* 5- Write a function tree_height that accepts an 'a tree 
and evaluates to a height of this tree. The height of a tree
 is the length of the longest path to a leaf. Thus the height of a leaf is 0. *)
fun tree_height t =
  case t of
      leaf => 0
    | node {value=v, left=l, right=r} =>
      1 + Int.max(tree_height l, tree_height r)
					      
(* 6- Write a function sum_tree that takes an int tree 
and evaluates to the sum of all values in the nodes. *)
fun sum_tree t =
  case t of
      leaf => 0
    | node {value=v, left=l, right=r} => v + sum_tree l + sum_tree r

								   
(* 7- Write a function gardener of type flag tree -> flag tree
 such that its structure is identical to the original tree except
 all nodes of the input containing prune_me are
 (along with all their descendants) replaced with a leaf.  *)
fun gardener t =
  case t of
      leaf => leaf
    | node {value=v, left=l, right=r} =>
      case v of
	  prune_me => leaf
	| _ => node { value=v, left=gardener l, right=gardener r  }

(* 8-  Re-implement various functions provided in the SML standard 
libraries for lists and options. 
See http://sml-family.org/Basis/list.html and
 http://sml-family.org/Basis/option.html. Good examples
 include last, take, drop, concat, getOpt, and join. *)
exception ListIsEmpty
exception InvalidSize
		    
fun null l =
  l = []

fun length l =
  case l of
      [] => 0
    | h::t => 1 + length t

fun hd l =
  case l of
      [] => raise ListIsEmpty
    | h::t => h

fun tl l =
  case l of
      [] => raise ListIsEmpty
    | h::t => t

fun last l =
  case l of
      [] => raise ListIsEmpty
    | x::[] => x
    | h::t => last t

fun getItem l =
  case l of
      [] => NONE
    | h::t => SOME (h, t)

exception InvalidPosition

fun nth (l, i) =
  if (i < 0)
  then raise InvalidPosition
  else case l of
	   [] => raise InvalidPosition
	 | h::t => if i = 0
		   then h
		   else nth(t, i - 1)
		       
fun take (l, i) =
  if (i < 0)
  then raise InvalidPosition
  else case l of
	   [] => raise InvalidPosition
	 | h::t => if i = 0
		   then []
		   else h::take(t, i - 1)

fun drop (l, i) =
  if (i < 0)
  then raise InvalidPosition
  else case l of
	   [] => raise InvalidPosition
	 | h::t => if i = 0
		   then h::t
		   else drop(t, i - 1)

fun rev l =
  let fun aux(list, acc) =
	case list of
	    [] => acc
	  | h::t => aux(t, h::acc)
  in
      aux(l,[])
  end

fun concat l =
  case l of
      [] => []
    | l1::l' => l1 @ (concat l')

fun revAppend (l1, l2) =
  (rev l1) @ l2
		 
fun map (f, l) =
  case l of
      [] => l
    | h::t => (f h)::map(f, t)

fun find (f, l) =
  case l of
      [] => NONE
    | h::t => if (f h) then SOME h else find(f, t)

fun filter (f, l) =
  case l of
      [] => []
    | h::t => if (f h) then h::filter(f, t) else filter(f, t)

fun partition (f, l) =
  case l of
      [] => ([], [])
    | h::t => let val (p, n) = partition(f, t)
	      in
		  if (f h) then (h::p, n) else (p, h::n)
	      end

fun foldl (f, init, l) =
  case l of
      [] => init
    | h::t => foldl(f, f(h, init), t)
		

fun foldr (f, init, l) =
  case l of
      [] => init
    | h::t => f(h, foldr(f, init, t))

fun exists (f, l) =
  case l of
      [] => false
    | h::t => if (f h) then true else exists(f, t)

fun all (f, l) =
  case l of
      [] => true
    | h::t => if not (f h) then false else all(f, t)

fun tabulate (n, f) =
  let fun countup(from, to) =
	if from >= to
	then []
	else f(from)::countup(from + 1, to)
  in
      if (n < 0) then raise InvalidSize else countup(0, n)
  end

fun collate(f, (l1, l2)) =
  case (l1, l2) of
      ([], []) => EQUAL
    | ([], l) => LESS
    | (l, []) => GREATER
    | (x::tx, y::ty) => if f(x, y) = EQUAL
			then collate(f, (tx, ty))
			else f(x, y)

fun getOpt (opt, a) =
  case opt of
      SOME v => v
    | _ => a

fun isSome opt =
  case opt of
      SOME _ => true
    | _ => false

exception OptionNone

fun valOf opt =
  case opt of
      NONE => raise OptionNone
    | SOME v => v

		    
		    
(* Problems 9-16 use this type definitions: *)
datatype nat = ZERO | SUCC of nat

(* A "natural" number is either zero, or the "successor" of a
 another integer. So for example the number 1 is just SUCC ZERO,
 the number 2 is SUCC (SUCC ZERO), and so on. *)

(* 9-  Write is_positive : nat -> bool, which given a 
"natural number" returns whether that number is positive (i.e. not zero) *)
fun is_positive n =
  case n of
      ZERO => false
    | _ => true
       
(* 10- Write pred : nat -> nat, which given a "natural number" 
returns its predecessor. Since 0 does not have a predecessor 
in the natural numbers, throw an exception Negative 
(will need to define it first). *)
exception Negative
	      
fun pred n =
  case n of
      ZERO => raise Negative
    | SUCC n => n

(* 11- Write nat_to_int : nat -> int, which given a
 "natural number" returns the corresponding int. 
For example, nat_to_int (SUCC (SUCC ZERO)) = 2. *)
fun nat_to_int n =
  case n of
      ZERO => 0
    | SUCC n => 1 + nat_to_int n

(* 12- Write int_to_nat : int -> nat which given an integer 
returns a "natural number" representation for it, or throws a
 Negative exception if the integer was negative. (Again, do not 
use this function in the next few problems.) *)
fun int_to_nat i =
  if i < 0
  then raise Negative
  else if i = 0
  then ZERO
  else SUCC (int_to_nat(i - 1))
      
(* 13-  Write add : nat * nat -> nat to perform addition. *)
fun add (m, n) =
  case (m, n) of
      (ZERO, x) => x
    | (x, ZERO) => x
    | (x, y) => SUCC (add(x, pred y))
		       
			  
(* 14- Write sub : nat * nat -> nat to perform subtraction. *)
fun sub (m, n) =
  case (m, n) of
      (ZERO, x) => raise Negative
    | (x, ZERO) => x
    | (x, y) => sub(pred x, pred y)

(* 15. Write mult : nat * nat -> nat to perform multiplication. *)
fun mul (m, n) =
  case (m, n) of
      (ZERO, _) => ZERO
    | (_, ZERO) => ZERO
    | (x, y) => add(x, mul(x, pred y))
		   
(* 16- Write less_than : nat * nat -> bool to return true when
 the first argument is less than the second. *)
fun less (m, n) =
  case (m, n) of
      (ZERO, SUCC x) => true
    | (_, ZERO) => false
    | (x, y) => less(pred x, pred y)
		   

(* The remaining problems use this datatype, which represents sets of integers *)
datatype intSet = Elems of int list
		| Range of { from : int, to : int }
		| Union of intSet * intSet
		| Intersection of intSet * intSet

(* 17- Write isEmpty : intSet -> bool that determines
 if the set is empty or not. *)
fun isEmpty is =
  case is of
      Elems l => l = []
    | Range {from=l, to=r} => l > r
    | Union (A, B) => isEmpty(A) andalso isEmpty(B)
    | Intersection (A, B) => true  (* TODO *)

(* 18- Write contains: intSet * int -> bool that returns
 whether the set contains a certain element or not. *)
(* helper *)
fun has (l, v) =
  case l of
      [] => false
    | x::tl => if x = v then true else has(tl, v)
					   
fun contains (is, i) =
  case is of
      Elems l => has(l, i)
    | Range{from=l, to=r} => l <= i andalso i <= r
    | Union(A, B) => contains(A, i) orelse contains(B, i)
    | Intersection(A, B) => contains(A, i) andalso contains(B, i)
							   
(* 19- Write toList : intSet -> int list that returns a
 list with the set's elements, without duplicates. *)
fun toList is =
  if isEmpty is
  then []
  else let fun without_dup(l, acc) =
	     case l of
		 [] => acc
	       | x::tl => if has(acc, x)
			  then without_dup(tl, acc)
			  else without_dup(tl, x::acc)
       in
	   case is of
	       Elems l => without_dup(l, [])
	     | Range{from=l, to=r} => let fun countup (from, to) =
					    if (from > to)
					    then []
					    else from::countup(from + 1,to)
				      in
					  countup(l, r)
				      end
	     | Union(A, B) => without_dup((toList A) @ (toList B), [])
	     | _ => [] (* TODO *)
       end
	     
	     
	     
