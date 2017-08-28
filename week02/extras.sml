(* 1- takes a list of numbers and adds them with alternating sign *)
fun alternate (xs : int list) =
  let
      fun alternate_sum (xs : int list, neg : bool) =
	if null xs
	then 0
	else if neg
	then 0 - (hd xs) + alternate_sum(tl xs, false)
	else (hd xs) + alternate_sum(tl xs, true)
  in
      alternate_sum(xs, false)
  end
			
      
(* 2- takes a non-empty list of numbers, and returns a pair (min, max)
 of the minimum and maximum of the numbers in the list. *)
fun min_max (xs : int list) =
  if null (tl xs)
  then (hd xs, hd xs)
  else let
      val tl_min_max = min_max(tl xs)
      val min = if (hd xs) < (#1 tl_min_max) then hd xs else (#1 tl_min_max)
      val max = if (hd xs) > (#2 tl_min_max) then hd xs else (#2 tl_min_max)
  in
      (min, max)
  end

(* 3- takes a list of numbers and returns a list of the partial sums
 of those numbers *)
fun cumsum (xs : int list) =
  let
      fun accum (xs : int list, acc : int) =
	if null (tl xs)
	then [acc + hd xs]
	else (acc + hd xs) :: accum(tl xs, acc + hd xs)
  in
      accum(xs, 0)
  end
      
(* 4- given a string option SOME name returns the string "Hello there, ...!" 
where the dots would be replaced by name *)
fun greeting (str : string option) =
  if isSome str
  then "Hello there, " ^ valOf str ^ "!"
  else "Hello there, you!"

(* 5- given a list of integers and another list of nonnegative integers,
 repeats the integers in the first list according to the numbers
 indicated by the second list *)
fun repeat (xs : int list, ys : int list) =
  let
      fun repeat_val (x : int, y : int) =
	if y = 0
	then []
	else x :: repeat_val(x, y - 1)

  in
      if null xs
      then []
      else repeat_val(hd xs, hd ys) @ repeat(tl xs, tl ys)
	      
  end
		     

(* 6- given two "optional" integers, adds them if they are both present
 (returning SOME of their sum), or returns NONE if at least one of the
 two arguments is NONE *)
fun addOpt (x : int option, y : int option) =
  if (isSome x) andalso (isSome y)
  then SOME (valOf x + valOf y)
  else NONE

(* 7- given a list of "optional" integers, adds those integers that 
are there (i.e. adds all the SOME i). For example: 
addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4. If the list does not 
contain any SOME is in it, i.e. they are all NONE or the list is empty,
 the function should return NONE *)
fun addAllOpt (xs : int option list) =
  if null xs
  then NONE
  else let
      val tl_add = addAllOpt(tl xs)
  in
      if not (isSome (hd xs))
      then tl_add
      else if not (isSome tl_add)
      then hd xs
      else SOME (valOf(hd xs) + valOf(tl_add))
  end

(* 8- given a list of booleans returns true if there is at least 
one of them that is true, otherwise returns false *)
fun any (bs : bool list) =
  if null bs
  then false
  else hd bs orelse any(tl bs)

(* 9- given a list of booleans returns true if all of them true,
 otherwise returns false *)
fun all (bs : bool list) =
  if null bs
  then true
  else hd bs andalso all(tl bs)

(* 10- given two lists of integers creates consecutive pairs, 
and stops when one of the lists is empty *)
fun zip (xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys) :: zip(tl xs, tl ys)

(* 11- challenge. when one list is empty it starts recycling from
 its start until the other list completes *)
fun zipRecycle (xs : int list, ys : int list) =
  let
      fun szip (xs : int list, ys : int list) =
	if null ys
	then ([], xs, [])
	else if null xs
	then ([], [], ys)
	else let
	    val ret = szip(tl xs, tl ys)
	in
	    ((hd xs, hd ys) :: (#1 ret), #2 ret, #3 ret)
	end
		 
      fun lzip (xs : int list, ys : int list) =
	let
	    val res = szip(xs, ys)
	in
	    if null (#2 res)
	    then #1 res
	    else (#1 res) @ lzip(#2 res, ys)
	end
	    
      fun rzip (xs : int list, ys : int list) =
	let
	    val res = szip(xs, ys)
	in
	    if null (#3 res)
	    then #1 res
	    else (#1 res) @ rzip(xs, #3 res)
	end

      val partial_ret = szip(xs, ys)
  in
      if null (#2 partial_ret) andalso null (#3 partial_ret)
      then #1 partial_ret
      else if null (#2 partial_ret)
      then (#1 partial_ret) @ rzip(xs, #3 partial_ret)
      else (#1 partial_ret) @ lzip(#2 partial_ret, ys)
  end
      
(* 12- should return SOME of a list when the original lists have 
the same length, and NONE if they do not *)
fun zipOpt (xs : int list, ys : int list) =
  if null xs andalso null ys
  then SOME []
  else if null xs orelse null ys
  then NONE
  else let
      val tl_zip = zipOpt(tl xs, tl ys)
  in
      if isSome(tl_zip)
      then SOME ((hd xs, hd ys) :: valOf(tl_zip))
      else NONE
  end
			   
(* 13- takes a list of pairs (s, i) and also a string s2 to look up.
 It then goes through the list of pairs looking for the string s2 in
 the first component. If it finds a match with corresponding number i,
 then it returns SOME i. If it does not, it returns NONE *)
fun lookup(pl : (string * int) list, s : string) =
  if null pl
  then NONE
  else if (#1 (hd pl)) = s
  then SOME (#2 (hd pl))
  else lookup(tl pl, s)

(* 14- given a list of integers creates two lists of integers,
 one containing the non-negative entries, the other containing
 the negative entries *)
fun splitup (xs : int list) =
  if null xs
  then ([], [])
  else let
      val tl_s = splitup(tl xs)
  in
      if hd xs < 0
      then ((hd xs) :: (#1 tl_s), #2 tl_s)
      else (#1 tl_s, (hd xs) :: (#2 tl_s))
  end

(* 15-  takes an extra "threshold" parameter, and uses that instead 
of 0 as the separating point for the two resulting lists *)
fun splitAt (xs : int list, at : int) =
  if null xs
  then ([], [])
  else let
      val tl_s = splitAt(tl xs, at)
  in
      if hd xs < at
      then ((hd xs) :: (#1 tl_s), #2 tl_s)
      else (#1 tl_s, (hd xs) :: (#2 tl_s))
  end

(* 16- given a list of integers determines whether the list is
 sorted in increasing order *)
fun isSorted (xs : int list) =
  if null xs orelse null (tl xs)
  then true
  else (hd xs) < (hd (tl xs)) andalso isSorted(tl xs)

(* 17- that given a list of integers determines whether the
 list is sorted in either increasing or decreasing order *)
fun isAnySorted (xs : int list) =
  let
      fun isInverse (xs : int list) =
	if null xs orelse null (tl xs)
	then true
	else (hd xs) > (hd (tl xs)) andalso isInverse(tl xs)
  in
      isSorted(xs) orelse isInverse(xs)
  end

(* 18- takes two lists of integers that are each sorted from
 smallest to largest, and merges them into one sorted list *)
fun sortedMerge (xs : int list, ys : int list) =
  if null xs
  then ys
  else if null ys
  then xs
  else if hd xs < hd ys
  then (hd xs) :: sortedMerge(tl xs, ys)
  else (hd ys) :: sortedMerge(xs, tl ys)

(* 19- Takes the first element out, and uses it as the "threshold" 
for splitAt. It then recursively sorts the two lists produced by
 splitAt. Finally it brings the two lists together. 
(Don't forget that element you took out, it needs to 
get back in at some point) *)
fun qsort (xs : int list) =
  if null xs orelse null (tl xs)
  then xs
  else let
      val pivot = hd xs
      val partition = splitAt(tl xs, pivot)
      val lxs = qsort(#1 partition)
      val rxs = pivot :: qsort(#2 partition)
  in
      sortedMerge(lxs, rxs)
  end

(* 20- takes a list of integers and produces two lists by
 alternating elements between the two lists. *)
fun divide (xs : int list) =
  if null xs
  then ([], [])
  else if null (tl xs)
  then (xs, [])
  else let
      val tl_div = divide(tl (tl xs))
  in
      ((hd xs) :: (#1 tl_div), (hd (tl xs)) :: (#2 tl_div))
  end

(* 21- Given the initial list of integers, splits it in two
 lists using divide, then recursively sorts those two lists,
 then merges them together with sortedMerge *)
fun not_so_quick_sort (xs : int list) =
  if null xs orelse null (tl xs)
  then xs
  else let
      val partition = divide(xs)
      val lxs = not_so_quick_sort(#1 partition)
      val rxs = not_so_quick_sort(#2 partition)
  in
      sortedMerge(lxs, rxs)      
  end

(* 22- given two numbers k and n it attempts to evenly divide 
k into n as many times as possible, and returns a pair (d, n2)
 where d is the number of times while n2 is the resulting n
 after all those divisions *)
fun fullDivide (x : int, y : int) =
  if y mod x <> 0
  then (0, y)
  else let
      val res = fullDivide(x, y div x)
  in
      (1 + (#1 res), #2 res)
  end

(* 23- given a number n returns a list of pairs (d, k) where d is a prime 
number dividing n and k is the number of times it fits. The pairs should 
be in increasing order of prime factor, and the process should stop when 
the divisor considered surpasses the square root of n. If you make sure 
to use the reduced number n2 given by fullDivide for each next step, 
you should not need to test if the divisors are prime: If a number
 divides into n, it must be prime (if it had prime factors, they would 
have been earlier prime factors of n and thus reduced earlier) *)
fun factorize (n : int) =
  let
      fun factor (x : int, d : int) =
	if x = 1
	then []
	else let
	    val res = fullDivide(d, x)
	in
	    if #1 res = 0
	    then factor(#2 res, d + 1)
	    else (d, #1 res) :: factor(#2 res, d + 1)
	end
  in
      factor(n, 2)
  end

(* 24- that given a factorization of a number n as described in the
 previous problem computes back the number n. So this should do the
 opposite of factorize *)
fun multiply (ps : (int * int) list) =
  let
      fun pow (x : int, y : int) =
	if y = 0
	then 1
	else x * pow(x, y - 1)
  in
      if null ps
      then 1
      else pow(#1 (hd ps), #2 (hd ps)) * multiply(tl ps)
  end

(* 25- Challenge hard. given a factorization list result 
from factorize creates a list all of possible products produced 
from using some or all of those prime factors no more than the 
number of times they are available. This should end up being a 
list of all the divisors of the number n that gave rise to the list *)
fun all_products (ps : (int * int) list) =
  let
      fun divisors (n : int, d : int) =
	if n = d
	then [n]
	else let
	    val ds = divisors(n, d + 1)
	in
	    if n mod d = 0
	    then d :: ds
	    else ds
	end
  in
      divisors(multiply(ps), 1)
  end
