fun splitAt s at cmp =
  case s of
      [] => ([],[])
    | h::t =>
      let val (tl,tr) = splitAt t at cmp
      in
	  if cmp(h,at)
	  then (h::tl,tr)
	  else (tl,h::tr)
      end

fun sortedMerge s1 s2 cmp =
  case (s1,s2) of
      ([],_) => s2
    | (_,[]) => s1
    | (hs1::ts1,hs2::ts2) =>
      if cmp(hs1,hs2)
      then hs1::(sortedMerge ts1 s2 cmp)
      else hs2::(sortedMerge s1 ts2 cmp)
    
fun qsort s cmp =
  case s of
      [] => s
    | _::[] => s
    | pivot::s' =>
      let
	  val (pl,pr) = splitAt s' pivot cmp
	  val ls = qsort pl cmp
	  val rs = pivot::(qsort pr cmp)
      in
	  sortedMerge ls rs cmp
      end

fun dftCmp (a,b) = a < b

fun fieldCmp ((p1f,p1s),(p2f,p2s)) = String.compare(p1f,p2f) = LESS

fun subtype a b =
  let fun subt sub super =
	case (sub,super) of
	    ([],[]) => true
	  | (_,[]) => true
	  | ([],_) => false
	  | ((n1,t1)::sub',(n2,t2)::super') =>
	    String.compare(n1,n2) = EQUAL andalso String.compare(t1,t2) = EQUAL
	    andalso (subt sub' super')
  in
      subt (qsort a fieldCmp) (qsort b fieldCmp)
  end
	       
  
