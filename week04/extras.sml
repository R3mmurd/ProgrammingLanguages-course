(* 1 *)
fun compose_opt f g x =
  if (g x) = NONE orelse ((f o g) x) = NONE
  then NONE
  else (f o g) x

(* 2 *)
fun do_while f p x =
  if (p x)
  then (do_while f p (f x))
  else x

(* 3 *)
fun factorial n =
  if n = 0
  then 1
  else let val (r,c)=do_while (fn (r,c) => (r*c,c+1))(fn (r,c) => c <= n)(1,1)
       in r
       end

(* 4 *)
fun fixed_point f x =
  do_while f (fn x => (f x) <> x) x
			
(* 5 *)
fun map2 f (x, y) =
  (f x, f y)

(* 6 *)
fun app_all f g x =
  let fun app_f l =
	case l of
	    []  => []
	  | h::t => f(h)@app_f(t)

  in
      app_f (g x)
  end

(* 7 *)
fun foldr f init xs =
  case xs of
      []     => init
    | x::xs' => f(x, (foldr f init xs'))

(* 8 *)
fun partition f l =
  case l of
      [] => ([], [])
    | h::t => let val (p, n) = partition f t
	      in
		  if (f h) then (h::p, n) else (p, h::n)
	      end

(* 9 *)
fun unfold f x =
  case (f x) of
      NONE => []
    | SOME (a, b) => a::(unfold f b)
		       

(* 14 *)
datatype 'a binNode = Node of {key:'a, llink:'a binNode, rlink:'a binNode}
		    | Leaf

fun map_tree f t =
  case t of
      Leaf                         => Leaf
    | Node {key=k,llink=l,rlink=r} =>
      Node {key=(f k),llink=(map_tree f l),rlink=(map_tree f r)}
	   
fun fold_tree f init t =
  case t of
      Leaf                         => init
    | Node {key=k,llink=l,rlink=r} => let val tmp = f(k, (fold_tree f init l))
				      in
					  f(tmp, (fold_tree f init r))
				      end
	   
fun filter_tree f t =
  case t of
      Leaf                         => Leaf
    | Node {key=k,llink=l,rlink=r} =>
      if (f k)
      then Node {key=k,llink=(filter_tree f l),rlink=(filter_tree f r)}
      else Leaf
