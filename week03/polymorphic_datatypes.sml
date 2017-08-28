fun sum_list xs =
  case xs of
      [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
      [] => ys
    | x::xs' => x :: append(xs', ys)

datatype 'a my_option = NONE | SOME of 'a
					   
datatype 'a mylist = Empty | Cons of 'a * 'a mylist
					     
datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
		       | Leaf of 'b

fun sum_tree t =
  case t of
      Leaf i => i
    | Node (i, llink, rlink) => i + sum_tree llink + sum_tree rlink

fun sum_leaves t =
  case t of
      Leaf i => i
    | Node (i, llink, rlink) => sum_leaves llink + sum_leaves rlink
							      
							      
