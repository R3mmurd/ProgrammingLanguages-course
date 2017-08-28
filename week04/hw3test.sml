(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_0 = only_capitals ["A","B","C"] = ["A","B","C"]

val test1_1 = only_capitals ["Alejandro","becerro","casa"] = ["Alejandro"]

val test1_2 = only_capitals ["Arturo","Barreto","Castillo"] =
	    ["Arturo","Barreto","Castillo"]

val test1_3 = only_capitals ["arbol","buscar","cola"] = []

val test1_4 = only_capitals ["abuso","beso","Carlos"] = ["Carlos"]

val test2_0 = longest_string1 ["A","bc","C"] = "bc"

val test2_1 = longest_string1 [] = ""

val test2_2 = longest_string1 ["Abc", "de", "F"] = "Abc"

val test2_3 = longest_string1 ["Abc", "de", "EFGH"] = "EFGH"

val test2_4 = longest_string1 ["Abcd", "efg", "HIJK"] = "Abcd"

val test3_0 = longest_string2 ["A","bc","C"] = "bc"

val test3_1 = longest_string2 [] = ""

val test3_2 = longest_string2 ["Abc", "de", "F"] = "Abc"

val test3_3 = longest_string2 ["Abc", "de", "EFGH"] = "EFGH"

val test3_4 = longest_string2 ["Abcd", "efg", "HIJK"] = "HIJK"
							    
val test4a_0 = longest_string3 ["A","bc","C"] = "bc"

val test4a_1 = longest_string3 [] = ""

val test4a_2 = longest_string3 ["Abc", "de", "F"] = "Abc"

val test4a_3 = longest_string3 ["Abc", "de", "EFGH"] = "EFGH"

val test4a_4 = longest_string3 ["Abcd", "efg", "HIJK"] = "Abcd"

val test4b_0 = longest_string4 ["A","bc","C"] = "bc"

val test4b_1 = longest_string4 [] = ""

val test4b_2 = longest_string4 ["Abc", "de", "F"] = "Abc"

val test4b_3 = longest_string4 ["Abc", "de", "EFGH"] = "EFGH"

val test4b_4 = longest_string4 ["Abcd", "efg", "HIJK"] = "HIJK"

val test5_0 = longest_capitalized ["A","bc","C"] = "A"

val test5_1 = longest_capitalized ["Alejandro","becerro","casa"] = "Alejandro"

val test5_2 = longest_capitalized ["Arturo","Barreto","Castillo"] = "Castillo"

val test5_3 = longest_capitalized ["arbol","buscar","cola"] = ""

val test5_4 = longest_capitalized ["Andres","beso","Carlos"] = "Andres"

val test6_0 = rev_string "abc" = "cba"

val test6_1 = rev_string "" = ""

val test6_2 = rev_string "b" = "b"

val test6_3 = rev_string "abba" = "abba"

val test6_4 = rev_string "alejandro" = "ordnajela"

val test7_0 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5]
	      = 4

val test7_1 = ((first_answer (fn x => if x = 6 then SOME x else NONE)
			     [1,2,3,4,5];
		false)
	       handle NoAnswer => true)

val test8_0 = all_answers (fn x => if x = 1 then SOME [x] else NONE)
			  [2,3,4,5,6,7] = NONE

val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE)
			  [2,3,4,5,6,7] = SOME [7,6,5,4,3,2]

val test9a_0 = count_wildcards Wildcard = 1

val test9a_1 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3

val test9b_0 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b_1 = count_wild_and_variable_lengths (Variable("abc")) = 3

val test9b_2 = count_wild_and_variable_lengths
		   (TupleP[Variable("abc"), Variable("de")]) = 5

val test9b_3 = count_wild_and_variable_lengths
		   (TupleP[Variable("abc"), Wildcard, Variable("de")]) = 6

val test9c_0 = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("y", Variable("x")) = 0
val test9c_2 = count_some_var ("x", TupleP[Variable("x"),Wildcard,
					   Variable("y")])  = 1
val test9c_3 = count_some_var ("x", TupleP[Variable("x"),Wildcard,
					   Variable("x"),Wildcard,
					   Variable("y")])  = 2

val test10_0 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP[Variable("x"),Wildcard]) = true
val test10_2 = check_pat (TupleP[Variable("x"),Wildcard,Variable("y")]) = true
val test10_3 = check_pat (TupleP[Variable("x"),Wildcard,
				 Variable("y"),Variable("y")]) = false
val test10_4 = check_pat (Wildcard) = true
val test10_5 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"]))
	       = false
val test10_6 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false				     

val test11_0 = match (Const(1), UnitP) = NONE
val test11_1 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE
					   

val test12_0 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]]) ([ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]) = SOME []
