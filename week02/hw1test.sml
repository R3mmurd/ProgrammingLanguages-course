(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true
val test_is_older_1 = is_older((2014, 12, 23), (2016, 10, 28)) = true
val test_is_older_2 = is_older((2017, 12, 23), (2016, 10, 28)) = false
val test_is_older_3 = is_older((2016, 01, 30), (2016, 10, 28)) = true
val test_is_older_4 = is_older((2016, 12, 23), (2016, 10, 28)) = false
val test_is_older_5 = is_older((2016, 10, 15), (2016, 10, 28)) = true
val test_is_older_6 = is_older((2016, 10, 30), (2016, 10, 28)) = false
val test_is_older_7 = is_older((2016, 10, 28), (2016, 10, 28)) = false
								     

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],4) = 0
val test2_2 = number_in_month ([(2014, 12, 25), (2012,2,28),(2013,12,1)], 12) = 2
								

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1)],4) = []
val test4_2 = dates_in_month ([(2014, 12, 25), (2012,2,28),(2013,12,1)], 12) = [(2014, 12, 25), (2013, 12, 1)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 3) = "how"
val test6_3 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"
val test6_4 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
								    

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7_1 = date_to_string (2013, 1, 1) = "January 1, 2013"
val test7_2 = date_to_string (2013, 2, 1) = "February 1, 2013"
val test7_3 = date_to_string (2013, 3, 1) = "March 1, 2013"
val test7_4 = date_to_string (2013, 4, 1) = "April 1, 2013"
val test7_5 = date_to_string (2013, 5, 1) = "May 1, 2013"
val test7_6 = date_to_string (2013, 7, 1) = "July 1, 2013"
val test7_7 = date_to_string (2013, 8, 1) = "August 1, 2013"
val test7_8 = date_to_string (2013, 9, 1) = "September 1, 2013"
val test7_9 = date_to_string (2013, 10, 1) = "October 1, 2013"
val test7_10 = date_to_string (2013, 11, 1) = "November 1, 2013"
val test7_11 = date_to_string (2013, 12, 1) = "December 1, 2013"
					      

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3
val test9_1 = what_month 16 = 1
val test9_2 = what_month 48 = 2
val test9_3 = what_month 350 = 12

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([(1,2,3),(5,2,3),(7,2,3),(3,2,3)]) = SOME (1,2,3)

val test12 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,3,3,3,3,2,3,3,3,3,3,2,2,2,4,2,2,2,3,3,3,3,4,4,4,4,4,4,4,3,3,2,2,4,4,2,2,3,3,4,4]) = 3
val test13 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,3,3,3,3,2,3,3,3,3,3,2,2,2,4,2,2,2,3,3,3,3,4,4,4,4,4,4,4,3,3,2,2,4,4,2,2,3,3,4,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
