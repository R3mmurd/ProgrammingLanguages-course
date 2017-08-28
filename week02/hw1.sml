(* problem 1 *)
fun is_older (d1 : (int * int * int), d2 : (int * int *int)) =
  if (#1 d1) < (#1 d2)
  then true (* if year 1 < year 2, then d1 comes before d2 *)
  else if (#1 d1) > (#1 d2)
  then false (* if year 1 > year 2, then d1 does not come before d2 *)
  else if (#2 d1) < (#2 d2) (* year 1 = year 2, compare months *)
  then true (* if month 1 < month 2, then d1 comes before d2 *)
  else if (#2 d1) > (#2 d2)
  then false (* if month 1 > month 2, then d1 does not come before d2 *)
  else (#3 d1) < (#3 d2) (* month 1 = month 2, set result from days *)

(* problem 2 *)
fun number_in_month (ds : (int * int * int) list, m : int) =
  if null ds
  then 0
  else let
      val tl_number = number_in_month(tl ds, m)
  in
      if #2 (hd ds) = m
      then 1 + tl_number
      else tl_number
  end

(* problem 3 *)
fun number_in_months (ds : (int * int * int) list, ms : int list) =
  if null ms
  then 0
  else if null ds
  then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

(* problem 4 *)
fun dates_in_month (ds : (int * int * int) list, m : int) =
  if null ds
  then []
  else let
      val tl_dates = dates_in_month(tl ds, m)
  in
      if #2 (hd ds) = m
      then (hd ds) :: tl_dates
      else tl_dates
  end

(* problem 5 *)
fun dates_in_months (ds : (int * int * int) list, ms : int list) =
  if null ms
  then []
  else if null ds
  then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

(* problem 6 *)
fun get_nth (ss : string list, n : int) =
  if n = 1
  then hd ss
  else get_nth(tl ss, n - 1)

(* problem 7 *)
fun date_to_string (d : int * int * int) =
  get_nth(["January", "February", "March", "April", "May", "June", "July",
	   "August", "September", "October", "November", "December"], #2 d) ^
  " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)

(* problem 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
  if null xs
  then 0
  else let
      fun get_number (sum : int, xs : int list, i : int) =
	if (hd xs) >= sum
	then i
	else get_number(sum - (hd xs), tl xs, i + 1)
  in
      get_number(sum, xs, 0)
  end

(* problem 9 *)
fun what_month (d : int) =
  let
      val months_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      fun get_month_number (d : int, mds : int list, i : int) =
	if d <= (hd mds)
	then i
	else get_month_number(d - (hd mds), tl mds, i + 1)
  in
      get_month_number(d, months_days, 1)
  end

(* problem 10 *)
fun month_range (d1 : int, d2 : int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2)

(* problem 11 *)
fun oldest (ds : (int * int * int) list) =
  if null ds
  then NONE
  else let
      fun oldest_nonempty (ds : (int * int * int) list) =
	if null (tl ds)
	then hd ds
	else let
	    val tl_oldest = oldest_nonempty(tl ds)
	in
	    if is_older(hd ds, tl_oldest)
	    then hd ds
	    else tl_oldest
	end
  in
      SOME (oldest_nonempty ds)
  end

(* challenge problems 12 *)

(* check if the month m is in the months list ms *)
fun contains (ms : int list, m : int) =
  if null ms
  then false
  else if (hd ms) = m
  then true
  else contains(tl ms, m)	   
	       
(* challenge problem 12 part 1 *) 
fun number_in_months_challenge (ds : (int * int * int) list, ms : int list) =
  if null ms
  then 0
  else if null ds
  then 0
  else let
      fun count (ds : (int * int * int) list, ms : int list, pms : int list) =
	if null ms
	then 0
	else if contains(pms, hd ms)
	then count(ds, tl ms, pms)
	else number_in_month(ds, hd ms) + count(ds, tl ms, (hd ms) :: pms)
  in
      count(ds, ms, [])
  end

(* challenge problem 12 part 2 *) 
fun dates_in_months_challenge (ds : (int * int * int) list, ms : int list) =
  if null ms
  then []
  else if null ds
  then []
  else let
      fun build (ds : (int * int * int) list, ms : int list, pms : int list) =
	if null ms
	then []
	else if contains(pms, hd ms)
	then build(ds, tl ms, pms)
	else dates_in_month(ds, hd ms) @ build(ds, tl ms, (hd ms) :: pms)
  in
      build(ds, ms, [])
  end

(* challenge problem 13 *)
fun reasonable_date (d : int * int * int) =
  if (#1 d) < 1 (* validating year *)
  then false
  else if (#2 d) < 1 orelse (#2 d) > 12 (* validating month *)
  then false
  else let
      fun is_leap_year (y : int) =
	y mod 4 = 0 andalso (y mod 400 = 0 orelse y mod 100 <> 0)

      val months_days = if is_leap_year(#1 d)
			then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
			else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      fun get_nth_month_days (mds : int list, n : int) =
	if n = 1
	then hd mds
	else get_nth_month_days(tl mds, n - 1)
  in
      (#3 d) >= 1 andalso (#3 d) <= get_nth_month_days(months_days, #2 d)
  end
