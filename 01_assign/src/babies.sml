structure Babies =
struct
local
  open Csc330
in
  
  (*
  Student name: Adam Kwan
  Student id: V00887099

  Add your code below this line

  Remember: your submitted code cannot use print

  *)

(* Count number of entries for a baby *)

fun count_entries_per_baby (arr : string list) = 
  let 
    val baby_breakdown = split_at(hd arr , #",") 
    val num_entries = length(baby_breakdown) - 2 
  in
    num_entries
  end 


(* Get the total of a single baby *)
fun get_total_babies(baby_arr: string list) = 
  if null (tl baby_arr)
  then 0 
  else valOf(fromString(hd baby_arr)) + get_total_babies(tl baby_arr)

(* Count the number of years that have a non-zero value *)
fun get_nonzero_years(baby_arr: string list) = 
  if null (tl baby_arr)
  then 0 
  else 
    if valOf(fromString(hd baby_arr)) > 0 
    then 1 + get_nonzero_years(tl baby_arr)
    else 0 + get_nonzero_years(tl baby_arr)

(* Get the total babies from the last year of the data *)
fun get_last_year_total(baby_arr: string list, start_year: int)=
  let 
    fun get_last_year(last_arr: string list, current_year: int)=
      if null (tl (tl last_arr))
      then [current_year, valOf(fromString(hd last_arr))]
      else get_last_year(tl last_arr, current_year + 1)
  in 
    get_last_year(baby_arr, start_year)
  end

(* Get the last year and total of that year for the last non zero year *)
fun get_nonzero_last_year_total(baby_arr: string list, start_year: int)=
  let 
    fun get_last_year(last_arr: string list, last_year: int, current_year: int, last_val: int) = 
      if null (tl last_arr)
      then [last_year,last_val]
      else 
        if valOf(fromString(hd last_arr)) > 0
        then get_last_year(tl last_arr, current_year, current_year + 1, valOf(fromString(((hd last_arr)))))
        else get_last_year(tl last_arr, last_year, current_year + 1,  last_val)
  in 
    get_last_year(baby_arr, start_year,start_year, 0)
  end 

(* Get the non-zero year and total babies *)
fun get_first_year_total(baby_arr: string list, start_year: int)=
  let 
    fun get_first_year(first_arr: string list, current_year: int) = 
      if valOf(fromString(hd first_arr)) > 0 
      then [current_year, valOf(fromString(hd first_arr))]
      else get_first_year(tl first_arr, current_year + 1 )
  in 
    get_first_year(baby_arr, start_year)
  end 

(* Get the year and total of the lowest non-zero year *)
fun get_min_year_list(baby_arr: string list, start_year: int)=
  let 
    fun get_min_year(inner_arr: string list, current_year: int, min_year: int, min_val: int) =
      if null (tl inner_arr)
      then [min_year,min_val]
      else 
        if valOf(fromString(hd inner_arr)) <> 0
        then 
          if min_val = ~1 orelse valOf(fromString(hd inner_arr)) < min_val
          then get_min_year(tl inner_arr, current_year + 1, current_year, valOf(fromString(hd inner_arr)))
          else get_min_year(tl inner_arr, current_year + 1, min_year,  min_val)
        else get_min_year(tl inner_arr, current_year + 1, min_year, min_val)
  in
    get_min_year(baby_arr, start_year, start_year, ~1)
  end

(* Get the year and total of the max babies *)
fun get_max_year_list(baby_arr: string list, start_year: int)=
  let 
    fun get_max_year(inner_arr: string list, current_year: int, max_year: int, max_val: int) =
      if null (tl inner_arr)
      then [max_year,max_val]
      else 
        if valOf(fromString(hd inner_arr)) <> 0
        then 
          if max_val = ~1 orelse valOf(fromString(hd inner_arr)) >= max_val
          then get_max_year(tl inner_arr, current_year + 1, current_year, valOf(fromString(hd inner_arr)))
          else get_max_year(tl inner_arr, current_year + 1, max_year,  max_val)
        else get_max_year(tl inner_arr, current_year + 1, max_year, max_val)
  in
    get_max_year(baby_arr, start_year, start_year, ~1)
  end

(* Calcutate the average babies per year *)
fun get_avg_per_year(baby_arr: string list, num_babies : int) = 
  let 
    val num_years = length(baby_arr) -2 
  in 
    int_to_real(num_babies) / int_to_real(num_years)
  end

(* Build the string of information for a single baby *)
fun get_baby_info(baby_arr: string list, start_year: int) = 
  let 
    val baby_name = hd baby_arr
    val total_babies = get_total_babies(tl baby_arr)
    val nonzero_years = get_nonzero_years(tl baby_arr)
    val last_year_total = get_last_year_total(tl baby_arr, start_year)
    val nonzero_last_year_total = get_nonzero_last_year_total(tl baby_arr, start_year)
    val first_year_total = get_first_year_total(tl baby_arr, start_year)
    val min_array = get_min_year_list(tl baby_arr, start_year)
    val max_array = get_max_year_list(tl baby_arr, start_year)
    val avg_per_year = get_avg_per_year(baby_arr, total_babies)
  in
    hd baby_arr ^ "\n Total: " ^ int_to_string(total_babies)
    ^ "\n Years: " ^ int_to_string(nonzero_years) 
    ^ "\n " ^  int_to_string(hd last_year_total)^ ": " ^ int_to_string(hd (tl last_year_total))
    ^ "\n First: " ^ int_to_string(hd first_year_total) ^ " " ^ int_to_string(hd (tl first_year_total))
    ^ "\n Last: " ^ int_to_string(hd nonzero_last_year_total) ^ " " ^ int_to_string(hd (tl nonzero_last_year_total))
    ^ "\n Min: " ^ int_to_string(hd min_array) ^ " " ^ int_to_string(hd (tl min_array))
    ^ "\n Max: " ^ int_to_string(hd max_array) ^ " " ^ int_to_string(hd (tl max_array))
    ^ "\n Avg: " ^ real_to_string(avg_per_year)
    ^ "\n"
  end
  
(* Search for baby and get information if it exists *)
fun check_for_baby(arr: string list list, babies_list: string list, start_year: int ) = 
  let 
    fun search_for_baby(search_arr, baby_names) =
      if null baby_names
      then ""
      else 
        let 
          fun search_next_line(lines) = 
            if null lines
            then (hd baby_names) ^ "\nBaby name [" ^ (hd baby_names) ^ "] was not found\n"
            else 
              if hd (hd lines) = hd baby_names
              then get_baby_info(hd lines,start_year)
              else search_next_line(tl lines)
        in 
          search_next_line(arr) ^ search_for_baby(search_arr, tl baby_names)
        end
  in
    search_for_baby(arr, babies_list)
  end

(* Format data into 2D array*)
fun format_data(data: string) = 
  let 
    fun line_breakdown(lines: string list) = 
      if null lines 
      then []
      else split_at(hd lines , #",")::line_breakdown(tl lines);
  in 
    line_breakdown(split_at(data , #"\n"))
  end 

(* Format initial string and call other functions to get more data *)
fun babies_program (babiesLines, NamesLines, offsetSt) =
  (* the output of the program is the string returned by this function *)
    let 
      val data_array = format_data(babiesLines)
      val lines = split_at(babiesLines , #"\n") 
      val num_babies = int_to_string(length(lines))
      val num_entries = int_to_string(count_entries_per_baby(lines))
      val search_babies = check_for_baby(data_array, split_at(NamesLines, #"\n"), valOf(fromString(offsetSt)))
    in 
      "Read " ^ num_babies ^ " babies. Starting year " ^ offsetSt ^ ". Each baby has " ^ num_entries ^ " entries.\n"
      ^ search_babies
    end 
      
          
  end

end
    
