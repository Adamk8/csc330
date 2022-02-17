structure Main =
struct
local
  open Csc330
  open Set           
in

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun comp_list_int(a, b) =
    comp_list_any(a, b, Int.compare)

fun comp_list_string(a, b) =
    comp_list_any(a, b, String.compare)

fun comp_pair_any ( (a1,b1), (a2,b2), fcomp) =
    comp_list_any ([a1,b1], [a2,b2], fcomp)

fun comp_pair_int (a,b) =
    comp_pair_any(a,b, Int.compare)

fun str_pair_int(a,b) =
    "(" ^ Int.toString(a) ^ ":" ^ Int.toString(b) ^ ")"

fun str_list_any (lst,fstr) =
    "[" ^ (String.concatWith "," (List.map fstr lst)) ^ "]"

fun str_list_int (lst) =
    str_list_any(lst, Int.toString)

fun ident x = x

fun str_list_string (lst) =
    str_list_any(lst, ident)

fun test (exp, errMsg) =
    if exp then
      ()
    else
      print("test failed: " ^ errMsg ^ "\n")

fun assert (exp, errMsg) =
    if exp then
      ()
    else
      (TextIO.output (TextIO.stdErr, errMsg ^ "\n"); OS.Process.exit(OS.Process.failure))

fun test_empty() =
    test(size_set(EmptySet Int.compare) = 0, "Test empty set")

fun test_set_int() =
    let
      val se = EmptySet Int.compare
      val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2
      val sb = se ++ 9 ++ 3 ++ 2
      val _ = test(sb IDENTICAL list_to_set ([2,3,9], Int.compare), "testing simple insertion")
      val _ = test(sa IDENTICAL list_to_set ([1,2,3,5], Int.compare), "testing duplicate insertion")
    in
      ()
    end
      
fun test_set_ops() =
    let
      val se = EmptySet Int.compare
      val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2
      val sb = se ++ 9 ++ 3 ++ 2 --2
      val _ = test(is_empty_set se, "testing is_empty_set")
      val _ = test(sb UNION sb IDENTICAL sb, "testing union")
      val _ = test(sb INTERSECT se IDENTICAL se, "testing intersect")
      val _ = test(sb EXCEPT sb IDENTICAL se, "testing intersect")
      val _ = test(not(2 IN sb), "testing IN")
      val _ = test(se IS_SUBSET_OF sb, "testing IS_SUBSET_OF")
    in
      ()
    end

fun test_conversions() =
    let
      val se = EmptySet Int.compare
      val sb = se ++ 9 ++ 3 ++ 2
      val _ = test(str_set(se, Int.toString) = "{}", "testing set_str")
      val _ = test(set_to_list(sb) = [2,3,9], "testing set_to_list")
    in
      ()
    end

fun test_others() =
    let
      val se = EmptySet String.compare
      val sa = se ++ "" ++ "a" ++ "ab" ++ "abc" ++ "ab" ++ "ac" ++ " a" 
      val _ = test(max_in_set(sa) = "ac", "testing max_in_set")
      val _ = test(min_in_set(sa) = "", "testing min_in_set")
    in
      ()
    end

fun test_pairs() =
    let
      val se = EmptySet comp_pair_int
      val sa = se ++ (1,3) ++ (1,2)
      val _   = test(str_set(sa, str_pair_int) = "{(1:2):(1:3)}",  "testing a pair")
    in
      ()
    end

fun test_lists() =
    let
      val se = EmptySet comp_list_int
      val sa = se ++ [1,3] ++ [1,2] ++ [] ++ [2,3,9]
      val _   = test(str_set(sa, str_list_int) = "{[]:[1,2]:[1,3]:[2,3,9]}",  "testing a list")
    in
      ()
    end

fun test_lists_strings() =
    let
      val se = EmptySet comp_list_string
      val sa = se ++ ["","abc"] ++ ["daniel","robert"] ++ [] ++ ["Canada","Japan","Brazil"] ++ ["","abc"]
      val _   = test(str_set(sa, str_list_string) = "{[]:[,abc]:[Canada,Japan,Brazil]:[daniel,robert]}", 
                      "testing a list of strings")
    in
      ()
    end

fun test_everything() = 
  let
    val x1 = Set(["abc","hi","name"],String.compare)
    val x2 = Set([1,2,3,67,87],Int.compare)
    val x3 = Set([1,2,3,67,87],Int.compare)
    val x4 = Set([67],Int.compare)
    val x5 = Set([5,9,100],Int.compare)
    val x6 = Set([[4,5]], comp_list_int)
    val l1 = [1,2,3,4]
    val l2 = []
    val l3 = [[5,6],[1,2],[3,4]]
    val empty = EmptySet Int.compare


    val t3 = in_set(x1,"2")
    val t4 = in_set(x2,1)
    val min1 = min_in_set(x1)
    val min2 = min_in_set(x2)
    val max1 = max_in_set(x1)
    val max2 = max_in_set(x2)

    val insert1 = insert_into_set(x1,"insert")
    val insert2 = insert_into_set(x2,5)
    val insert3 = insert_into_set(empty,5)
    val insert4 = insert_into_set(x2,5)
    val insert5 = insert_into_set(x6,[1,2])

    val remove1 = remove_from_set(x1,"hi")
    val remove2 = remove_from_set(x2,87)
    val remove3 = remove_from_set(empty,3)

    val size1 = size_set(x1)
    val size2 = size_set(empty)

    val equal1 = equal_set(x2,x3)
    val equal2 = equal_set(x2,remove2)
    val equal3 = equal_set(x2,empty)
    val equal4 = equal_set(empty,empty)

    val sub1 = is_subset_of(empty,empty)
    val sub2 = is_subset_of(empty,x2)
    val sub3 = is_subset_of(x2,empty)
    val sub4 = is_subset_of(x2,x3)
    val sub5 = is_subset_of(x4,x3)
    val sub6 = is_subset_of(x3,x4)

    val set1 = list_to_set(l1, Int.compare)
    val set2 = list_to_set(l2, String.compare)
    val set3 = list_to_set(l3, comp_list_int)

    val list1 = set_to_list(x1)
    val list2 = set_to_list(x2)
    val list3 = set_to_list(empty)

    val str1 = str_set(x1, String.toString)
    val str2 = str_set(x2, Int.toString)
    val str3 = str_set(empty, Int.toString)

    val union1 = union_set(x2,x5)
    val union2 = union_set(x3,empty)

    val intersect1 = intersect_set(x2,x5)
    val intersect2 = intersect_set(x3,x4)
    val intersect3 = intersect_set(x3,empty)

    val except1 = except_set(x2,x5)
    val except2 = except_set(x3,x4)
    val except3 = except_set(x2,x3)
    val except4 = except_set(empty,x3)
    val except5 = except_set(x2,empty)
  in
    ()
  end


fun main (prog_name, args) =
    let
      val _ = test_empty()
      val _ = test_set_int()
      val _ = test_set_ops()
      val _ = test_conversions()
      val _ = test_others()
      val _ = test_pairs()
      val _ = test_lists()
      val _ = test_lists_strings()
      val results = test_everything()
    in
      print("Finished testing\n"); exit()
    end

                                
end
end    
