(*
Your name:
Your student id:
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun is_empty_set s =
    false

fun min_in_set s =
     raise SetIsEmpty

fun max_in_set s =
    raise SetIsEmpty

fun insert_into_set(s,v) =
    s

fun in_set(s, v) =
    true

fun union_set(s, t) =
    s

fun intersect_set(s, t) =
    s

fun except_set(s, t) =
    s

fun remove_from_set(s,v) =
    s
    
fun size_set(s: 'a set) =
    0

fun equal_set(s, t) =
    false

fun is_subset_of(s, t) =
    false
        
fun list_to_set(lst, f) =
    EmptySet f

fun set_to_list s =
    []

fun str_set (s, fstr) =
    "{}"
      
fun map_set (s, fcomp, f) =
    EmptySet fcomp

fun s -- v = s
fun s ++ v = s
fun s IDENTICAL t = false
fun s UNION t = s
fun s INTERSECT t = s
fun s EXCEPT t = s
fun v IN s = false
fun s IS_SUBSET_OF t = false


fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    EQUAL
                          
end
end    
