

fun repeat(lst: int list, times: int) = 
    if times > 0
    then lst @ repeat(lst, times - 1)
    else []


(* Infix notation*)
fun op *(lst: int list, times: int) = 
    if times > 0
    then lst @ (lst * (times - 1))
    else []

val a1 = repeat([1,2,3],3)
val a2 = repeat([1,2,3],0)
val a3 = repeat([1,2,3],~1)
val a5 = repeat([],4)

val b1 = [1,2,3] * 4

fun len [] = 0
    | len (_::tail) = 1 + len(tail)


val c1 = len([1,2,3,4])
val c2 = len([])


fun remove_duplicates(lst) = 
    case lst of 
    [] => []
    | [x] => [x]
    | head::x::tail =>
        if head = x 
        then remove_duplicates(tl lst)
        else head::remove_duplicates(tl lst)
    
val d1 = remove_duplicates([1,1,2,3,4,4])


(* Function actually finds subsequence *)
fun sublist(lst,sub) = 
    case (lst,sub) of 
        ([],[]) => true
        | (_,[]) => true
        | ([],_) => false
        | (ha::ta,hb::tb) => 
        if ha = hb 
        then sublist(ta,tb)
        else sublist(ta,sub)

val e1 = sublist([1,2,3],[2,3])
val e2 = sublist([1,2,3],[4,3])