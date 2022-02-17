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

fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    let 
        fun any_comp(lst_a,lst_b) =
            case (lst_a,lst_b) of 
                ([],[])  => EQUAL
                | ([],_) => LESS
                | (_,[]) => GREATER
                | (head_a::tail_a,head_b::tail_b)  => 
                    let 
                        val comp_val = fcomp(head_a,head_b)
                    in 
                        case comp_val of 
                            EQUAL => any_comp(tail_a,tail_b)
                            | LESS => LESS
                            | GREATER => GREATER
                    end
    in
        case (a,b) of 
            ([],[])  => EQUAL
            | ([],_) => LESS
            | (_,[]) => GREATER
            | (_,_)  => any_comp(a,b)
    end  


fun comp_list_int(a, b) =
    comp_list_any(a, b, Int.compare)    


fun is_empty_set s =
    case s of 
        EmptySet s => true
        | Set([],_) => true
        | _ => false

fun min_in_set s =
    let 
        fun min(lst, comp, min_val) = 
            case lst of 
                [] => min_val
                | head::tail =>
                    if comp(head,min_val) = LESS
                    then min(tail,comp,head)
                    else min(tail,comp,min_val)
    in
        case s of 
            EmptySet _ => raise SetIsEmpty
            | Set([],_) => raise SetIsEmpty
            | Set(lst,comp) => min(lst, comp, hd lst)
    end

fun max_in_set s =
    let 
        fun max(lst, comp, max_val) = 
            case lst of 
                [] => max_val
                | head::tail =>
                    if comp(head,max_val) = GREATER
                    then max(tail,comp,head)
                    else max(tail,comp,max_val)
    in
        case s of 
            EmptySet _ => raise SetIsEmpty
            | Set([],_) => raise SetIsEmpty
            | Set(lst,comp) => max(lst, comp, hd lst)
    end

fun insert_into_set(s,v) =
    let
        fun insert(lst,comp,new_lst) = 
            case lst of
                [] => Set(new_lst@[v],comp) 
                | head::tail =>
                    case comp(v,head) of 
                        GREATER => insert(tail,comp,new_lst@[head])
                        | LESS => Set((new_lst@[v])@lst, comp)
                        | EQUAL => s
    in
        case s of 
            EmptySet x => Set([v],x)
            | Set(lst,comp) => insert(lst, comp, [])
    end

fun in_set(s, v) =
    let 
        fun find(lst, comp) = 
            case lst of 
                [] => false
                | head::tail =>
                    if comp(v,head) = EQUAL
                    then true
                    else find(tail, comp)
    in
        case s of 
            EmptySet _ => false
            | Set(lst,comp) => find(lst, comp)
    end

fun union_set(s, t) =
    let 
        fun union(lst,new_set) =
            case lst of 
                [] => new_set
                | head::tail => union(tail,insert_into_set(new_set,head))
    in
        case (s,t) of
            (EmptySet x,EmptySet _) => EmptySet x
            | (EmptySet _,Set(lst,comp)) => Set(lst,comp)
            | (Set(lst,comp),EmptySet _) => Set(lst,comp)
            | (Set(_,_),Set(lst_t,comp)) => union(lst_t,s)
    end

fun intersect_set(s, t) =
    let 
        fun intersect(lst,new_set) =
            case lst of 
                [] => new_set
                | head::tail => 
                    if in_set(t,head)
                    then intersect(tail,insert_into_set(new_set, head))
                    else intersect(tail, new_set)
    in
        case (s,t) of
            (EmptySet x,EmptySet _) => EmptySet x
            | (EmptySet x,_) => EmptySet x
            | (_,EmptySet x) => EmptySet x
            | (Set(lst_s,comp),Set(_,_)) => intersect(lst_s,EmptySet comp)
    end

fun except_set(s, t) =
        let 
        fun except(lst,new_set) =
            case lst of 
                [] => new_set
                | head::tail => 
                    if in_set(t,head)
                    then except(tail, new_set) 
                    else except(tail,insert_into_set(new_set, head))
    in
        case (s,t) of
            (EmptySet x,_) => EmptySet x
            | (_,EmptySet _) => s
            | (Set(lst_s,comp),Set(_,_)) => except(lst_s,EmptySet comp)
    end

fun remove_from_set(s,v) =
    let
        fun remove(lst,comp,new_lst) = 
            case lst of
                [] => Set(new_lst,comp) 
                | head::tail =>
                    case comp(v,head) of 
                        EQUAL => Set(new_lst@tail,comp) 
                        | _ => remove(tail,comp,new_lst@[head])
    in
        case s of 
            EmptySet _ => s
            | Set(head::[],comp) => 
                if comp(v,head) = EQUAL
                then EmptySet(comp)
                else s
            | Set(lst,comp) => remove(lst, comp, [])
    end
    
fun size_set(s: 'a set) =
        let
        fun size(lst,size_val) = 
            case lst of
                [] => size_val
                | _::tail =>  size(tail,size_val+1)
                    
    in
        case s of 
            EmptySet _ => 0
            | Set(lst,_) => size(lst,0)
    end

fun equal_set(s, t) =
    let
        fun equal(lst_a,lst_b,comp) = 
            case (lst_a,lst_b) of   
                ([],[]) => true
                | ([],_) => false
                | (_,[]) => false
                | _ => 
                    if comp(hd lst_a,hd lst_b) = EQUAL
                    then equal(tl lst_a, tl lst_b, comp)
                    else false 
    in
        case (s,t) of 
            (EmptySet _, EmptySet _) => true
            | (EmptySet _,_) => false
            | (_,EmptySet _) => false 
            | (Set(lst_s,comp),Set(lst_t,_)) => equal(lst_s,lst_t,comp)
    end

fun is_subset_of(s, t) =
    let
        fun subset(lst_sub,lst_set,comp) = 
            case (lst_sub,lst_set) of   
                ([],_) => true
                | (_,[]) => false
                | _ => 
                    if comp(hd lst_sub,hd lst_set) = EQUAL
                    then subset(tl lst_sub, tl lst_set, comp)
                    else subset(lst_sub, tl lst_set, comp)
    in
        case (s,t) of 
            (EmptySet _, EmptySet _) => true
            | (EmptySet _,_) => true
            | (_,EmptySet _) => false 
            | (Set(lst_s,comp),Set(lst_t,_)) => subset(lst_s,lst_t,comp)
    end
        
fun list_to_set(lst, f) =
    let
        fun make_set(lst_a,new_set) = 
            case lst_a of 
                [] => new_set
                | head::tail => make_set(tail, insert_into_set(new_set,head)) 
    in
        case lst of 
            [] => EmptySet f 
            | _ => make_set(lst,EmptySet f)
    end

fun set_to_list s =
    case s of 
        EmptySet _ => []
        | Set(lst,_) => lst

fun str_set (s, fstr) =
    let 
        fun make_str(lst, is_first, output) =
            if is_first
            then
                case lst of 
                    [] => output ^ "}"
                    | head::tail => make_str(tail,false,output ^ fstr(head))
            else 
                case lst of 
                    [] => output ^ "}"
                    | head::tail => make_str(tail,false,output ^ ":" ^ fstr(head))
    in
        case s of 
            EmptySet _ => "{}"
            | Set(lst,_) => make_str(lst,true,"{")
    end

fun map_set (s, fcomp, f) =
    let 
        fun map(lst,new_set) = 
            case lst of 
                [] => new_set
                | head::tail => map(tail,insert_into_set(new_set,f(head)))
                
    in
        case s of 
            EmptySet _ => EmptySet fcomp
            | Set(lst,_) => map(lst, EmptySet fcomp)
    end

fun add_one num = 
    num + 1


fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s,t)
fun s UNION t = union_set(s,t)
fun s INTERSECT t = intersect_set(s,t)
fun s EXCEPT t = except_set(s,t)
fun v IN s = in_set(s,v)
fun s IS_SUBSET_OF t = is_subset_of(s,t)



val x1 = Set(["abc","hi","name"],String.compare)
val x2 = Set([1,2,3,67,87],Int.compare)
val x3 = Set([1,2,3,67,87],Int.compare)
val x4 = Set([67],Int.compare)
val x5 = Set([5,9,100],Int.compare)
val x6 = Set([[4,5]], comp_list_int)
val x7  = Set([], Int.compare)
val l1 = [1,2,3,4]
val l2 = []
val l3 = [[5,6],[1,2],[3,4]]
val empty = EmptySet Int.compare

val test1 = x2 ++ 90

val e1 = is_empty_set(x1)
val e2 = is_empty_set(x7)
val e3 = is_empty_set(empty)

val t3 = in_set(x1,"2")
val t4 = in_set(x2,1)

val min1 = min_in_set(x1)
val min2 = min_in_set(x2)
(* val min3 = min_in_set(empty)
val min4 = min_in_set(x7)  *)

val max1 = max_in_set(x1)
val max2 = max_in_set(x2)
(* val max1 = max_in_set(empty) *)
(* val max2 = max_in_set(x7) *)

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

val map1 = map_set(x2,Int.compare,add_one)