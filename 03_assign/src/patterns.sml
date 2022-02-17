(*
Your name:
Your student id:
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value


(* write your tree functions here *)


fun tree_insert_in_order(t, v) = 
    case t of
        emptyTree => nodeTree(v,emptyTree,emptyTree)
        | nodeTree (curr, emptyTree, emptyTree) => 
            if v <= curr
            then nodeTree(curr,nodeTree(v,emptyTree,emptyTree),emptyTree)
            else nodeTree(curr,emptyTree,nodeTree(v,emptyTree,emptyTree))
        | nodeTree (curr, left, right) =>
            if v <= curr
            then nodeTree(curr,tree_insert_in_order(left,v),right)
            else nodeTree(curr,left,tree_insert_in_order(right,v))

   
fun tree_fold_pre_order f acc t = 
    case t of 
        emptyTree => acc
        | nodeTree(x,left,right) => tree_fold_pre_order f (tree_fold_pre_order f (f(acc,x)) left) right

fun tree_max t = 
    let 
        fun max_option(a,b) = 
            if isSome(a)
            then
                if valOf(a) >= b
                then a 
                else SOME b
            else SOME b
            
    in
        tree_fold_pre_order max_option NONE t 
    end

fun tree_height t = 
    let
      fun get_max(a,b) =
        if a >= b
        then a 
        else b  
    in
      case t of 
        emptyTree => 0
        | nodeTree(_,left,right) => get_max(1 + tree_height(left),1 + tree_height(right))
    end
    

fun tree_delete(t,v) = 
        case t of 
            emptyTree => raise NotFound
            | nodeTree (curr, emptyTree, emptyTree) => 
                if v = curr
                then emptyTree
                else raise NotFound
            | nodeTree (curr, left, emptyTree) =>
                if v = curr
                then left
                else 
                    if v < curr
                    then nodeTree(curr,tree_delete(left,v),emptyTree)
                    else raise NotFound
            | nodeTree (curr, emptyTree, right) =>
                if v = curr
                then right
                else 
                    if v < curr
                    then raise NotFound
                    else nodeTree(curr,emptyTree,tree_delete(right,v))
            | nodeTree (curr, left, right) =>
                if v = curr 
                then 
                    let 
                        val max_child = valOf(tree_max(left))
                    in
                        nodeTree(max_child,tree_delete(left,max_child),right)
                    end
                else 
                    if v < curr
                    then nodeTree(curr,tree_delete(left,v),right)
                    else nodeTree(curr,left,tree_delete(right,v))


fun tree_to_list t = 
    let
      val result = tree_fold_pre_order (fn (acc,x) => acc@[x]) [] t
    in
        result
    end
    

fun tree_filter f t = 
    case t of 
        emptyTree => t
        | nodeTree(x,left,right) => 
            if f(x)
            then nodeTree(x,(tree_filter f left),(tree_filter f right))
            else (tree_filter f (tree_delete(t,x)))

fun tree_sum_even t = 
        tree_fold_pre_order (fn (acc,x) => acc + x) 0 (tree_filter (fn x => x mod 2 = 0) t)


fun first_answer f lst =
    case lst of 
        [] => raise NoAnswer
        | head::tail => 
            if isSome(f(head))
            then valOf(f(head))
            else first_answer f tail

fun all_answers f lst = 
    let
        fun check_elements(lst, acc) =
            case lst of 
                [] => acc 
                | head::tail => 
                    case f(head) of 
                        SOME(x) => check_elements(tail, (SOME (valOf(acc)@x)))
                        | NONE => NONE

    in
      check_elements(lst, (SOME []))
    end


fun check_pattern p = 
    let 
        fun get_strings (p,acc) = 
            case p of 
                Variable s => acc@[s]
                | ConstructorP(_,Variable(s)) => acc@[s]
                | TupleP (head::[]) => (get_strings(head, acc))
                | TupleP (head::tail) => (get_strings(head, acc))@(get_strings ((TupleP tail), acc))
                | _ => acc

        fun not_in_list (s,lst) = 
            case lst of 
                [] => true
                | head::tail =>
                    if head = s 
                    then false
                    else not_in_list(s,tail)

        fun no_repeats lst = 
            case lst of 
                [] => true
                | head::tail => not_in_list(head,tail) andalso no_repeats tail
        
        fun all_strings p = List.foldl (get_strings) [] p

        val string_list = 
            case p of 
                TupleP lst => all_strings lst
                | Variable s => [s]
                | ConstructorP(_,Variable(s)) => [s]
                | _ => []

        val result = no_repeats string_list
    in 
       result
    end

fun match(v, p) =
    let 
        fun compare_tuples(v_lst, p_lst) = 
            if List.length(v_lst) = List.length(p_lst)
            then all_answers (fn x => case x of (a,b) => (match(a, b))) (ListPair.zip(v_lst,p_lst))
            else NONE 

    in 
        case (v,p) of 
            (_,Wildcard) => SOME []
            | (Unit,UnitP) => SOME []
            | (_,Variable s) => SOME [(s,v)]
            | (Const(i),ConstP(j)) => if i = j then SOME [] else NONE
            | (Tuple(v_lst),TupleP(p_lst)) => compare_tuples(v_lst,p_lst)
            | (Constructor(s1,v1),ConstructorP(s2,p2)) => 
                if s1 = s2 andalso isSome(match(v1, p2))
                then (match(v1, p2))
                else NONE 
            | (_,_) => NONE
    end

fun first_match v p_lst = 
    let 
        fun curryed_match v p = match(v,p)
    in
        (SOME (first_answer (curryed_match v) p_lst)) handle NoAnswer => NONE
    end




(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end

