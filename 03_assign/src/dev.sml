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
            else nodeTree(curr,left,nodeTree(v,emptyTree,emptyTree))

   
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
                then 
                    let 
                        val max_child = valOf(tree_max(right))
                    in
                        nodeTree(max_child,emptyTree,tree_delete(right,max_child))
                    end
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
    tree_fold_pre_order (fn (acc,x) => acc@[x]) [] t

fun tree_filter f t = 
    case t of 
        emptyTree => emptyTree
        | nodeTree(x,left,right) => 
            if f(x)
            then nodeTree(x,tree_filter f left,tree_filter f right)
            else 
                let 
                    val new_tree = tree_delete(t,x)
                in    
                   tree_filter f new_tree
                end

fun tree_sum_even t = 
    let 
        val filtered_tree = tree_filter (fn x => x mod 2 = 0) t
    in 
        tree_fold_pre_order (fn (acc,x) => acc + x) 0 filtered_tree
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


(* Testing *)

val t1 = emptyTree
val insert1 = tree_insert_in_order(t1,5)
val insert2 = tree_insert_in_order(insert1,3)
val insert3 = tree_insert_in_order(insert2,1)
val insert4 = tree_insert_in_order(insert3,4)
val insert5 = tree_insert_in_order(insert4,8)

val h1 = tree_height(insert5)
val h2 = tree_height(insert2)
val h3 = tree_height(insert1)

val max1 = tree_max(insert3)
val max2 = tree_max(insert5)
val max3 = tree_max(t1)

val delete1 = tree_delete(insert3,1)
val delete2 = tree_delete(insert5,4)

val list1 = tree_to_list(insert5)
val list2 = tree_to_list(emptyTree)

val sum1 = tree_sum_even(insert5)
val sum2 = tree_sum_even(insert3)
val sum3 = tree_sum_even(insert4)
val sum4 = tree_sum_even(t1)