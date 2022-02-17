

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

val p1 = Variable "cat"
val p2 = TupleP [Wildcard,Variable "cat",
                         Variable "pp",TupleP[Variable "tt"],
                         Wildcard,ConstP 3,
                         ConstructorP("cony",Variable "pp")]

val c1 = check_pattern p1
val c2 = check_pattern p2