

(* Check if int exists in a list  *)
fun exist(arr: int list, key: int) = 
    if null arr 
    then false 
    else 
        if hd arr = key 
        then true 
        else exist(tl arr, key)

val a1 = exist([], 1)
val a2 = exist([1,2,3],4)
val a3 = exist([1,2,3],1)
val a4 = exist([1,2,3],2)
val a5 = exist([1,2,3],3)

(* Check if int exists in a list  *)


fun index(arr: int list, key: int) = 
    let 
        fun get_index(arr: int list, index:int) = 
            if null arr 
            then NONE 
            else 
                if hd arr = key 
                then SOME(index) 
                else get_index(tl arr, index + 1)
    in
        get_index(arr, 0)
    end

val b1 = index([], 1)
val b2 = index([1,2,3],4)
val b3 = index([1,2,3],1)
val b4 = index([1,2,3],2)
val b5 = index([1,2,3],3)
