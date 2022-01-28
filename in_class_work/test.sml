val a = 10
val b = 2

fun f (b) = 
let 
val b = 5
in 
a + b
end 

val a = a + 1
val z = f(3)