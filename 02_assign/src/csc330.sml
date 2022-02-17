(* It's been done in build.cm already. *)

structure Csc330 = 

struct

(* 
  in case you need a dot
*)
val dot = "."


exception DoNotUse

fun exit() = OS.Process.exit(OS.Process.success)

fun failure() = OS.Process.exit(OS.Process.failure)

fun fold x y =
    raise DoNotUse

fun mapPartial  x y = 
    raise DoNotUse

fun foldl x y =
    raise DoNotUse

fun foldr x y =
    raise DoNotUse

fun map x y =
    raise DoNotUse

fun app x y =
    raise DoNotUse

fun parse_command_line args = 
    let
      val _ = if length(args) <> 3 then
                let
                  val _ = print "illegal number of command line parameters\n"
                  val _ = failure()
                in
                  0
                end
              else 0
      val binary::fileName::offsetSt:: _ = args
    in
      (binary, fileName, offsetSt) 
    end

end              
