structure Main =
(* do not modify this file *)


struct

local
  open Csc330
  open Babies
in

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
          
  fun main (prog_name, args) =
      let
        val (_, fileName, offsetSt) = parse_command_line args
        val output = babies_program(read_file(fileName), read_stdin(), offsetSt)
      in
        finish(output)
      end
        

end

end
