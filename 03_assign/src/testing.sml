structure Testing =
struct

fun do_test_set(t: string, tests: bool list) =
  let 
    fun toInt (b: bool) =
      if b then 1 else 0;

    val len = length tests
    val count = foldl op+ 0 (map toInt tests)
                      
    fun zip_with_index(xs) =
      let
        fun add(i, xs) =
          case xs of
              [] => []
            | head::tail => (i,head) :: add(i+1, tail)
      in
        add(1,xs)
      end
          
    fun report_failed(tests_with_i) =
        let
          fun report_each(xs) =
              case xs of
                  [] => []
               |  (i,x)::tail =>
                  (if not x then
                     print ("      * Subtest  " ^ Int.toString(i) ^ " failed\n")
                   else
                     ()) :: report_each(tail)
        in
          print("*Test '" ^ t ^ "' failed!!!!!!!\n   "^ Int.toString(len -count) ^ " out of " ^ Int.toString(len) ^ " failed\n");
          List.length (report_each(tests_with_i)) = 0
        end
        
  in
    if (count = len) then 
      (print ("*Test '" ^ t ^ "' passed\n   "^ Int.toString(count) ^ " out of " ^ Int.toString(len) ^ "\n");
       count = len)
    else
      report_failed(zip_with_index(tests))
  end

end

