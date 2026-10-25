class PROG_ARGS

inherit
   ARGUMENTS

create
   main

feature

   main
      local
         i: INTEGER
      do
         from
            i := 0
         until
            i > argument_count
         loop
            io.put_string("Argument "+i.out+": "+argument(i)+"%N");
            i := i + 1
         end
      end

end -- PROG_ARGS
