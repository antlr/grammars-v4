class SHOW_PRIMES

inherit
   ARGUMENTS

create
   main

feature

   main
      local
         i,n: INTEGER
      do
         if argument_count = 1 then
            n := argument(1).to_integer
            from
               i := 2
            until
               i > n
            loop
               if is_prime(i) then
                  io.put_string(i.out+"%N");
               end;
               i := i+1
            end
         else
            io.put_string("Usage: "+argument(0)+" <n>%N")
         end
      end

   is_prime(n: INTEGER): BOOLEAN
      require
         n > 1
      local
         i: INTEGER
      do
         Result := (n = 2) or (n \\ 2 /= 0);
         from
            i := 3
         until
            not Result or i*i > n
         loop
            Result := (n \\ i /= 0);
            i := i + 2
         end
      end

end -- SHOW_PRIMES
