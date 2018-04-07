
(*
 * methodless-primes.cl
 *
 * Designed by Jesse H. Willett, jhw@cory, 11103234, with 
 *             Istvan Siposs, isiposs@cory, 12342921.
 *
 * This program generates primes in order without using any methods.
 * Actually, it does use three methods: those of IO to print out each prime, and
 * abort() to halt the program.  These methods are incidental, however,
 * to the information-processing functionality of the program.  We
 * could regard the attribute 'out's sequential values as our output,
 * and the string "halt" as our terminate signal.
 *
 * Naturally, using Cool this way is a real waste, basically reducing it 
 * to assembly without the benefit of compilation.  
 *
 * There could even be a subroutine-like construction, in that different
 * code could be in the assign fields of attributes of other classes,
 * and it could be executed by calling 'new Sub', but no parameters
 * could be passed to the subroutine, and it could only return itself.
 * but returning itself would be useless since we couldn't call methods
 * and the only operators we have are for Int and Bool, which do nothing
 * interesting when we initialize them!
 *)

class Main inherits IO {

  main() : Int {	-- main() is an atrophied method so we can parse. 
    0 
  };

  out : Int <-		-- out is our 'output'.  It's values are the primes.
    {
      out_string("2 is trivially prime.\n");
      2;
    };

  testee : Int <- out;	-- testee is a number to be tested for primeness.   

  divisor : Int;	-- divisor is a number which may factor testee.

  stop : Int <- 500;	-- stop is an arbitrary value limiting testee. 	

  m : Object <-		-- m supplants the main method.
    while true loop 
      {

        testee <- testee + 1;
        divisor <- 2;

        while 
          if testee < divisor * divisor 
            then false 		-- can stop if divisor > sqrt(testee).
	  else if testee - divisor*(testee/divisor) = 0 
            then false 		-- can stop if divisor divides testee. 
            else true
          fi fi     
        loop 
          divisor <- divisor + 1
        pool;        

        if testee < divisor * divisor	-- which reason did we stop for?
        then 	-- testee has no factors less than sqrt(testee).
          {
            out <- testee;	-- we could think of out itself as the output.
            out_int(out); 
            out_string(" is prime.\n");
          }
        else	-- the loop halted on testee/divisor = 0, testee isn't prime.
          0	-- testee isn't prime, do nothing.
	fi;   	

        if stop <= testee then 
          "halt".abort()	-- we could think of "halt" as SIGTERM.
        else 
          "continue"
        fi;       

      } 
    pool;

}; (* end of Main *)

