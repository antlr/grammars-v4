(*
   This method implements a driver for testing the ATOI class.
The program repeatedly asks the user to enter a number, which
is then coverted from its string form to an integer and back
again to a string.  The results of both conversions are printed
on the screen.  Typing "stop" at the prompt exits the program.
*)

class Main inherits IO {
   newline() : Object {
	out_string("\n")
   };

   prompt() : String {
	{
	   out_string("Enter a number>");
	   in_string();
	}
   };

   main() : Object {
   (* Since we didn't bother to inherit from the A2I class, we have
	to have an object of type A2I in order to access the
	methods of that class. *)
     (let z : A2I <- new A2I in
	while true loop  
	   (let s : String <- prompt() in
		if s = "stop" then 
		    abort() -- we don't bother to terminate gracefully
		else
		   (let i : Int <- z.a2i(s) in
			(let news : String <- z.i2a(i) in
			   {
			     out_int(i);
			     newline();
			     out_string(news);
			     newline();
			   }
	                )
                  )
		fi
	   )
        pool
     )
   };
};
