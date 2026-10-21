(*
 *  The IO class is predefined and has 4 methods:
 *
 *    out_string(s : String) : SELF_TYPE
 *    out_int(i : Int) : SELF_TYPE
 *    in_string() : String
 *    in_int() : Int
 *
 *    The out operations print their argument to the terminal. The
 *    in_string method reads an entire line from the terminal and returns a
 *    string not containing the new line. The in_int method also reads
 *    an entire line from the terminal and returns the integer
 *    corresponding to the first non blank word on the line. If that
 *    word is not an integer, it returns 0.
 *
 *
 *  Because our language is object oriented, we need an object of type
 *  IO in order to call any of these methods.
 *
 *  There are basically two ways of getting access to IO in a class C.
 *
 *   1) Define C to Inherit from IO. This way the IO methods become
 *      methods of C, and they can be called using the abbreviated
 *      dispatch, i.e.
 *
 *      class C inherits IO is
 *          ...
 *          out_string("Hello world\n")
 *          ...
 *      end;
 *
 *   2) If your class C does not directly or indirectly inherit from
 *      IO, the best way to access IO is through an initialized
 *      attribute of type IO. 
 *
 *      class C inherits Foo is
 *         io : IO <- new IO;
 *         ...
 *             io.out_string("Hello world\n");
 *         ...
 *      end;
 *
 *  Approach 1) is most often used, in particular when you need IO
 *  functions in the Main class.
 *
 *)


class A {

   -- Let's assume that we don't want A to not inherit from IO.

   io : IO <- new IO;

   out_a() : Object { io.out_string("A: Hello world\n") };

};


class B inherits A {

   -- B does not have to an extra attribute, since it inherits io from A.

   out_b() : Object { io.out_string("B: Hello world\n") };

};


class C inherits IO {

   -- Now the IO methods are part of C.

   out_c() : Object { out_string("C: Hello world\n") };

   -- Note that out_string(...) is just a shorthand for self.out_string(...)

};


class D inherits C {

   -- Inherits IO methods from C.

   out_d() : Object { out_string("D: Hello world\n") };

};


class Main inherits IO {

   -- Same case as class C.

   main() : Object {
      {
	 (new A).out_a();
	 (new B).out_b();
	 (new C).out_c();
	 (new D).out_d();
	 out_string("Done.\n");
      }
   };

};
