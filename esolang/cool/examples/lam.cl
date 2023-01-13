(* A program for

   1. Representing lambda terms
   2. Interpreting lambda terms
   3. Compiling lambda calculus programs to Cool

   The lambda calculus is described by the following grammar:

   e ::= x	       a variable
      |  \x.e	       a function with argument x
      |  e1@e2	       apply function e1 to argument e2

  Jeff Foster (jfoster@cs.berkeley.edu)
  March 24, 2000
*)

(*
 * A list of variables.  We use this to do de Bruijn numbering
 *
 *)
class VarList inherits IO {
  isNil() : Bool { true };
  head()  : Variable { { abort(); new Variable; } };
  tail()  : VarList { { abort(); new VarList; } };
  add(x : Variable) : VarList { (new VarListNE).init(x, self) };
  print() : SELF_TYPE { out_string("\n") };
};

class VarListNE inherits VarList {
  x : Variable;
  rest : VarList;
  isNil() : Bool { false };
  head()  : Variable { x };
  tail()  : VarList { rest };
  init(y : Variable, r : VarList) : VarListNE { { x <- y; rest <- r; self; } };
  print() : SELF_TYPE { { x.print_self(); out_string(" ");
	                  rest.print(); self; } };
};

(*
 * A list of closures we need to build.  We need to number (well, name)
 * the closures uniquely.
 *)
class LambdaList {
  isNil() : Bool { true };
  headE() : VarList { { abort(); new VarList; } };
  headC() : Lambda { { abort(); new Lambda; } };
  headN() : Int { { abort(); 0; } };
  tail()  : LambdaList { { abort(); new LambdaList; } };
  add(e : VarList, x : Lambda, n : Int) : LambdaList {
    (new LambdaListNE).init(e, x, n, self)
  };
};

class LambdaListNE inherits LambdaList {
  lam : Lambda;
  num : Int;
  env : VarList;
  rest : LambdaList;
  isNil() : Bool { false };
  headE() : VarList { env };
  headC() : Lambda { lam };
  headN() : Int { num };
  tail()  : LambdaList { rest };
  init(e : VarList, l : Lambda, n : Int, r : LambdaList) : LambdaListNE {
    {
      env <- e;
      lam <- l;
      num <- n;
      rest <- r;
      self;
    }
  };
};

class LambdaListRef {
  nextNum : Int <- 0;
  l : LambdaList;
  isNil() : Bool { l.isNil() };
  headE() : VarList { l.headE() };
  headC() : Lambda { l.headC() };
  headN() : Int { l.headN() };
  reset() : SELF_TYPE {
    {
      nextNum <- 0;
      l <- new LambdaList;
      self;
    }
  };
  add(env : VarList, c : Lambda) : Int {
    {
      l <- l.add(env, c, nextNum);
      nextNum <- nextNum + 1;
      nextNum - 1;
    }
  };
  removeHead() : SELF_TYPE {
    {
      l <- l.tail();
      self;
    }
  };
};

(*
 * Lambda expressions
 *
 *)

-- A pure virtual class representing any expression
class Expr inherits IO {

  -- Print this lambda term
  print_self() : SELF_TYPE {
    {
      out_string("\nError: Expr is pure virtual; can't print self\n");
      abort();
      self;
    }
  };

  -- Do one step of (outermost) beta reduction to this term
  beta() : Expr {
    {
      out_string("\nError: Expr is pure virtual; can't beta-reduce\n");
      abort();
      self;
    }
  };

  -- Replace all occurrences of x by e
  substitute(x : Variable, e : Expr) : Expr {
    {
      out_string("\nError: Expr is pure virtual; can't substitute\n");
      abort();
      self;
    }
  };

  -- Generate Cool code to evaluate this expression
  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("\nError: Expr is pure virtual; can't gen_code\n");
      abort();
      self;
    }
  };
};

(*
 * Variables
 *)
class Variable inherits Expr {
  name : String;

  init(n:String) : Variable {
    {
      name <- n;
      self;
    }
  };

  print_self() : SELF_TYPE {
    out_string(name)
  };

  beta() : Expr { self };
  
  substitute(x : Variable, e : Expr) : Expr {
    if x = self then e else self fi
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    let cur_env : VarList <- env in
      { while (if cur_env.isNil() then
	          false
	       else
	         not (cur_env.head() = self)
	       fi) loop
	  { out_string("get_parent().");
	    cur_env <- cur_env.tail();
          }
        pool;
        if cur_env.isNil() then
          { out_string("Error:  free occurrence of ");
            print_self();
            out_string("\n");
            abort();
            self;
          }
        else
          out_string("get_x()")
        fi;
      }
  };
};

(*
 * Functions
 *)
class Lambda inherits Expr {
  arg : Variable;
  body : Expr;

  init(a:Variable, b:Expr) : Lambda {
    {
      arg <- a;
      body <- b;
      self;
    }
  };

  print_self() : SELF_TYPE {
    {
      out_string("\\");
      arg.print_self();
      out_string(".");
      body.print_self();
      self;
    }
  };

  beta() : Expr { self };

  apply(actual : Expr) : Expr {
    body.substitute(arg, actual)
  };

  -- We allow variables to be reused
  substitute(x : Variable, e : Expr) : Expr {
    if x = arg then
      self
    else
      let new_body : Expr <- body.substitute(x, e),
	  new_lam : Lambda <- new Lambda in
	new_lam.init(arg, new_body)
    fi
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("((new Closure");
      out_int(closures.add(env, self));
      out_string(").init(");
      if env.isNil() then
        out_string("new Closure))")
      else
	out_string("self))") fi;
      self;
    }
  };

  gen_closure_code(n : Int, env : VarList,
		   closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("class Closure");
      out_int(n);
      out_string(" inherits Closure {\n");
      out_string("  apply(y : EvalObject) : EvalObject {\n");
      out_string("    { out_string(\"Applying closure ");
      out_int(n);
      out_string("\\n\");\n");
      out_string("      x <- y;\n");
      body.gen_code(env.add(arg), closures);
      out_string(";}};\n");
      out_string("};\n");
    }
  };
};

(*
 * Applications
 *)
class App inherits Expr {
  fun : Expr;
  arg : Expr;

  init(f : Expr, a : Expr) : App {
    {
      fun <- f;
      arg <- a;
      self;
    }
  };

  print_self() : SELF_TYPE {
    {
      out_string("((");
      fun.print_self();
      out_string(")@(");
      arg.print_self();
      out_string("))");
      self;
    }
  };

  beta() : Expr {
    case fun of
      l : Lambda => l.apply(arg);     -- Lazy evaluation
      e : Expr =>
	let new_fun : Expr <- fun.beta(),
	    new_app : App <- new App in
	  new_app.init(new_fun, arg);
    esac
  };

  substitute(x : Variable, e : Expr) : Expr {
    let new_fun : Expr <- fun.substitute(x, e),
        new_arg : Expr <- arg.substitute(x, e),
        new_app : App <- new App in
      new_app.init(new_fun, new_arg)
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("(let x : EvalObject <- ");
      fun.gen_code(env, closures);
      out_string(",\n");
      out_string("     y : EvalObject <- ");
      arg.gen_code(env, closures);
      out_string(" in\n");
      out_string("  case x of\n");
      out_string("    c : Closure => c.apply(y);\n");
      out_string("    o : Object => { abort(); new EvalObject; };\n");
      out_string("  esac)");
    }
  };
};

(*
 * Term: A class for building up terms
 *
 *)

class Term inherits IO {
  (*
   * The basics
   *)
  var(x : String) : Variable {
    let v : Variable <- new Variable in
      v.init(x)
  };

  lam(x : Variable, e : Expr) : Lambda {
    let l : Lambda <- new Lambda in
      l.init(x, e)
  };

  app(e1 : Expr, e2 : Expr) : App {
    let a : App <- new App in
      a.init(e1, e2)
  };

  (*
   * Some useful terms
   *)
  i() : Expr {
    let x : Variable <- var("x") in
      lam(x,x)
  };

  k() : Expr {
    let x : Variable <- var("x"),
        y : Variable <- var("y") in
    lam(x,lam(y,x))
  };

  s() : Expr {
    let x : Variable <- var("x"),
        y : Variable <- var("y"),
        z : Variable <- var("z") in
      lam(x,lam(y,lam(z,app(app(x,z),app(y,z)))))
  };

};

(*
 *
 * The main method -- build up some lambda terms and try things out
 *
 *)

class Main inherits Term {
  -- Beta-reduce an expression, printing out the term at each step
  beta_reduce(e : Expr) : Expr {
    {
      out_string("beta-reduce: ");
      e.print_self();
      let done : Bool <- false,
          new_expr : Expr in
        {
	  while (not done) loop
	    {
	      new_expr <- e.beta();
	      if (new_expr = e) then
		done <- true
	      else
		{
		  e <- new_expr;
		  out_string(" =>\n");
		  e.print_self();
		}
	      fi;
	    }
          pool;
	  out_string("\n");
          e;
	};
    }
  };

  eval_class() : SELF_TYPE {
    {
      out_string("class EvalObject inherits IO {\n");
      out_string("  eval() : EvalObject { { abort(); self; } };\n");
      out_string("};\n");
    }
  };

  closure_class() : SELF_TYPE {
    {
      out_string("class Closure inherits EvalObject {\n");
      out_string("  parent : Closure;\n");
      out_string("  x : EvalObject;\n");
      out_string("  get_parent() : Closure { parent };\n");
      out_string("  get_x() : EvalObject { x };\n");
      out_string("  init(p : Closure) : Closure {{ parent <- p; self; }};\n");
      out_string("  apply(y : EvalObject) : EvalObject { { abort(); self; } };\n");
      out_string("};\n");
    }
  };

  gen_code(e : Expr) : SELF_TYPE {
    let cl : LambdaListRef <- (new LambdaListRef).reset() in
      {
	out_string("Generating code for ");
	e.print_self();
	out_string("\n------------------cut here------------------\n");
	out_string("(*Generated by lam.cl (Jeff Foster, March 2000)*)\n");
	eval_class();
	closure_class();
	out_string("class Main {\n");
	out_string("  main() : EvalObject {\n");
	e.gen_code(new VarList, cl);
	out_string("\n};\n};\n");
	while (not (cl.isNil())) loop
	  let e : VarList <- cl.headE(),
	      c : Lambda <- cl.headC(),
	      n : Int <- cl.headN() in
	    {
	      cl.removeHead();
	      c.gen_closure_code(n, e, cl);
	    }
	pool;
	out_string("\n------------------cut here------------------\n");
      }
  };

  main() : Int {
    {
      i().print_self();
      out_string("\n");
      k().print_self();
      out_string("\n");
      s().print_self();
      out_string("\n");
      beta_reduce(app(app(app(s(), k()), i()), i()));
      beta_reduce(app(app(k(),i()),i()));
      gen_code(app(i(), i()));
      gen_code(app(app(app(s(), k()), i()), i()));
      gen_code(app(app(app(app(app(app(app(app(i(), k()), s()), s()),
                                   k()), s()), i()), k()), i()));
      gen_code(app(app(i(), app(k(), s())), app(k(), app(s(), s()))));
      0;
    }
  };
};
