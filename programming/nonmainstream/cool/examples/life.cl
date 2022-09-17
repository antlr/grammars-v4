(* The Game of Life 
   Tendo Kayiira, Summer '95
   With code taken from /private/cool/class/examples/cells.cl

 This introduction was taken off the internet. It gives a brief 
 description of the Game Of Life. It also gives the rules by which 
 this particular game follows.

	Introduction

   John Conway's Game of Life is a mathematical amusement, but it 
   is also much more: an insight into how a system of simple 
   cellualar automata can create complex, odd, and often aesthetically 
   pleasing patterns. It is played on a cartesian grid of cells
   which are either 'on' or 'off' The game gets it's name from the 
   similarity between the behaviour of these cells and the behaviour 
   of living organisms.

 The Rules

  The playfield is a cartesian grid of arbitrary size. Each cell in 
  this grid can be in an 'on' state or an 'off' state. On each 'turn' 
  (called a generation,) the state of each cell changes simultaneously 
  depending on it's state and the state of all cells adjacent to it.

   For 'on' cells, 
      If the cell has 0 or 1 neighbours which are 'on', the cell turns 
        'off'. ('dies of loneliness') 
      If the cell has 2 or 3 neighbours which are 'on', the cell stays 
        'on'. (nothing happens to that cell) 
      If the cell has 4, 5, 6, 7, 8, or 9 neighbours which are 'on', 
        the cell turns 'off'. ('dies of overcrowding') 

   For 'off' cells, 
      If the cell has 0, 1, 2, 4, 5, 6, 7, 8, or 9 neighbours which 
        are 'on', the cell stays 'off'. (nothing happens to that cell) 
      If the cell has 3 neighbours which are 'on', the cell turns 
        'on'. (3 neighbouring 'alive' cells 'give birth' to a fourth.) 

   Repeat for as many generations as desired. 

 *)
 

class Board inherits IO { 
 
 rows : Int;
 columns : Int;
 board_size : Int;

 size_of_board(initial : String) : Int {
   initial.length()
 };

 board_init(start : String) : SELF_TYPE {
   (let size :Int  <- size_of_board(start) in
    {
	if size = 15 then
	 {
	  rows <- 3;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 16 then
	  {
	  rows <- 4;
	  columns <- 4;
	  board_size <- size;
	 }
	else if size = 20 then
	 {
	  rows <- 4;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 21 then
	 {
	  rows <- 3;
	  columns <- 7;
	  board_size <- size;
	 }
	else if size = 25 then
	 {
	  rows <- 5;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 28 then
	 {
	  rows <- 7;
	  columns <- 4;
	  board_size <- size;
	 }
	else 	-- If none of the above fit, then just give 
	 {  -- the configuration of the most common board
	  rows <- 5;
	  columns <- 5;
	  board_size <- size;
	 }
	fi fi fi fi fi fi;
	self;
    }
   )
 };

};



class CellularAutomaton inherits Board {
    population_map : String;
   
    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
	    board_init(map);
            self;
        }
    };



   
    print() : SELF_TYPE {
        
	(let i : Int <- 0 in
	(let num : Int <- board_size in
	{
 	out_string("\n");
	 while i < num loop
           {
	    out_string(population_map.substr(i,columns));
	    out_string("\n"); 
	    i <- i + columns;
	   }
	 pool;
 	out_string("\n");
	self;
	}
	) ) 
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
	if board_size - 1 < position then
		" "
	else 
        	population_map.substr(position, 1)
	fi
    };
   
 north(position : Int): String {
	if (position - columns) < 0 then
	      " "	                       
	else
	   cell(position - columns)
	fi
 };

 south(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else
	   cell(position + columns)
	fi
 };

 east(position : Int): String {
	if (((position + 1) /columns ) * columns) = (position + 1) then
	      " "                
	else
	   cell(position + 1)
	fi 
 };

 west(position : Int): String {
	if position = 0 then
	      " "
	else 
	   if ((position / columns) * columns) = position then
	      " "
	   else
	      cell(position - 1)
	fi fi
 };

 northwest(position : Int): String {
	if (position - columns) < 0 then
	      " "	                       
	else  if ((position / columns) * columns) = position then
	      " "
	      else
		north(position - 1)
	fi fi
 };

 northeast(position : Int): String {
	if (position - columns) < 0 then
	      " "	
	else if (((position + 1) /columns ) * columns) = (position + 1) then
	      " "     
	     else
	       north(position + 1)
	fi fi
 };

 southeast(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else if (((position + 1) /columns ) * columns) = (position + 1) then
	       " "                
	     else
	       south(position + 1)
	fi fi
 };

 southwest(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else  if ((position / columns) * columns) = position then
	      " "
	      else
	       south(position - 1)
	fi fi
 };

 neighbors(position: Int): Int { 
 	{
	     if north(position) = "X" then 1 else 0 fi
	     + if south(position) = "X" then 1 else 0 fi
 	     + if east(position) = "X" then 1 else 0 fi
 	     + if west(position) = "X" then 1 else 0 fi
	     + if northeast(position) = "X" then 1 else 0 fi
	     + if northwest(position) = "X" then 1 else 0 fi
 	     + if southeast(position) = "X" then 1 else 0 fi
	     + if southwest(position) = "X" then 1 else 0 fi;
	 }
 };

 
(* A cell will live if 2 or 3 of it's neighbors are alive. It dies 
   otherwise. A cell is born if only 3 of it's neighbors are alive. *)
    
    cell_at_next_evolution(position : Int) : String {

	if neighbors(position) = 3 then
		"X"
	else
	   if neighbors(position) = 2 then
		if cell(position) = "X" then
			"X"
		else
			"-"
	        fi
	   else
		"-"
	fi fi
    };
  

    evolve() : SELF_TYPE {
        (let position : Int <- 0 in
        (let num : Int <- num_cells() in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };

(* This is where the background pattern is detremined by the user. More 
   patterns can be added as long as whoever adds keeps the board either
   3x5, 4x5, 5x5, 3x7, 7x4, 4x4 with the row first then column. *) 
 option(): String {
 {
  (let num : Int in
   {
   out_string("\nPlease chose a number:\n");
   out_string("\t1: A cross\n"); 
   out_string("\t2: A slash from the upper left to lower right\n");
   out_string("\t3: A slash from the upper right to lower left\n"); 
   out_string("\t4: An X\n"); 
   out_string("\t5: A greater than sign \n"); 
   out_string("\t6: A less than sign\n"); 
   out_string("\t7: Two greater than signs\n"); 
   out_string("\t8: Two less than signs\n"); 
   out_string("\t9: A 'V'\n"); 
   out_string("\t10: An inverse 'V'\n"); 
   out_string("\t11: Numbers 9 and 10 combined\n"); 
   out_string("\t12: A full grid\n"); 
   out_string("\t13: A 'T'\n");
   out_string("\t14: A plus '+'\n");
   out_string("\t15: A 'W'\n");
   out_string("\t16: An 'M'\n");
   out_string("\t17: An 'E'\n");
   out_string("\t18: A '3'\n");
   out_string("\t19: An 'O'\n");
   out_string("\t20: An '8'\n");
   out_string("\t21: An 'S'\n");
   out_string("Your choice => ");
   num <- in_int();
   out_string("\n");
   if num = 1 then
    	" XX  XXXX XXXX  XX  "
   else if num = 2 then
    	"    X   X   X   X   X    "
   else if num = 3 then
    	"X     X     X     X     X"
   else if num = 4 then
	"X   X X X   X   X X X   X"
   else if num = 5 then
	"X     X     X   X   X    "
   else if num = 6 then
	"    X   X   X     X     X"
   else if num = 7 then
	"X  X  X  XX  X      "
   else if num = 8 then
	" X  XX  X  X  X     "
   else if num = 9 then
	"X   X X X   X  "
   else if num = 10 then
	"  X   X X X   X"
   else if num = 11 then
	"X X X X X X X X"
   else if num = 12 then
	"XXXXXXXXXXXXXXXXXXXXXXXXX"
   else if num = 13 then
    	"XXXXX  X    X    X    X  "
   else if num = 14 then
    	"  X    X  XXXXX  X    X  "
   else if num = 15 then
    	"X     X X X X   X X  "
   else if num = 16 then
    	"  X X   X X X X     X"
   else if num = 17 then
	"XXXXX   X   XXXXX   X   XXXX"
   else if num = 18 then
	"XXX    X   X  X    X   XXXX "
   else if num = 19 then
	" XX X  XX  X XX "
   else if num = 20 then
	" XX X  XX  X XX X  XX  X XX "
   else if num = 21 then
	" XXXX   X    XX    X   XXXX "
   else
	"                         "
  fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi;
    }
   );
 }
 };




 prompt() : Bool { 
 {
  (let ans : String in
   {
   out_string("Would you like to continue with the next generation? \n");
   out_string("Please use lowercase y or n for your answer [y]: ");
   ans <- in_string();
   out_string("\n");
   if ans = "n" then 
	false
   else
	true
   fi;
   }
  );
 }
 };


 prompt2() : Bool { 
  (let ans : String in
   {
   out_string("\n\n");
   out_string("Would you like to choose a background pattern? \n");
   out_string("Please use lowercase y or n for your answer [n]: ");
   ans <- in_string();
   if ans = "y" then 
	true
   else
	false
   fi;
   }
  )
 };


};

class Main inherits CellularAutomaton {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
	 (let continue : Bool  in
	  (let choice : String  in
	   {
	   out_string("Welcome to the Game of Life.\n");
	   out_string("There are many initial states to choose from. \n");
	   while prompt2() loop
	    {
	     continue <- true;
	     choice <- option();
	     cells <- (new CellularAutomaton).init(choice);
	     cells.print();
             while continue loop
		if prompt() then
                    {
                        cells.evolve();
                        cells.print();
                    }
		else
		    continue <- false
	      fi 
                pool;
            }
            pool;
	    self;
      }  ) ); }
    };
};

