class Packet;
  rand integer source_value;
  constraint filter1 {source_value > 2 * m;}
endclass
function integer toggle_rand(Packet p);
  if (p.filter1.constraint_mode()) p.filter1.constraint_mode(0);
  else p.filter1.constraint_mode(1);
  toggle_rand = p.randomize();
endfunction
class CA;
  rand byte x, y;
  byte v, w;
  constraint c1 {x < v && y > w;}
  ;
endclass
