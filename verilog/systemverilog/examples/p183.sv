class Packet;
  local integer i;
  function integer compare(Packet other);
    compare = (this.i == other.i);
  endfunction
endclass
