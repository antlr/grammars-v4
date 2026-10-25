class Packet;
  integer i = 1;
  function integer get();
    get = i;
  endfunction
endclass
class LinkedPacket extends Packet;
  integer i = 2;
  function integer get();
    get = -i;
  endfunction
endclass
