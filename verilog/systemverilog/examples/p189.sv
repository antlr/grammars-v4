class Packet;
  Packet next;
  function Packet get_next();
    get_next = next;
  endfunction
  extern protected virtual function int send(int value);
endclass
function int Packet::send(int value);
endfunction
