class LinkedPacket;
  Packet packet_c;
  LinkedPacket next;
  function LinkedPacket get_next();
    get_next = next;
  endfunction
endclass
