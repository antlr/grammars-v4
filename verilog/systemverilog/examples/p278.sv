class Packet;
  rand int header;
  rand int len;
  rand byte payload[];
  int crc;
  constraint G {
    len > 1;
    payload.size == len;
  }
  function void post_randomize;
    crc = payload.sum;
  endfunction
endclass
