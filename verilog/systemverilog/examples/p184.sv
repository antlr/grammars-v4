class Jumbo_Packet;
  const int max_size = 9 * 1024;
  byte payload[];
  function new(int size);
    payload = new[size > max_size ? max_size : size];
  endfunction
endclass
