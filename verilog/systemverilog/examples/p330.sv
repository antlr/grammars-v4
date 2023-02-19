function automatic int crc(byte packet[1000:1]);
  for (int j = 1; j <= 1000; j++) begin
    crc ^= packet[j];
  end
endfunction
