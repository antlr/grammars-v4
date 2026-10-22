class Bus;
  rand bit [15:0] addr;
  rand bit [31:0] data;
  constraint word_align {addr[1:0] == 2'b0;}
endclass
