class packet;
  typedef struct {
    randc int addr = 1 + constant;
    int crc;
    rand byte data[] = {1, 2, 3, 4};
  } header;
  rand header h1;
endclass
