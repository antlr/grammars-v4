class xyz;
  bit [3:0] m_x;
  int m_y;
  bit m_z;
  covergroup cov1 @m_z;
    coverpoint m_x;
    coverpoint m_y;
  endgroup
  function new();
    cov1 = new;
  endfunction
endclass
