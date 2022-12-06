task get_first(output int adr);
  fork
    wait_device(1, adr);
    wait_device(7, adr);
    wait_device(13, adr);
  join_any
  disable fork;
endtask
